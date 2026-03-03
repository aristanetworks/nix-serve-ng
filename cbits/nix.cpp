#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <map>
#include <mutex>
#include <stdexcept>

#ifndef LIX
    #define CPPNIX 1
#else
    #define CPPNIX 0
    #if __has_include (<lix/libutil/async.hh>)
        #define LIX_POST_2_93 1
        #define LIX_PRE_2_93  0
    #else
        #define LIX_POST_2_93 0
        #define LIX_PRE_2_93  1
    #endif
#endif

#if CPPNIX
    #include <nix/main/shared.hh>
    #include <nix/store/filetransfer.hh>
    #include <nix/store/local-store.hh>
    #include <nix/store/log-store.hh>
    #include <nix/store/store-api.hh>
#else
    #include <lix/config.h>
    #include <lix/libmain/shared.hh>
    #include <lix/libstore/filetransfer.hh>
    #include <lix/libstore/local-store.hh>
    #include <lix/libstore/log-store.hh>
    #include <lix/libstore/store-api.hh>
    #if LIX_POST_2_93
        #include <lix/libutil/async.hh>
    #endif
#endif

#if CPPNIX || LIX_PRE_2_93
    #define BLOCKON(X) X
#else
    #define BLOCKON(X) aio().blockOn(X)
#endif

#include "nix.hh"

using namespace nix;

#if LIX_POST_2_93
static AsyncIoRoot & aio()
{
    static thread_local AsyncIoRoot root;
    return root;
}
#endif

#if CPPNIX || LIX_PRE_2_93

typedef std::shared_ptr<Store> StorePtr;
#define OPEN_STORE(X) X == "" ? openStore() : openStore(X)
#define STORE_REF(X) ref<Store>(X)

std::mutex _stores_mutex;
static std::map<std::string, StorePtr> _stores;

static void insertStore(std::string url, StorePtr store) {
    std::lock_guard<std::mutex> guard(_stores_mutex);
    _stores.insert({url,store});
}

static StorePtr lookupStore(std::string url) {
    std::lock_guard<std::mutex> guard(_stores_mutex);
    return _stores.at(url);
}

#else

typedef std::optional<ref<Store>> StorePtr;
#define OPEN_STORE(X) aio().blockOn(X == "" ? openStore() : openStore(X))
#define STORE_REF(X) *X

static Sync<std::map<std::string, StorePtr>> _stores;

static void insertStore(std::string url, StorePtr store) {
    auto _stores_(_stores.lock());
    (*_stores_).insert({url,store});
}

static StorePtr lookupStore(std::string url) {
    auto _stores_(_stores.lock());
    return (*_stores_).at(url);
}

#endif

// Based on: https://github.com/NixOS/nix/blob/2.8.1/perl/lib/Nix/Store.xs#L24-L37
// with allowance for multiple stores
static ref<Store> getStore(std::string const url, bool cached = true)
{
    StorePtr store;

    if (!cached) {
        store = OPEN_STORE(url);
        insertStore(url, store);
    } else {
        // We know this exists from the Haskell side
        store = lookupStore(url);
    }

    return STORE_REF(store);
}

extern "C" {

// Must be called once before the server is stated to avoid races
ffi_return_codes_t initStore(char const * const url)
{
    static bool _initDone = false;

    try {
        if (!_initDone) {
            #if CPPNIX
            initLibStore(true);
            #else
            initNix();
            #endif
            _initDone = true;
        }
    } catch(const std::exception& e) {
        std::cout << "Fatal exception in initializing nix: " << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }

    try {
        getStore(url, false);
    } catch(const std::exception& e) {
        std::cout << "Exception in 'initStore': " << e.what() << std::endl;
        return RETURN_FAIL;
    }
    return RETURN_OK;
}

void freeString(struct string * const input)
{
    free((void *) input->data);
}

}

// TODO: Perhaps use convention where destination is first argument
void copyString(std::string const input, struct string * const output)
{
    size_t const size = input.size();

    char * data = (char *) calloc(size, sizeof(char));

    input.copy(data, size);

    output->size = size;

    output->data = data;
}

static const struct string emptyString = { .data = NULL, .size = 0 };

extern "C" {

void freeStrings(struct strings * const input)
{
    size_t size = input->size;

    for (size_t i = 0; i < size; i++) {
        freeString(&input->data[i]);
    }

    free(input->data);
}

}

void copyStrings
    ( std::vector<std::string> input
    , struct strings * const output
    )
{
    size_t const size = input.size();

    struct string * data = (struct string *) calloc(input.size(), sizeof(struct string));

    for (size_t i = 0; i < size; i++) {
        copyString(input[i], &data[i]);
    }

    output->data = data;

    output->size = size;
}

extern "C" {

void getStoreDir(struct string * const output)
{
    copyString(settings.nixStore, output);
}

unsigned long getMaxConnectTimeout()
{
    #if CPPNIX
    return fileTransferSettings.connectTimeout;
    #elif ! defined(LIX_MAJOR) || LIX_MAJOR <= 2 && LIX_MINOR < 94
    return fileTransferSettings.connectTimeout.get();
    #else
    return fileTransferSettings.maxConnectTimeout.get();
    #endif
}

ffi_return_codes_t queryPathFromHashPart
    ( char const * const url
    , char const * const hashPart
    , struct string * const output
    )
{
    try {
        ref<Store> store = getStore(url);

        std::optional<StorePath> path = BLOCKON(store->queryPathFromHashPart(hashPart));

        if (path.has_value()) {
            copyString(store->printStorePath(path.value()), output);
        } else {
            *output = emptyString;
        }

    } catch(const std::exception& e) {
        std::cout << "Exception in 'queryPathFromHashPart': " << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }
    return RETURN_OK;
}

ffi_return_codes_t queryPathInfo
    ( char const * const url
    , char const * const storePath
    , PathInfo * const output
    )
{
    try {
        ref<Store> store = getStore(url);

        ref<ValidPathInfo const> const validPathInfo =
            BLOCKON(store->queryPathInfo(store->parseStorePath(storePath)));

        std::optional<StorePath const> const deriver = validPathInfo->deriver;

        if (deriver.has_value()) {
            copyString(store->printStorePath(deriver.value()), &output->deriver);
        } else {
            output->deriver = emptyString;
        };

        #if CPPNIX
        copyString(validPathInfo->narHash.to_string(nix::HashFormat::Nix32, true), &output->narHash);
        #else
        copyString(validPathInfo->narHash.to_string(nix::Base::Base32, true), &output->narHash);
        #endif

        output->narSize = validPathInfo->narSize;

        std::vector<std::string> references(validPathInfo->references.size());

        std::transform(
            validPathInfo->references.begin(),
            validPathInfo->references.end(),
            references.begin(),
            [=](StorePath storePath) { return store->printStorePath(storePath); }
        );

        copyStrings(references, &output->references);

        std::vector<std::string> sigs(validPathInfo->sigs.begin(), validPathInfo->sigs.end());

        copyStrings(sigs, &output->sigs);

    } catch(const std::exception& e) {
        std::cout << "Exception in 'queryPathInfo': " << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }
    return RETURN_OK;
}

void freePathInfo(struct PathInfo * const input)
{
    freeString(&input->deriver);
    freeString(&input->narHash);
    freeStrings(&input->references);
    freeStrings(&input->sigs);
}

// TODO: This can be done in Haskell using the `ed25519` package
ffi_return_codes_t signString
    ( char const * const secretKey
    , char const * const message
    , struct string * const output
    )
{
    try {
        #if ! defined(LIX_MAJOR) || LIX_MAJOR <= 2 && LIX_MINOR < 94
        std::string signature = SecretKey(secretKey).signDetached(message);
        #else
        std::string signature = SecretKey::parse(secretKey).signDetached(message);
        #endif

        copyString(signature, output);

    } catch(const std::exception& e) {
        std::cout << "Exception in 'signStrings': " << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }
    return RETURN_OK;
}

ffi_return_codes_t dumpPath
    ( char const * const url
    , char const * const storePathStr
    , bool (* const callback)(char const * const data, size_t const size)
    )
{
    try {
        ref<Store> store = getStore(url);

        auto storePath = store->parseStorePath(storePathStr);

        LambdaSink sink([=](std::string_view v) {
            bool succeeded = (*callback)(v.data(), v.size());

            if (!succeeded) {
                // We don't really care about the error message.  The only
                // reason for throwing an exception here is that this is the
                // only way that a Nix sink can exit early.
                throw std::runtime_error("");
            }
        });

        try {
            #if CPPNIX
            store->narFromPath(storePath, sink);
            #elif LIX_PRE_2_93
            sink << store->narFromPath(storePath);
            #elif ! defined(LIX_MAJOR) || LIX_MAJOR <= 2 && LIX_MINOR < 94
            aio().blockOn(store->narFromPath(storePath))->drainInto(sink);
            #else
            aio().blockOn(aio().blockOn(store->narFromPath(storePath))->drainInto(sink));
            #endif
        } catch (const std::runtime_error & e) {
            // Intentionally do nothing.  We're only using the exception as a
            // short-circuiting mechanism.
        }
    } catch(const std::exception& e) {
        std::cout << "Exception in 'dumpPath': " << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }
    return RETURN_OK;
}

ffi_return_codes_t dumpLog
    ( char const * const url
    , char const * const baseName
    , struct string * const output
    )
{
    try {
        ref<Store> store = getStore(url);

        StorePath storePath(baseName);

        auto subs = dynamic_cast<LocalStore *>(&*store)
                  ? BLOCKON(getDefaultSubstituters())
                  : std::list<ref<Store>>();

        subs.push_front(store);

        *output = emptyString;

        for (auto & sub : subs) {
            LogStore * logStore = dynamic_cast<LogStore *>(&*sub);

            if (!logStore) continue;

            std::optional<std::string> log = BLOCKON(logStore->getBuildLog(storePath));

            if (log.has_value()) {
                copyString(log.value(), output);
            }
        }

        return RETURN_OK;
    } catch(const std::exception& e) {
        std::cout << "Exception in 'dumpLog': '" << e.what() << std::endl;
        return RETURN_EXCEPTION;
    }
}

}
