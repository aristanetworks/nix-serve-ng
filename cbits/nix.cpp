#include <cstddef>
#include <cstdlib>

#ifndef LIX
    #include <nix/store/store-api.hh>
    #include <nix/store/log-store.hh>
    #include <nix/main/shared.hh>
#else
    #include <lix/libstore/store-api.hh>
    #include <lix/libstore/log-store.hh>
    #include <lix/libmain/shared.hh>
#endif

#include "nix.hh"

using namespace nix;

// Copied from:
//
// https://github.com/NixOS/nix/blob/2.8.1/perl/lib/Nix/Store.xs#L24-L37
static ref<Store> getStore()
{
    static std::shared_ptr<Store> _store;

    if (!_store) {
#ifndef LIX
        initLibStore(true);
#else
        initNix();
#endif

        _store = openStore();
    }

    return ref<Store>(_store);
}

extern "C" {

// Must be called once before the server is stated to avoid races
void initStore()
{
    getStore();
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

void queryPathFromHashPart
    ( char const * const hashPart
    , struct string * const output
    )
{
    ref<Store> store = getStore();

    std::optional<StorePath> path = store->queryPathFromHashPart(hashPart);

    if (path.has_value()) {
        copyString(store->printStorePath(path.value()), output);
    } else {
        *output = emptyString;
    }
}

void queryPathInfo
    ( char const * const storePath
    , PathInfo * const output
    )
{
    ref<Store> store = getStore();

    ref<ValidPathInfo const> const validPathInfo =
        store->queryPathInfo(store->parseStorePath(storePath));

    std::optional<StorePath const> const deriver = validPathInfo->deriver;

    if (deriver.has_value()) {
        copyString(store->printStorePath(deriver.value()), &output->deriver);
    } else {
        output->deriver = emptyString;
    };

#ifndef LIX
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
}

void freePathInfo(struct PathInfo * const input)
{
    freeString(&input->deriver);
    freeString(&input->narHash);
    freeStrings(&input->references);
    freeStrings(&input->sigs);
}

// TODO: This can be done in Haskell using the `ed25519` package
void signString
    ( char const * const secretKey
    , char const * const message
    , struct string * const output
    )
{
    std::string signature = SecretKey(secretKey).signDetached(message);

    copyString(signature, output);
}

bool dumpPath
    ( char const * const hashPart
    , bool (* const callback)(char const * const data, size_t const size)
    )
{
    ref<Store> store = getStore();

    std::optional<StorePath> storePath =
        store->queryPathFromHashPart(hashPart);

    if (storePath.has_value()) {
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
#ifndef LIX
            store->narFromPath(storePath.value(), sink);
#else
            sink << store->narFromPath(storePath.value());
#endif
        } catch (const std::runtime_error & e) {
            // Intentionally do nothing.  We're only using the exception as a
            // short-circuiting mechanism.
        }

        return true;
    } else {
        return false;
    }
}

void dumpLog(char const * const baseName, struct string * const output) {
    ref<Store> store = getStore();

    StorePath storePath(baseName);

    auto subs = getDefaultSubstituters();

    subs.push_front(store);

    *output = emptyString;

    for (auto & sub : subs) {
        LogStore * logStore = dynamic_cast<LogStore *>(&*sub);

        std::optional<std::string> log = logStore->getBuildLog(storePath);

        if (log.has_value()) {
            copyString(log.value(), output);
        }
    }
}

}
