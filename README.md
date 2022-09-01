# `nix-serve-ng`

`nix-serve-ng` is a faster, more reliable, drop-in replacement for `nix-serve`.

## Quick start

There are two main approaches you can use to upgrade a NixOS system to replace
the old `nix-serve` with `nix-serve-ng`.

If you specify your desired NixOS system within `flake.nix` then you can do
something like this:

```nix
{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;

    nix-serve-ng.url = github:aristanetworks/nix-serve-ng;
  };

  outputs = { nixpkgs, nix-serve-ng, ... }: {
    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
      modules = [
        nix-serve-ng.nixosModules.default

        { services.nix-serve.enable = true;

          …
        }
      ];

      system = "x86_64-linux";
    };
  };
}
```

If you don't use `flake.nix` then you can instead define your NixOS module:
like this:

```nix
let
  nix-serve-ng-src = builtins.fetchTarball {
    # Replace the URL and hash with whatever you actually need
    url    = "https://github.com/aristanetworks/nix-serve-ng/archive/166672a78f20e4ec58b0a9748f4f04fc9f341ea3.tar.gz";

    sha256 = "0giib41mjb1j91an1qwpgh94bvrn841azdjv082kwd4kcjhms52h";
  };

  nix-serve-ng = import nix-serve-ng-src;

in
  { ... }: {
    imports = [ nix-serve-ng.nixosModules.default ];
  }
```

## Motivation

Our requirements for this project were:

* Improve reliability

  … since `nix-serve` would intermittently hang and require restarts

* Improve efficiency

  … since `nix-serve` was doing some obviously inefficient things which we
  felt we could improve upon

* Be backwards-compatible

  Our replacement would need to be a drop-in replacement for the original
  `nix-serve`, supporting the same command-line options and even sharing the
  same executable name

  The only exception is logging: we provide more detailed logging than before

Did we satisfy those requirements?

## Results

* Reliability

  We have test-driven this internally under heavy load with stable memory
  usage and without any failures but it's probably premature to declare victory.

  In particular, we have *not* done the following things:

  * Memory leak detection

    In other words, we haven't put our `nix-serve` through, say, `valgrind`

  * Exploit detection

    In other words, we haven't attempted to crash or sabotage the service with
    maliciously-crafted payload

* Performance

  We have improved significantly on efficiency, not only compared to `nix-serve`
  but also compared to other `nix-serve` rewrites.  We are more efficient than:

  * The original `nix-serve`

  * [`eris`](https://github.com/thoughtpolice/eris) - A Perl rewrite of
    `nix-serve`

  * [`harmonia`](https://github.com/helsinki-systems/harmonia) - A Rust rewrite
    of `nix-serve`

* Backwards-compatibility

  We have excellent backwards-compatibility.  In particular, in the vast
  majority of cases, you can simply replace `pkgs.nix-serve` with
  `pkgs.nix-serve-ng` and make no other changes.

  * Our executable shares the same name (`nix-serve`) as the original program

  * We support most the original command-line options

    The options that we're aware of that we do not currently support fall into
    two categories:

    * Useless options which are only relevant to `starman`:

      Upon request, we can still parse and ignore the following irrelevant
      options for extra backwards compatibility:

      * `--workers`

        We do not use worker subprocess like `starman` does.  Instead we use
        `warp` which internally uses Haskell green threads to service a much
        larger number of requests with less overhead and lower footprint when
        idle.

      * `--preload-app`

        This optimization is meaningless for a compiled Haskell executable.

      * `--disable-proctitle`

    * Useful options

      We might accept requests to support the following options, but we might
      explore other alternatives first before supporting them:

      * `--max-requests`

        `warp` itself is unlikely to be a bottleneck to servicing a large number
        of requests but there may still be Nix-specific or disk-specific
        reasons to cap the number of requests.

      * `--disable-keepalive`

      * `--keepalive-timeout`

      * `--read-timeout`

      * `--user`

      * `--group`

      * `--pid`

      * `--error-log`
