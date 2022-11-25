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
    url    = "https://github.com/aristanetworks/nix-serve-ng/archive/1937593598bb1285b41804f25cd6f9ddd4d5f1cb.tar.gz";

    sha256 = "1lqd207gbx1wjbhky33d2r8xi6avfbx4v0kpsvn84zaanifdgz2g";
  };

  nix-serve-ng = import nix-serve-ng-src;

in
  { ... }: {
    imports = [ nix-serve-ng.nixosModules.default ];

    …
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

  See the Benchmarks section below for more details

* Backwards-compatibility

  We have excellent backwards-compatibility, so in the vast majority of cases,
  you can simply replace `pkgs.nix-serve` with `pkgs.nix-serve-ng` and make no
  other changes.

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

  Because of this backwards-compatibility you only need to replace the old
  `nix-serve` executable with the `nix-serve` executable built by this package
  (which is what the included NixOS module does).

  You don't need to define or use any new NixOS options.  You continue to use
  the old `services.nix-serve` options hierarchy to configure the upgraded
  service.

## Benchmarks

The test environment is a large server machine:

* CPU: 24 × Intel(R) Xeon(R) CPU E5-2680 v3 @ 2.50GHz
* RAM: 384 GB (24 × 16 GB @ 2133 MT/s)
* Disk (`/nix/store`): ≈4 TB SSD

Legend:

* **Fetch present NAR info ×10**: Time to fetch the NAR info for 10 different files that are present
* **Fetch absent NAR info ×1**: Time to fetch the NAR info a single file that is absent
* **Fetch empty NAR ×10**: Time to fetch the NAR for the same empty file 10 times
* **Fetch 10 MB NAR ×10**: Time to fetch the NAR for the same 10 MB file 10 times

Raw numbers:

| Benchmark                  | `nix-serve`      | `eris`           | `harmonia`       | `nix-serve-ng`   |
|----------------------------|------------------|------------------|------------------|------------------|
| Fetch present NAR info ×10 | 2.09 ms ± 66  μs | 41.5 ms ± 426 μs | 1.57 ms ±  91 μs | 1.32 ms ±  33 μs |
| Fetch absent NAR info ×1   | 212  μs ± 18  μs | 3.42 ms ± 113 μs | 139  μs ±  11 μs | 115  μs ± 6.2 μs |
| Fetch empty NAR ×10        | 164  ms ± 8.5 ms | 246  ms ±  20 ms | 279  ms ±  10 ms | 5.16 ms ± 368 μs |
| Fetch 10 MB NAR ×10        | 291  ms ± 8.7 ms | 453  ms ±  19 ms | 487  ms ±  41 ms | 86.9 ms ± 3.0 ms |

Speedups (compared to `nix-serve`):

| Benchmark                  | `nix-serve`      | `eris`           | `harmonia`       | `nix-serve-ng`   |
|----------------------------|------------------|------------------|------------------|------------------|
| Fetch present NAR info ×10 | 1.0              | 0.05             | 1.33             |  1.58            |
| Fetch absent NAR info ×1   | 1.0              | 0.06             | 1.53             |  1.84            |
| Fetch empty NAR ×10        | 1.0              | 0.67             | 0.59             | 31.80            |
| Fetch 10 MB NAR ×10        | 1.0              | 0.64             | 0.60             |  3.35            |

We can summarize `nix-serve-ng`'s performance like this:

* Time to handle a NAR info request: ≈ 100 μs 
* Time to serve a NAR: ≈ 500 μs + 800 μs / MB

You can reproduce these benchmarks using the benchmark suite.  See the
instructions in [`./benchmark/Main.hs`](./benchmark/Main.hs) for running your
own benchmarks.

Caveats:

* We haven't used any of these services' tuning options, including:
  * Tuning garbage collection (for `nix-serve-ng`)
  * Tuning concurrency/parallelism/workers
* We haven't benchmarked memory utilization
