{ pkgs, ... }:

let
  nix-serve-port = 5000;
in
pkgs.testers.nixosTest {
  name = "nix-serve-ng-basic";

  nodes.server = { pkgs, config, ... }: {
    services.nix-serve = {
      enable = true;
      package = pkgs.nix-serve-ng;
      port = nix-serve-port;
    };

    # hello is small and guarantees a known store path is present
    environment.systemPackages = [ pkgs.hello ];
  };

  testScript =
    let port = builtins.toString nix-serve-port;
    in ''
      server.wait_for_unit("nix-serve.service")
      server.wait_for_open_port(${port})

      # verify the cache info endpoint
      server.succeed(
        "curl -sf http://localhost:${port}/nix-cache-info | grep 'StoreDir: /nix/store'"
      )

      # hello's store path is known at eval time
      hash_part = "${pkgs.hello}".removeprefix("/nix/store/")[:32]

      # fetch the narinfo and verify it looks reasonable
      narinfo = server.succeed(f"curl -sf http://localhost:${port}/{hash_part}.narinfo")
      print(narinfo)
      assert "NarHash:" in narinfo, "narinfo missing NarHash"
      assert "NarSize:" in narinfo, "narinfo missing NarSize"

      # parse the nar url from the narinfo and download it
      nar_url = next(
        line.split(": ", 1)[1]
        for line in narinfo.splitlines()
        if line.startswith("URL: ")
      )
      nar_size = int(server.succeed(f"curl -sf http://localhost:${port}/{nar_url} | wc -c"))
      assert nar_size > 0, f"Expected non-empty NAR, got {nar_size} bytes"
    '';
}
