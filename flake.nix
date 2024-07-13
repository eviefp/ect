{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      allSystems = [
        "x86_64-linux" # 64bit AMD/Intel x86
      ];

      forAllSystems = fn:
        nixpkgs.lib.genAttrs allSystems
          (system: fn { pkgs = import nixpkgs { inherit system; }; });
    in
    {
      packages = forAllSystems ({ pkgs }:
        let
          hlib = pkgs.haskell.lib;
          hp = pkgs.haskell.packages.ghc965.override {
            overrides = hself: hsuper: {
              async-timer = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.async-timer));
              iCalendar = hself.callCabal2nix "iCalendar"
                (
                  builtins.fetchGit {
                    url = "https://github.com/centralapp/iCalendar.git";
                    ref = "master";
                    rev = "0858aa2ed64bc5357e943ab1e1327721e24b566d";
                  })
                { };
              org-parser = hlib.doJailbreak (hself.callCabal2nix "org-parser"
                ((
                  builtins.fetchGit {
                    url = "https://github.com/eviefp/org-mode-hs.git";
                    ref = "main";
                    rev = "31416b5b4e0ed9f5fff60de9c4e854a2e9bef493";
                  }) + "/org-parser")
                { });
              vty = hself.callCabal2nix "vty"
                (
                  builtins.fetchGit {
                    url = "https://github.com/jtdaugherty/vty.git";
                    rev = "2f9eb83654f9942a4ec54d9d2335a941fa66e272";
                  })
                { };
              vty-unix = hlib.doJailbreak (hlib.markUnbroken hsuper.vty-unix);
              brick = hself.callCabal2nix "brick"
                (
                  builtins.fetchGit {
                    url = "https://github.com/jtdaugherty/brick.git";
                    rev = "fc6f5eed07829d3e3be6717097c0249b1f2a0c04";
                  })
                { };
            };
          };
        in
        {
          default = hp.callCabal2nix "ect" ./. { };
        });
      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.mkShell {
          name = "ect-shell";
          nativeBuildInputs = [
            pkgs.zlib.dev
            pkgs.haskell.compiler.ghc965
            pkgs.haskell.packages.ghc965.cabal-install
            pkgs.haskell.packages.ghc965.cabal2nix
            pkgs.haskell.packages.ghc965.hoogle
            pkgs.haskell.packages.ghc965.haskell-language-server
            pkgs.inotify-tools
          ];
        };
      });
    };
}
