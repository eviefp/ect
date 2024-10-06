{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix }:
    let
      allSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
    in
    flake-utils.lib.eachSystem allSystems (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        treefmt-config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            fourmolu.package = pkgs.haskell.packages.ghc910.fourmolu;
          };
        };
        treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
      in
      {
        formatter = treefmt.wrapper;
        checks = {
          fmt = treefmt.check self;
          hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.haskell.packages.ghc910.hlint ]; } ''
            cd ${./.}
            hlint src test app
            touch $out
          '';
        };
        devShells = {
          default = pkgs.mkShell {
            name = "ect-shell";
            buildInputs = [
              pkgs.zlib
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskell.compiler.ghc910
              pkgs.haskell.packages.ghc910.cabal-install
              # pkgs.haskell.packages.ghc910.cabal2nix
              # pkgs.haskell.packages.ghc910.implicit-hie
              # pkgs.haskell.packages.ghc910.hoogle
              # pkgs.haskell.packages.ghc910.json-to-haskell
              pkgs.haskell.packages.ghc910.haskell-language-server
              pkgs.inotify-tools
            ];
          };
        };
        packages =
          let
            hlib = pkgs.haskell.lib;
            hp = pkgs.haskell.packages.ghc910.override {
              overrides = hself: hsuper: {
                async-timer = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.async-timer));
                multiwalk = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.multiwalk));
                scotty = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.scotty));
                iCalendar = hlib.doJailbreak (hself.callCabal2nix "iCalendar"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/centralapp/iCalendar.git";
                      ref = "master";
                      rev = "0858aa2ed64bc5357e943ab1e1327721e24b566d";
                    })
                  { });
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
            default = hp.callCabal2nix "etc" ./. { };
          };
      });
}
