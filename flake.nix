{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
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
          hp = pkgs.haskell.packages.ghc963.override {
            overrides = hself: hsuper: {
              org-parser = hlib.doJailbreak (hlib.markUnbroken hsuper.org-parser);
              async-timer = hlib.dontCheck (hlib.doJailbreak (hlib.markUnbroken hsuper.async-timer));
              iCalendar = hself.callCabal2nix "iCalendar" (
                builtins.fetchGit {
                  url = "https://github.com/centralapp/iCalendar.git";
                  ref = "master";
                  rev = "0858aa2ed64bc5357e943ab1e1327721e24b566d";
                }) {};

              vty = hself.callCabal2nix "vty" (
                builtins.fetchGit {
                  url = "https://github.com/jtdaugherty/vty.git";
                  rev = "2f9eb83654f9942a4ec54d9d2335a941fa66e272";
                }) {};
              vty-unix = hlib.doJailbreak (hlib.markUnbroken hsuper.vty-unix);
              brick = hself.callCabal2nix "brick" (
                builtins.fetchGit {
                  url = "https://github.com/jtdaugherty/brick.git";
                  rev = "fc6f5eed07829d3e3be6717097c0249b1f2a0c04";
                }) {};
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
            pkgs.haskell.compiler.ghc963
            pkgs.haskell.packages.ghc963.cabal-install
            pkgs.haskell.packages.ghc963.cabal2nix
            # pkgs.haskell.packages.ghc981.implicit-hie
            # pkgs.haskell.packages.ghc981.hoogle
            # pkgs.haskell.packages.ghc981.json-to-haskell
            pkgs.haskell.packages.ghc963.haskell-language-server
          ];
        };
      });
    };
}
