{
  inputs = {
    flake-utils.url   = "github:numtide/flake-utils";
    nixpkgs.url       = "github:NixOS/nixpkgs/nixos-unstable";
    crem.url          = "github:marcosh/crem";
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, crem, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages =
            let
              watchWithGhcid = pkgs.writers.writeDashBin "watch" ''
                ${pkgs.ghcid}/bin/ghcid --command="cabal repl"
              '';
              # Wrap cabal to always run `hpack` first.
              cabalWrapped = pkgs.writers.writeDashBin "cabal" ''
                ${pkgs.hpack}/bin/hpack >/dev/null 2>&1
                ${pkgs.cabal-install}/bin/cabal "$@"
              '';
              hask = pkgs.haskell.packages.ghc96.override {
                overrides = self: super: {
                  # elm-street =
                  # let src = pkgs.fetchgit {
                  #   url    = "https://github.com/Holmusk/elm-street";
                  #   rev    = "d104c24bd328144057641b684330041bbfcfc9fe";
                  #   sha256 = "sha256-SV3yR//fmAqhxN0c60DTvtyqYsFuHVLbLlqM/CQ+STY=";
                  #   };
                  # in self.callCabal2nix "elm-street" src {};
                };
              };
            in with pkgs; [
              # For JS version
              ghc-wasm-meta.packages.${system}.all_9_6

              cabalWrapped
              ghcid
              hpack
              nodejs
              stylish-haskell
              watchWithGhcid

              (hask.ghcWithPackages (ps: with ps; [
                aeson
                bytestring
                crem
                elm-street
                extra
                hspec
                http-types
                profunctors
                servant
                servant-server
                servant-websockets
                singletons-base
                text
                wai
                warp
                websockets
                QuickCheck

                # For css
                clay
              ]))
            ];
          };
        };
      }
    );
}


