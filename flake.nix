{
  nixConfig = {
    extra-substituters = [ "https://stackage-infrastructure.cachix.org" ];
    extra-trusted-public-keys = [ "stackage-infrastructure.cachix.org-1:R3E1FYE8IKCNbUWCvVhsnlLJ4FC6onEQLhQX2kY0ufQ=" ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ self, ... }:
  let
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux"; overlays = [ self.overlays.this ];
    };
  in {

    overlays.this =
      let
        hsOverlay = pkgs: hself: hsuper: {
          curator = hself.callPackage ./nix/packages/curator.nix {};
          Cabal = hself.callPackage ./nix/packages/Cabal.nix {};
          Cabal-syntax = hself.callPackage ./nix/packages/Cabal-syntax.nix {};
          # pantry: jailbreak to allow Cabal 3.16 (has Cabal <3.15 bound),
          # and add pkg-config for configure
          pantry = pkgs.haskell.lib.compose.addPkgconfigDepend pkgs.zlib
            (pkgs.haskell.lib.doJailbreak hsuper.pantry);
        };
      in final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };
      };

    packages.x86_64-linux.default = self.packages.x86_64-linux.curator;
    packages.x86_64-linux.curator = pkgs.myHaskellPackages.curator;

    packages.x86_64-linux.gen-packages = pkgs.writeShellApplication {
      name = "gen-packages";
      text = builtins.readFile ./nix/scripts/gen-packages.sh;
      runtimeInputs = [
        pkgs.cabal-install
        pkgs.cabal2nix
      ];
    };

    packages.x86_64-linux.sync-lts = pkgs.writeShellApplication {
      name = "sync-lts";
      text = builtins.readFile ./nix/scripts/sync-lts.sh;
      runtimeInputs = [
        pkgs.coreutils
        pkgs.jq
        pkgs.gh
        pkgs.gnused
        pkgs.gnugrep
      ];
    };

    packages.x86_64-linux.update-cabal = pkgs.writeShellApplication {
      name = "update-cabal";
      text = builtins.readFile ./nix/scripts/update-cabal.sh;
      runtimeInputs = [
        pkgs.curl
        pkgs.jq
        pkgs.gnused
        pkgs.gnugrep
        pkgs.cabal2nix
      ];
    };

    devShells.x86_64-linux.stack = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.haskellPackages.ghc
        pkgs.zlib
        pkgs.pkg-config
      ];
    };
    devShells.x86_64-linux.default = pkgs.myHaskellPackages.shellFor {
      packages = hpkgs: [ hpkgs.curator ];
      nativeBuildInputs = [
        pkgs.cabal-install
        pkgs.haskell-language-server
      ];
    };
  };
}
