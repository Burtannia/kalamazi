{
  description = "kalamazi";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              kalamazi = hfinal.callCabal2nix "kalamazi" ./. { };
            };
        };
        kalamazi = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.kalamazi;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
          {
          devShells = rec {
            default = kalamazi-shell;
            kalamazi-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.kalamazi ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                hspkgs.yesod-bin
                pkgs.bashInteractive
              ];
              shellHook = ''
                exec zsh
              '';
            };
          };
          packages = rec {
            default = kalamazi;
            kalamazi = pkgs.kalamazi;
          };
        };
    in
      { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
