{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://flakular.in/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://flakular.in/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   relude.source = inputs.relude;
          # };
          # settings = {
          #   relude = {
          #     haddock = false;
          #     broken = false;
          #   };
          # };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: { fourmolu = hp.fourmolu; ghcid = null; treefmt = config.treefmt.build.wrapper; } // config.treefmt.build.programs;

            hlsCheck.enable = true;

            mkShellArgs = {
              buildInputs = with pkgs; [
                gnumake
              ];
            };
          };

        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          # This is the default, and can be overriden.
          package = pkgs.treefmt;
          # formats .hs files (fourmolu is also available)
          programs.ormolu.enable = true;
          # formats .nix files
          programs.nixpkgs-fmt.enable = true;
          # formats .cabal files
          programs.cabal-fmt.enable = false;
          # Suggests improvements for your code in .hs files
          programs.hlint.enable = false;
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
