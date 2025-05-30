{
  description = "atomic-css overlay, development and examples";

  nixConfig = {
    extra-substituters = [
      "https://hyperbole.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hyperbole.cachix.org-1:9Pl9dJXuJrAxGkrG8WNQ/hlO9rKt9b5IPksG7y78UGQ="
    ];
  };

  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter/main";
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-filter,
      flake-utils,
      pre-commit-hooks,
    }:
    let
      packageName = "atomic-css";
      examplesName = "example";
      src = nix-filter.lib {
        root = ./.;
        include = [
          (nix-filter.lib.inDirectory "src")
          (nix-filter.lib.inDirectory "embed")
          (nix-filter.lib.inDirectory "test")
          ./README.md
          ./CHANGELOG.md
          ./LICENSE
          ./${packageName}.cabal
          ./cabal.project
          ./package.yaml
          ./fourmolu.yaml
        ];
      };

      overlay = final: prev: {
        overriddenHaskellPackages = {
          ghc982 = (prev.overriddenHaskellPackages.ghc982 or prev.haskell.packages.ghc982).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
                skeletest = hprev.skeletest.overrideAttrs (old: {
                  meta = old.meta // {
                    broken = false;
                  };
                });
                Diff = hfinal.callHackage "Diff" "0.5" { };
              }
            );
          });
          ghc966 = (prev.overriddenHaskellPackages.ghc966 or prev.haskell.packages.ghc966).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
                attoparsec-aeson = hfinal.callHackage "attoparsec-aeson" "2.2.0.0" { };
                skeletest = hprev.skeletest.overrideAttrs (old: {
                  meta = old.meta // {
                    broken = false;
                  };
                });
                Diff = hfinal.callHackage "Diff" "0.5" { };
                aeson = hfinal.callHackage "aeson" "2.2.2.0" { };
              }
            );
          });
        };
      };
    in
    {
      overlays.default = overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            (nix-filter.lib.inDirectory "app")
            ./example/${examplesName}.cabal
            ./example/cabal.project
            ./example/LICENSE
          ];
        };

        ghcVersions = [
          "966"
          "982"
        ];

        ghcPkgs = builtins.listToAttrs (
          map (ghcVer: {
            name = "ghc${ghcVer}";
            value = (
              pkgs.overriddenHaskellPackages."ghc${ghcVer}".extend (
                hfinal: hprev: {
                  ${examplesName} = hfinal.callCabal2nix examplesName example-src { };
                }
              )
            );
          }) ghcVersions
        );

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = src;
          hooks = {
            hlint.enable = true;
            fourmolu.enable = true;
            hpack.enable = true;
            nixfmt-rfc-style.enable = true;
            flake-checker = {
              enable = true;
              args = [ "--no-telemetry" ];
            };
            check-merge-conflicts.enable = true;
          };
        };

        shellCommon = version: {
          inherit (pre-commit) shellHook;
          buildInputs = with pkgs.haskell.packages."ghc${version}"; [
            cabal-install
            haskell-language-server
            fast-tags
            ghcid
            fourmolu
            pkgs.hpack
          ];
          withHoogle = true;
          doBenchmark = true;
          CABAL_CONFIG = "/dev/null";
        };

        exe =
          version:
          pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-${examplesName}")
            (drv: {
              # Added due to an issue building on macOS only
              postInstall = ''
                ${drv.postInstall or ""}
                  echo "Contents of $out/bin:"
                  ls -la $out/bin
                  echo remove-references-to -t ${ghcPkgs."ghc${version}".warp}
                  remove-references-to -t ${ghcPkgs."ghc${version}".warp} $out/bin/*
              '';
            });
      in
      {
        checks = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-check-${examplesName}";
            value = pkgs.runCommand "ghc${version}-check-example" {
              buildInputs = [
                (exe version)
              ] ++ self.devShells.${system}."ghc${version}-shell".buildInputs;
            } "type example; CABAL_CONFIG=/dev/null cabal --dry-run repl; touch $out";
          }) ghcVersions
        );

        apps =
          {
            default = self.apps.${system}."ghc966-${examplesName}";
          }
          // builtins.listToAttrs (
            map (version: {
              name = "ghc${version}-${examplesName}";
              value = {
                type = "app";
                program = "${exe version}/bin/example";
              };
            }) ghcVersions
          );

        packages =
          {
            default = self.packages.${system}."ghc982-${packageName}";
          }
          // builtins.listToAttrs (
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${examplesName}";
                value = ghcPkgs."ghc${version}".${examplesName};
              }
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".${packageName};
              }
            ]) ghcVersions
          );

        devShells =
          {
            default = self.devShells.${system}.ghc982-shell;
          }
          // builtins.listToAttrs (
            map (version: {
              name = "ghc${version}-shell";
              value = ghcPkgs."ghc${version}".shellFor (
                shellCommon version
                // {
                  packages = p: [
                    p.${packageName}
                    p.${examplesName}
                  ];
                }
              );
            }) ghcVersions
          );
      }
    );
}
