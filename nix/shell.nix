{ repoRoot, inputs, pkgs, lib, system }:

_cabalProject:

let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
  cardano-submit-api = inputs.cardano-node.legacyPackages.cardano-submit-api;
in
{
  name = "plutus-tools";
  packages = [
    cardano-cli
    cardano-node
    cardano-submit-api
    inputs.mithril.packages.mithril-client
    pkgs.ghcid
    pkgs.haskellPackages.hoogle
    pkgs.cowsay
    pkgs.watchexec
    pkgs.eza
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    CARDANO_SUBMIT_API = "${cardano-submit-api}/bin/cardano-submit-api";
  };
  shellHook = ''
    alias ls=eza
    alias find=fd
    echo "make plutus-tools great again" | cowsay
    set -o vi
  '';


  preCommit = {
    # NOTE: when this attribute set changes, `.pre-commit-config.yaml` (which is a sym link to the nix store) changes.
    #       To maintain a the same hooks for both nix and non-nix environment you should update the `.pre-commit-config.yaml.nonix`
    #       (`cp .pre-commit-config.yaml .pre-commit-config.yaml.nonix`).
    #       This step is necessary because `.pre-commit-config.yaml` is ignored by git.
    cabal-fmt.enable = true;
    stylish-haskell.enable = false;
    fourmolu.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
