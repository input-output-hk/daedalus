# Development docs

## How to update the website

To update https://input-output-hk.github.io/cardano-launcher/, run the
[update-gh-pages](./scripts/update-gh-pages.sh) script:

    ./scripts/update-gh-pages.sh
    git push origin gh-pages

## How to update the cardano-wallet and cardano-node versions

Use the `niv` tool. To get it, run `nix-shell`. Then:

    niv update cardano-wallet
    niv update cardano-node

This will use the branches configuration in
[`nix/sources.json`](../nix/sources.json). Niv also provides options
for choosing another branch or git rev.

## cardano-node configurations for tests

Before running tests, ensure that you have the `BYRON_CONFIGS`
environment pointing to the `configuration` subdirectory of the
`cardano-node` repo. If running in a `nix-shell`, this is set
automatically.
