# cardano-launcher Shelley

[![Build status](https://badge.buildkite.com/98083d5651511146dab7911b99f20ff9b60b4f8be25298a82f.svg)](https://buildkite.com/input-output-hk/cardano-launcher)

`cardano-launcher` is a Node.js module for starting
[cardano-wallet](https://github.com/input-output-hk/cardano-wallet)
and the Shelley
[cardano-node](https://github.com/input-output-hk/cardano-node).

Its primary user is
[Daedalus](https://github.com/input-output-hk/daedalus); however it
could be used by any Javascript application.

## Documentation

See the generated API docs at https://input-output-hk.github.io/cardano-launcher/modules/_cardanolauncher_.html.

See [docs/example.js](./docs/example.js)
and [docs/example-jormungandr.js](./docs/example-jormungandr.js)
for example code.

## Component diagram

![Component diagram](./docs/component-diagram.png)

## Message sequence diagram - launch

![Component diagram](./docs/launch.png)


## Local Development

### `npm start`

Runs the project in development/watch mode.

### `npm run build`

Bundles the package to the `dist` folder.

### `npm test`

Runs the test watcher ([Jest](https://jestjs.io/docs/en/getting-started))
in an interactive mode.
By default, runs tests related to files changed since the last commit.

Alternatively, to run only unit tests:

    npx tsdx test unit

See the [Jest command-line reference](https://jestjs.io/docs/en/cli)
for all the options.

### `npm run typedoc`

Generates API documentation to the `site` folder.

### `nix-shell`

Runs a bash shell with project development dependencies (e.g. `npm`,
`cardano-wallet`) in the `PATH`.

See [nix.md](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md)
for information on how to set up Nix.

### Updating cardano-wallet version

To modify the cardano-wallet version, use [`niv update`](https://github.com/nmattia/niv#update).

    nix-shell --run "niv update cardano-wallet"

The version of `cardano-node` and `jormungandr` are defined by
cardano-wallet because it has specific version requirements for its
backends.

#### Overriding node backend versions

To use your own build of `cardano-node` or `jormungandr`, export your
`PATH` environment variable so that your build is ahead of those set
by the `nix-shell`.

## Design docs

 * [ADP-92](https://jira.iohk.io/browse/ADP-92) - Jira ticket
   containing user stories and task acceptance criteria.

 * [`update-system-byron.md`](./docs/update-system-byron.md) -
   Overview of how the update system works in the old codebase (Byron
   era).

 * [`windows-clean-shutdown.md`](./docs/windows-clean-shutdown.md) -
   How to stop the wallet and node processes on Windows.

 * [`dev.md`](./docs/dev.md) - Development information.

## Testing it out

After building, there is a very basic CLI that you can use to test.

Jormungandr self-node:

    ./bin/cardano-launcher jormungandr self test/data/jormungandr ./state-launcher

Byron mainnet:

    ./bin/cardano-launcher byron mainnet $BYRON_CONFIGS ./state-launcher
