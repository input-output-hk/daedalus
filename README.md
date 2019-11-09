<blockquote>
<sub>Document maintainer: Nikola Glumac<br/>Document status: Active</sub>
</blockquote>

# Daedalus
[![Build status](https://badge.buildkite.com/e173494257519752d79bb52c7859df6277c6d759b217b68384.svg?branch=master)](https://buildkite.com/input-output-hk/daedalus)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/input-output-hk/daedalus?branch=master&svg=true)](https://ci.appveyor.com/project/input-output/daedalus)
[![Release](https://img.shields.io/github/release/input-output-hk/daedalus.svg)](https://github.com/input-output-hk/daedalus/releases)

Daedalus - cryptocurrency wallet

## Automated build

### CI/dev build scripts

Platform-specific build scripts facilitate building Daedalus the way it is built
by the IOHK CI:

#### Linux/macOS

This script requires [Nix](https://nixos.org/nix/), (optionally)
configured with the [IOHK binary cache][cache].

    scripts/build-installer-unix.sh [OPTIONS..]

The result can be found at `installers/csl-daedalus/daedalus-*.pkg`.

[cache]: https://github.com/input-output-hk/cardano-sl/blob/3dbe220ae108fa707b55c47e689ed794edf5f4d4/docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md#nix-build-mode-recommended

#### Pure Nix installer build

This will use nix to build a Linux installer. Using the [IOHK binary
cache][cache] will speed things up.

    nix build -f ./release.nix mainnet.installer

The result can be found at `./result/daedalus-*.bin`.

# Development

`shell.nix` provides a way to load a shell with all the correct versions of all the required dependencies for development.

## V2 API Integration Guide [Jormungandr]

API docs for pinned cardano-wallet version: https://input-output-hk.github.io/cardano-wallet/api/v2019-10-16/

### Selfnode

1. Run `yarn nix:dev` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`
3. Once Daedalus has started, and has gotten past the loading screen, run `yarn v2:wallet:importer` from a new terminal window. This is only required if you wish to import some funded wallets

### QA testnet

1. Run `yarn nix:qa` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

### Nightly testnet

1. Run `yarn nix:nightly` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

### V2 Known Issues
- As network-info is stubbed, the NTP check will throw. Just disregard this for now.
- Lots of things have been temporarily commented out or mocked to get the integration started.
- TLS is not yet supported, so `request.js` has been overwritten to use the HTTP module for the time being.

### Updating Upstream Dependencies (cardano-wallet & Jormungandr)
Niv is used to manage the version of upstream dependencies. The versions of these dependencies can be seen in `nix/sources.json`.

Dependencies are updated with the follow nix command:
- Update to the latest master: `nix-shell -A devops --run "niv update cardano-wallet"`
- Update to a specific revision: `nix-shell -A devops --run "niv update cardano-wallet -a rev=1988f22895c45e12506ec83da0496ebdcdd17719"`

## Connect to staging cluster:

1. Start the nix-shell with staging environment `yarn nix:staging`
2. Within the nix-shell run any command like `yarn dev`

## Connect to Local Demo Cluster:

### Build and Run cardano-sl Demo Cluster

1. Install nix: `curl https://nixos.org/nix/install | sh`
2. Employ the signed IOHK binary cache:
   ```bash
   $ sudo mkdir -p /etc/nix
   $ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer
   ```
   and then add the following lines:
   ```
   substituters = https://hydra.iohk.io https://cache.nixos.org/
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   max-jobs = 2  # run at most two builds at once
   cores = 0     # the builder will use all available CPU cores
   ```
3. Build and run demo cluster: `scripts/launch/demo-nix.sh`

### Start Daedalus Using Demo Cluster

1. Start local cardano-sl demo cluster (`./scripts/launch/demo-nix.sh`)
2. Inspect the terminal output of cardano-sl and copy the timestamp from the message
   `system start:  1537184804`
3. Start the nix-shell with development environment `yarn nix:dev 1537184804` (timestamp is
different each time you restart the cardano-sl demo cluster)
4. Within the nix-shell run any command like `yarn dev`

## "Frontend only" mode

The `frontendOnlyMode` makes it possible to connect to manually started instances of cardano-node for advanced debugging purposes.

### How to connect:
#### Nix
1. Within the [cardano-sl repository](https://github.com/input-output-hk/cardano-sl), build a script for a certain network. E.g. for testnet: `nix-build -A connectScripts.testnet.wallet -o launch_testnet`
2. Launch this cluster + node with `./launch_testnet`
3. You should now have a `state-wallet-testnet` folder inside the cardano-sl repo. Copy the full path to the sub folder `tls` in there.
4. Within the Daedalus repo checkout this branch and run: `CARDANO_TLS_PATH=/path/to/tls CARDANO_HOST=localhost CARDANO_PORT=8090 nix-shell`

Now you should have a pre-configured nix-shell session where you can `yarn dev` as usual and Daedalus connects itself to the manually started cardano node.

##### Parameters:

| Param              | Mandatory | Default     |
|--------------------|-----------|-------------|
| `CARDANO_TLS_PATH` | Yes       |             |
| `CARDANO_HOST`     | No        | `localhost` |
| `CARDANO_PORT`     | No        | `8090`      |

So if you just start the default cardano node (which runs on localhost:8090) you can also start nix-shell with `CARDANO_TLS_PATH=/path/to/tls nix-shell`

#### Without Nix
1. If you have previously used `nix-shell`, run `rm -rf node_modules` as it is likely some of the bindings won't match your local nodejs version
2. `yarn install`
3. `WALLET_HOST=xxx WALLET_PORT=xxx yarn js-launcher`

This mode currently mocks TLS certificates as this is not being used for V2 integrations.

## Notes:

`shell.nix` also provides a script for updating yarn.lock. Run `nix-shell -A fixYarnLock`
to update `yarn.lock` file.

### Configuring the Network

There are three different network options you can run Daedalus in: `mainnet`, `testnet` and `development` (default).
To set desired network option use `NETWORK` environment variable:

```bash
$ export NETWORK=testnet
$ yarn dev
```

### Cardano Wallet API documentation

While running Daedalus in development mode you can access Cardano Wallet API documentation on the following URL: https://localhost:8091/docs/v1/index/.

# Testing

You can find more details regarding tests setup within
[Running Daedalus acceptance tests](https://github.com/input-output-hk/daedalus/blob/master/features/README.md) README file.

**Notes:** Be aware that only a single Daedalus instance can run per state directory.
So you have to exit any development instances before running tests!

## Wallet fault injection

General information about wallet fault injection can be found in the [Cardano's wallet-new README file](https://github.com/input-output-hk/cardano-sl/tree/develop/wallet-new#fault-injection).

`shell.nix` has support for passing the necessary flags:

- `--arg allowFaultInjection true` is necessary to enable any processing of faults, and
- `--arg walletExtraArgs '[ "--somefault" ]'` can be used for enabling certain fault types at startup.

# Windows

This batch file requires [Node.js](https://nodejs.org/en/download/) and
[7zip](https://www.7-zip.org/download.html).

    scripts/build-installer-win64.bat

The result will can be found at `.\daedalus-*.exe`.

### CSS Modules

This boilerplate out of the box is configured to use [css-modules](https://github.com/css-modules/css-modules).

All `.css` file extensions will use css-modules unless it has `.global.css`.

If you need global styles, stylesheets with `.global.css` will not go through the
css-modules loader. e.g. `app.global.css`

### Externals

If you use any 3rd party libraries which can't or won't be built with webpack, you must list them in your `webpack.config.base.js`：

```javascript
externals: [
  // put your node 3rd party libraries which can't be built with webpack here (mysql, mongodb, and so on..)
]
```

For a common example, to install Bootstrap, `yarn install --save bootstrap` and link them in the head of app.html

```html
<link rel="stylesheet" href="../node_modules/bootstrap/dist/css/bootstrap.css" />
<link rel="image/svg+xml" href="../node_modules/bootstrap/dist/fonts/glyphicons-halflings-regular.eot" />
...
```

Make sure to list bootstrap in externals in `webpack.config.base.js` or the app won't include them in the package:
```js
externals: ['bootstrap']
```

## Packaging

```bash
$ yarn run package
```

To package apps for all platforms:

```bash
$ yarn run package:all
```

To package apps with options:

```bash
$ yarn run package -- --[option]
```

### Options

- --name, -n: Application name (default: ElectronReact)
- --version, -v: Electron version (default: latest version)
- --asar, -a: [asar](https://github.com/atom/asar) support (default: false)
- --icon, -i: Application icon
- --all: pack for all platforms

Use `electron-packager` to pack your app with `--all` options for darwin (osx), linux and win32 (windows) platform. After build, you will find them in `release` folder. Otherwise, you will only find one for your os.
