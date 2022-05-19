<blockquote>
<sub>Document maintainer: Nikola Glumac<br/>Document status: Active</sub>
</blockquote>

# Daedalus
[![Build status](https://badge.buildkite.com/e173494257519752d79bb52c7859df6277c6d759b217b68384.svg?branch=master)](https://buildkite.com/input-output-hk/daedalus)
[![Release](https://img.shields.io/github/release/input-output-hk/daedalus.svg)](https://github.com/input-output-hk/daedalus/releases)

Daedalus - Cryptocurrency Wallet

## Setup development environment

### Linux/macOS

#### Yarn

[Yarn](https://yarnpkg.com/lang/en/docs/install) is required to install `npm` dependencies to build Daedalus.

#### Nix

[Nix](https://nixos.org/nix/) is needed to run Daedalus in `nix-shell`.

1. Install nix: `curl -L https://nixos.org/nix/install | sh` (use `sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume` on macOS Catalina)
2. Employ the signed IOHK binary cache:
   ```bash
   $ sudo mkdir -p /etc/nix
   $ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer
   ```
   and then add the following lines:
   ```
   build-users-group = nixbld

   max-jobs = auto
   cores = 0
   sandbox = false

   require-sigs = true
   trusted-users = root
   allowed-users = *

   substituters = https://hydra.iohk.io https://cache.nixos.org/
   trusted-substituters =
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   extra-sandbox-paths = /System/Library/Frameworks /System/Library/PrivateFrameworks /usr/lib

   # If you are running on a Mac with M1 chip please uncomment 'system' setting to enforce running on Rosetta2
   # system = x86_64-darwin
   ```

3. Run `nix-shell` with correct list of arguments or by using existing `package.json` scripts to load a shell with all the correct versions of all the required dependencies for development.

**Notes:**

If you get SSL error when running `nix-shell` (SSL peer certificate or SSH remote key was not OK) try the next steps:
1. Reinstall nix
   ```bash
   $ nix-env -e *
   $ curl -L https://nixos.org/nix/install | sh
   ```
2. Download certificate from https://docs.certifytheweb.com/docs/kb/kb-202109-letsencrypt/ and import to your keychain.

#### Running Daedalus with Cardano Node

##### Selfnode

1. Run `yarn nix:selfnode` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell` (use `KEEP_LOCAL_CLUSTER_RUNNING` environment variable to keep the local cluster running after Daedalus exits: `KEEP_LOCAL_CLUSTER_RUNNING=true yarn dev`)
3. Once Daedalus has started and has gotten past the loading screen run the following commands from a new terminal window if you wish to import funded wallets:
  - Byron wallets: `yarn byron:wallet:importer`
  - Shelley wallets: `yarn shelley:wallet:importer`
  - Mary wallets: `yarn mary:wallet:importer` (all of which contain native tokens which are visible once selfnode enters Mary era)
  - Yoroi Byron wallets: `yarn yoroi:wallet:importer`
  - _ITN Byron wallets:_ `yarn itn:byron:wallet:importer` **[Deprecated]**
  - _ITN Shelley wallets:_ `yarn itn:shelley:wallet:importer` **[Deprecated]**

   These scripts import 3 wallets by default. You can import up to 10 wallets by supplying `WALLET_COUNT` environment variable (e.g. `WALLET_COUNT=10 yarn mary:wallet:importer`).

   List of all funded wallet recovery phrases can be found here: https://github.com/input-output-hk/daedalus/blob/develop/utils/api-importer/mnemonics.js

**Notes:**
- Cardano wallet process ID shown on the "Diagnostics" screen is faked and expected to match the Cardano node process ID.
- Stake pool metadata is fetched directly by default (IOHK SMASH server option is not available).
- Token metadata is fetched from a mock token metadata server which is automatically ran alongside the local cluster (there is no need to run it [manually](https://github.com/input-output-hk/daedalus#native-token-metadata-server))
- Daedalus will ask you if you wish to keep the local cluster running after it exits - this option is useful if you need to preserve local cluster state between Daedalus restarts.

| Parameter | Value
| --- | ---
| slotLength | 0.2 sec
| epochLength | 50 slots
| desiredPoolNumber | 3
| minimumUtxoValue | 1 ADA

##### Mainnet

1. Run `yarn nix:mainnet` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

##### Flight

1. Run `yarn nix:flight` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

##### Testnet

1. Run `yarn nix:testnet` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

##### Native token metadata server

Daedalus, by default, uses the following metadata server for all networks except for the mainnet: `https://metadata.cardano-testnet.iohkdev.io/`.

It's also possible to use a mock server locally by running the following command in `nix-shell` prior to starting Daedalus:

```
$ mock-token-metadata-server --port 65432 ./utils/cardano/native-tokens/registry.json
Mock metadata server running with url http://localhost:65432/
```

Then proceed to launch Daedalus and make sure to provide the mock token metadata server port:

```
$ MOCK_TOKEN_METADATA_SERVER_PORT=65432 yarn dev
```

This enables you to modify the metadata directly by modifying the registry file directly:

```
$ vi ./utils/cardano/native-tokens/registry.json        # ..or any other editor, if you prefer
```

Use the following command to check if the mock server is working correctly:

```
$ curl -i -H "Content-type: application/json" --data '{"subjects":["789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1"],"properties":["name","description","ticker","unit","logo"]}'
http://localhost:65432/metadata/query
```
... and expect a "200 OK" response.

### Windows

This option is only for troubleshooting windows specific issues with hardware wallets. It is not recommended to use Windows as a developer environment.
Most of the commands need `nix` and will run only on Linux or macOS.

#### Requisites
- Windows 10/11
- Daedalus testnet installation (similar version used in branch) in `C:\Program Files\Daedalus Testnet`
- NodeJS 16
- Python2
- `yarn global add windows-build-tools` (if this does not work extract daedalus\nix\windows-usb-libs.zip under daedalus\build folder)
- Microsoft Build Tools 2015
- Microsoft Visual Studio 2017 (Include Desktop development with C++)
- `yarn config set msvsversion 2015 --global`

#### Steps
- `yarn install`
- `yarn dev:windows`

#### Updating upstream dependencies (cardano-wallet, cardano-node, and iohk-nix)

`Niv` is used to manage the version of upstream dependencies. The versions of these dependencies can be seen in `nix/sources.json`.

Dependencies are updated with the follow nix commands:
- Update cardano-wallet to the latest master: `nix-shell -A devops --arg nivOnly true --run "niv update cardano-wallet"`
- Update cardano-wallet to a specific revision: `nix-shell -A devops --arg nivOnly true --run "niv update cardano-wallet -a rev=91db88f9195de49d4fb4299c68fc3f6de09856ab"`
- Update cardano-node to a specific tag: `nix-shell -A devops --arg nivOnly true --run "niv update cardano-node -b tags/1.20.0"`
- Update iohk-nix to the latest master: `nix-shell -A devops --arg nivOnly true --run  "niv update iohk-nix -b master"`

#### Notes

`nix-shell` also provides a script for updating `yarn.lock` file:

    nix-shell -A fixYarnLock

### Cardano Wallet Api documentation

Api documentation for edge `cardano-wallet` version: https://input-output-hk.github.io/cardano-wallet/api/edge/

### Externals

If you use any 3rd party libraries which can't or won't be built with webpack, you must list them in your `source/main/webpack.config.js` and/or `source/renderer/webpack.config.js`：

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

### Debugging

You can debug the main process by following one of these approaches:
- [VSCode](https://code.visualstudio.com/docs/nodejs/nodejs-debugging#_attaching-to-nodejs)
- [Chrome](https://nodejs.org/en/docs/guides/debugging-getting-started/#inspector-clients)
- [IntelliJ](https://www.jetbrains.com/help/idea/run-debug-configuration-node-js-remote-debug.html)

The inspector runs on port 9229

### Linking with UI Libraries (e.g. React Polymorph)

You can link libraries with Daedalus using one of the following steps:

#### 1) Using `yalc`

1) Install `yalc` globally using `yarn global add yalc`.
2) Run `yalc publish` from the library's root directory that you want to link with Daedalus.
3) Switch to Daedalus and run `yalc add <package-name>` or preferably `yalc link <package-name>`.
4) You should be able to start Daedalus and see the changes you are making locally in the library.
5) To make sure your changes are reflected as you update code in the library, use `yalc push`.

#### 2) Using `yarn link`

1) From the Daedalus root directory, go to `node_modules/react` and `yarn link`.
2) Navigate to the `react-dom` package in the same directory and run `yarn link` again.
3) Go to the library's root directory and run `yarn link`, `yarn link react` and `yarn link react-dom`.
4) Go back to the Daedalus root directory and run `yarn link <package-name>`.
5) Finally, run `yarn build:watch` from the library's root directory.

## Testing

You can find more details regarding tests setup within
[Running Daedalus acceptance tests](https://github.com/input-output-hk/daedalus/blob/master/tests/README.md) README file.

**Notes:** Be aware that only a single Daedalus instance can run per state directory.
So you have to exit any development instances before running tests!

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

- --name, -n: Application name (default: Electron)
- --version, -v: Electron version (default: latest version)
- --asar, -a: [asar](https://github.com/atom/asar) support (default: false)
- --icon, -i: Application icon
- --all: pack for all platforms

Use `electron-packager` to pack your app with `--all` options for macOS, Linux and Windows platform. After build, you will find them in `release` folder. Otherwise, you will only find one for your OS.

## Automated builds

### CI/dev build scripts

Platform-specific build scripts facilitate building Daedalus the way it is built by the IOHK CI:

#### Linux/macOS

This script requires [Nix](https://nixos.org/nix/), (optionally) configured with the [IOHK binary cache][cache].

    scripts/build-installer-unix.sh [OPTIONS..]

The result can be found at `installers/csl-daedalus/daedalus-*.pkg`.

[cache]: https://github.com/input-output-hk/cardano-sl/blob/3dbe220ae108fa707b55c47e689ed794edf5f4d4/docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md#nix-build-mode-recommended

#### Windows

This batch file requires [Node.js](https://nodejs.org/en/download/) and
[7zip](https://www.7-zip.org/download.html).

    scripts/build-installer-win64.bat

The result will can be found at `.\daedalus-*.exe`.

#### Pure Nix installer build

This will use nix to build a Linux installer. Using the [IOHK binary
cache][cache] will speed things up.

    nix build -f ./release.nix mainnet.installer

The result can be found at `./result/daedalus-*.bin`.
