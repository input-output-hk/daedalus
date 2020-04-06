<blockquote>
<sub>Document maintainer: Nikola Glumac<br/>Document status: Active</sub>
</blockquote>

# Daedalus
[![Build status](https://badge.buildkite.com/e173494257519752d79bb52c7859df6277c6d759b217b68384.svg?branch=master)](https://buildkite.com/input-output-hk/daedalus)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/input-output-hk/daedalus?branch=master&svg=true)](https://ci.appveyor.com/project/input-output/daedalus)
[![Release](https://img.shields.io/github/release/input-output-hk/daedalus.svg)](https://github.com/input-output-hk/daedalus/releases)

Daedalus - Cryptocurrency Wallet

## Installation

### Yarn

[Yarn](https://yarnpkg.com/lang/en/docs/install) is required to install `npm` dependencies to build Daedalus.

### Nix

[Nix](https://nixos.org/nix/) is needed to run Daedalus in `nix-shell`.

**Note:** There are special instructions for
[installing Nix on macOS Catalina](https://github.com/NixOS/nix/issues/2925#issuecomment-564149154).

1. Install nix: `curl https://nixos.org/nix/install | sh`
2. Employ the signed IOHK binary cache:
   ```bash
   $ sudo mkdir -p /etc/nix
   $ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer
   ```
   and then add the following lines:
   ```
   substituters = https://hydra.iohk.io https://cache.nixos.org/
   trusted-substituters =
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   max-jobs = 2  # run at most two builds at once
   cores = 0     # the builder will use all available CPU cores
   extra-sandbox-paths = /System/Library/Frameworks
   ```
3. Run `nix-shell` with correct list of arguments or by using existing `package.json` scripts to load a shell with all the correct versions of all the required dependencies for development.

## Development

### Running Daedalus with Cardano Node

#### Selfnode

1. Run `yarn nix:selfnode` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`
3. Once Daedalus has started, and has gotten past the loading screen, run `yarn byron:wallet:importer` from a new terminal window. This is only required if you wish to import some funded wallets.

#### Mainnet

1. Run `yarn nix:mainnet` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

#### Flight

1. Run `yarn nix:flight` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

#### Testnet

1. Run `yarn nix:testnet` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

#### Staging

1. Run `yarn nix:staging` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

### Running Daedalus with Jormungandr

#### ITN Selfnode

1. Run `yarn nix:itn_selfnode` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`
3. Once Daedalus has started, and has gotten past the loading screen, run `yarn itn:shelley:wallet:importer` from a new terminal window. This is only required if you wish to import some funded wallets. It is also possible to import funded legacy wallets by running `yarn itn:byron:wallet:importer` script.

#### ITN Rewards V1

1. Run `yarn nix:itn` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

#### QA Testnet

1. Run `yarn nix:qa` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

#### Nightly Testnet

1. Run `yarn nix:nightly` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix-shell`

### Updating upstream dependencies (cardano-wallet, cardano-node & Jormungandr)

`Niv` is used to manage the version of upstream dependencies. The versions of these dependencies can be seen in `nix/sources.json`.

Dependencies are updated with the follow nix commands:
- Update to the latest master: `nix-shell -A devops --run "niv update cardano-wallet"`
- Update to a specific revision: `nix-shell -A devops --run "niv update cardano-wallet -a rev=1988f22895c45e12506ec83da0496ebdcdd17719"`

#### Notes

`nix-shell` also provides a script for updating `yarn.lock` file:

    nix-shell -A fixYarnLock

### Cardano Wallet Api documentation

Api documentation for edge `cardano-wallet` version: https://input-output-hk.github.io/cardano-wallet/api/edge/

### CSS modules

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

## Testing

You can find more details regarding tests setup within
[Running Daedalus acceptance tests](https://github.com/input-output-hk/daedalus/blob/master/features/README.md) README file.

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
