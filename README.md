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

# Linux/macOS

This script requires [Nix](https://nixos.org/nix/), (optionally)
configured with the [IOHK binary cache][cache].

    scripts/build-installer-unix.sh [OPTIONS..]

The result can be found at `installers/csl-daedalus/daedalus-*.pkg`.

[cache]: https://github.com/input-output-hk/cardano-sl/blob/3dbe220ae108fa707b55c47e689ed794edf5f4d4/docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md#nix-build-mode-recommended

# Pure Nix installer build

This will use nix to build a Linux installer. Using the [IOHK binary
cache][cache] will speed things up.

    nix build -f ./release.nix mainnet.installer

The result can be found at `./result/daedalus-*.bin`.

# Nix Shell

`shell.nix` provides a way to load a shell with all the correct versions of all the required dependencies for development. Run `nix-shell` in the daedalus directory to start the shell.

`shell.nix` also provides a script for updating yarn.lock. Run `nix-shell -A fixYarnLock` to update `yarn.lock` file.

# Windows

This batch file requires [Node.js](https://nodejs.org/en/download/) and
[7zip](https://www.7-zip.org/download.html).

    scripts/build-installer-win64.bat

The result will can be found at `.\daedalus-*.exe`.

## Stepwise build

### Install Node.js dependencies.

To ensure secure and reproducible builds we are using [yarn](https://yarnpkg.com/lang/en/) to manage dependencies.

```bash
$ yarn install
```

## Development

Run with:

```bash
$ export CARDANO_TLS_PATH={path-to-cardano-sl}/run/tls-files/
$ yarn run dev
```

*Note: requires a node version >= 8 and an yarn version >= 1.7.0.*

### Development - with Cardano Wallet

Build and run [Cardano SL](https://github.com/input-output-hk/cardano-sl)

Build with:

```bash
$ brew install haskell-stack # OR curl -ssl https://get.haskellstack.org/ | sh
$ stack setup
$ stack install cpphs
$ brew install xz # OR sudo apt-get install xz-utils
$ brew install rocksdb # OR sudo apt-get install librocksdb-dev
$ git clone git@github.com:input-output-hk/cardano-sl.git
$ cd cardano-sl/
$ ./scripts/build/cardano-sl.sh
```

Run with:

```bash
$ tmux new-session -s cardano
$ WALLET_CLIENT_AUTH_DISABLE=1 ./scripts/launch/demo-with-wallet-api.sh
```

Stop with:

```bash
$ tmux kill-session -t cardano
```

### Development - network options

There are three different network options you can run Daedalus in: `mainnet`, `testnet` and `development` (default).
To set desired network option use `NETWORK` environment variable:

```bash
$ export NETWORK=testnet
$ yarn run dev
```

### Testing

You can run the test suite in two different modes:

**One-time run:**
For running tests once using the application in production mode:

```bash
$ yarn run test
```

**Watch & Rerun on file changes:**
For development purposes run the tests continuously in watch mode which will re-run tests when source code changes:

```bash
$ yarn run test:watch
```

You can find more details regarding tests setup within [Running Daedalus acceptance tests](https://github.com/input-output-hk/daedalus/blob/master/features/README.md) README file.

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
