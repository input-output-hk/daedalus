# Daedalus
[![Build status](https://badge.buildkite.com/e173494257519752d79bb52c7859df6277c6d759b217b68384.svg?branch=master)](https://buildkite.com/input-output-hk/daedalus)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/input-output-hk/daedalus?branch=master&svg=true)](https://ci.appveyor.com/project/input-output/daedalus)
[![Release](https://img.shields.io/github/release/input-output-hk/daedalus.svg)](https://github.com/input-output-hk/daedalus/releases)

Daedalus - cryptocurrency wallet

## Automated build

### CI/dev build scripts

Platform-specific build scripts facilitate building Daedalus the way it is built
by the IOHK CI:

   - `scripts/build-installer-unix.sh [OPTIONS..]`
   - `scripts/build-installer-windows.bat`

The result can be found at:
   - on OS X:    `${BUILD}/installers/csl-daedalus/daedalus-*.pkg`
   - on WIndows: `${BUILD}/installers/daedalus-*.exe`

### One-click build-fresh-daedalus scripts

These rely on the scripts from the previous section, but also go to a certain
trouble to ensure that dependencies are installed, and even check out a fresh
version of Daedalus from the specifid branch.

These are intended to be used by developers in a "clean rebuild" scenario, to
facilitate validation.

Dependencies:
   - on OS X:    `git`
   - on Windows: `Node.js`, `7zip`

Location:
   - on OS X:    https://github.com/input-output-hk/daedalus/blob/master/scripts/osx-build-fresh-daedalus.sh
   - on Windows: https://github.com/input-output-hk/daedalus/blob/master/scripts/windows-build-fresh-daedalus.bat

Invocation:
   ```shell
   {osx,windows}-build-fresh-daedalus.{sh,bat} [BRANCH] [GITHUB-USER] [OPTIONS...]
   ```
   ..where `BRANCH` defaults to the current release branch, and `GITHUB-USER`
   defaults to `input-output-hk`.

   The remaining `OPTIONS` are passed as-is to the respective build scripts.

## Stepwise build

### Install Node.js dependencies.

```bash
$ npm install
```

## Development

Run with:

```bash
$ npm run dev
```

*Note: requires a node version >= 8 and an npm version >= 5. This project defaults to 8.x*

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
$ ./scripts/launch/demo-with-wallet-api.sh
```

Stop with:

```bash
$ tmux kill-session -t cardano
```

### Development - network options

There are three different network options you can run Daedalus in: `mainnet`, `testnet` and `development` (default).
To set desired network option use `NETWORK` environment variable:

```bash
$ NETWORK=testnet npm run dev
```

### Testing

You can run the test suite in two different modes:

**One-time run:**
For running tests once using the application in production mode:

```bash
$ npm run test
```

**Watch & Rerun on file changes:**
For development purposes run the tests continuously in watch mode which will re-run tests when source code changes:

```bash
$ npm run test-watch
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

For a common example, to install Bootstrap, `npm i --save bootstrap` and link them in the head of app.html

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
$ npm run package
```

To package apps for all platforms:

```bash
$ npm run package-all
```

To package apps with options:

```bash
$ npm run package -- --[option]
```

### Options

- --name, -n: Application name (default: ElectronReact)
- --version, -v: Electron version (default: latest version)
- --asar, -a: [asar](https://github.com/atom/asar) support (default: false)
- --icon, -i: Application icon
- --all: pack for all platforms

Use `electron-packager` to pack your app with `--all` options for darwin (osx), linux and win32 (windows) platform. After build, you will find them in `release` folder. Otherwise, you will only find one for your os.
