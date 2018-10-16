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

`shell.nix` provides a way to load a shell with all the correct versions of all the
required dependencies for development.

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
   trusted-substituters =
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspc
   ```
3. Build and run demo cluster: `scripts/launch/demo-nix.sh`

### Start Daedalus Using Demo Cluster

1. Start local cardano-sl demo cluster (`./scripts/launch/demo-nix.sh`)
2. Inspect the terminal output of cardano-sl and copy the timestamp from the message
   `Using system start time 1537184804`
3. Start the nix-shell with development environment `yarn nix:dev 1537184804` (timestamp is 
different each time you restart the cardano-sl demo cluster)
4. Within the nix-shell run any command like `yarn dev`

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

# Testing

You can find more details regarding tests setup within
[Running Daedalus acceptance tests](https://github.com/input-output-hk/daedalus/blob/master/features/README.md) README file.

**Notes:** Be aware that only a single Daedalus instance can run per state directory.
So you have to exit any development instances before running tests!

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
