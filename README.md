<blockquote>
<sub>Document maintainer: Daniel Main<br/>Document status: Active</sub>
</blockquote>

# Daedalus
[![Build status](https://badge.buildkite.com/e173494257519752d79bb52c7859df6277c6d759b217b68384.svg?branch=master)](https://buildkite.com/input-output-hk/daedalus)
[![Release](https://img.shields.io/github/release/input-output-hk/daedalus.svg)](https://github.com/input-output-hk/daedalus/releases)
[![License](https://img.shields.io/github/license/input-output-hk/daedalus)](https://github.com/input-output-hk/daedalus/blob/develop/LICENSE)

Daedalus - Cryptocurrency Wallet

## Setup development environment

### Linux/macOS

#### Yarn

[Yarn](https://yarnpkg.com/lang/en/docs/install) is required to install `npm` dependencies to build Daedalus.

#### Nix

[Nix](https://nixos.org/nix/) is needed to run Daedalus in `nix develop` shell.

1. Install upstream nix using [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer)
   ```
   $ curl -fsSL https://install.determinate.systems/nix | sh -s -- install
   ```
2. Employ the signed IOHK binary cache:
   ```bash
   $ sudo mkdir -p /etc/nix
   $ sudo vi /etc/nix/nix.custom.conf       # ..or any other editor, if you prefer
   ```
   and then add the following 5 settings are set to:
   ```
   trusted-users = <your_user_id>

   substituters = https://cache.iog.io https://cache.nixos.org/

   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

   experimental-features = nix-command flakes

   # If you are running on a Mac with Apple Silicon chip, but want to also build for Intel:
   extra-platforms = x86_64-darwin aarch64-darwin
   ```
   
3. Reload nix-daemon.
   1. For MacOS
      1. Stop the daemon
      ```bash
      sudo launchctl remove org.nixos.nix-daemon
      ```
      2. Verify it's not running (only grep process should be listed)
      ```bash
      ps aux | grep nix-daemon
      ```
      3. Start the daemon
      ```bash
      sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
      ```
   2. For Linux
      1. Stop the daemon
      ```bash
      sudo systemctl stop nix-daemon.service nix-daemon.socket determinate-nixd.socket
      ```
      2. Verify it's not running (only grep process should be listed)
      ```bash
      ps aux | grep nix-daemon
      ```
      3. Start the sockets that will later auto-start the daemon
      ```bash
      sudo systemctl start nix-daemon.socket determinate-nixd.socket
      ```



4. Run `nix develop` with a correct argument or by using existing `package.json` scripts to load a shell with all the correct versions of all the required dependencies for development, e.g.:
    * `nix develop -L .#mainnet`
    * … which is equivalent to `yarn nix:mainnet`

#### Using direnv (Optional)

If you have [direnv](https://direnv.net/) installed, you can use it to automatically load the appropriate Nix development shell when entering the project directory.

1. Allow direnv for this directory:
   ```bash
   $ direnv allow
   ```

2. By default, the `preprod` environment will be loaded. To use a different network, set the `DAEDALUS_CLUSTER` environment variable:
   ```bash
   $ export DAEDALUS_CLUSTER=mainnet  # Use mainnet
   $ export DAEDALUS_CLUSTER=preview  # Use preview testnet
   $ export DAEDALUS_CLUSTER=selfnode # Use local selfnode
   ```

3. The environment will automatically reload when you `cd` into the project directory.

**Notes:**

If you get SSL error when running `nix develop` (SSL peer certificate or SSH remote key was not OK) try the next steps:
1. Reinstall nix
   ```bash
   $ nix-env -e *
   $ curl -L https://nixos.org/nix/install | sh
   ```
2. Download certificate from https://docs.certifytheweb.com/docs/kb/kb-202109-letsencrypt/ and import to your keychain.

#### Running Daedalus with Cardano Node

**Notes:**

Do not work around Chromium sandbox failures with
`ELECTRON_DISABLE_SANDBOX`, `--no-sandbox`, or
`--disable-setuid-sandbox`. Daedalus rejects sandbox-disabling configuration
and never retries without the sandbox. Use a supported system package or
diagnose the development host instead.

##### Selfnode

1. Run `yarn nix:selfnode` from `daedalus`.
2. Run `yarn dev` from the subsequent `nix develop` shell (use `KEEP_LOCAL_CLUSTER_RUNNING` environment variable to keep the local cluster running after Daedalus exits: `KEEP_LOCAL_CLUSTER_RUNNING=true yarn dev`)
3. Once Daedalus has started and has gotten past the loading screen run the following commands from a new terminal window if you wish to import funded wallets:
- Byron wallets: `yarn byron:wallet:importer`
- Shelley wallets: `yarn shelley:wallet:importer`
- Mary wallets: `yarn mary:wallet:importer` (all of which contain native tokens which are visible once selfnode enters Mary era)
- Yoroi Byron wallets: `yarn yoroi:wallet:importer`
- _ITN Byron wallets:_ `yarn itn:byron:wallet:importer` **[Deprecated]**
- _ITN Shelley wallets:_ `yarn itn:shelley:wallet:importer` **[Deprecated]**

These scripts import 3 wallets by default. You can import up to 10 wallets by supplying `WALLET_COUNT` environment variable (e.g. `WALLET_COUNT=10 yarn mary:wallet:importer`).

List of all funded wallet recovery phrases can be found here: https://github.com/input-output-hk/daedalus/blob/develop/utils/api-importer/mnemonics.ts

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
2. Run `yarn dev` from the subsequent shell

##### Preview

1. Run `yarn nix:preview` from `daedalus`.
2. Run `yarn dev` from the subsequent shell

##### Preprod

1. Run `yarn nix:preprod` from `daedalus`.
2. Run `yarn dev` from the subsequent shell

##### Native token metadata server

Daedalus, by default, uses the following metadata server for all networks except for the mainnet: `https://metadata.cardano-testnet.iohkdev.io/`.

It's also possible to use a mock server locally by running the following command in `nix develop` prior to starting Daedalus:

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

#### Updating upstream dependencies (cardano-wallet, cardano-node)

Nix flake is used to manage the version of upstream dependencies. The versions of these dependencies can be seen in `flake.nix`.

Dependencies are updated in the `inputs` section of `flake.nix` followed with e.g.:

```
nix flake lock --update-input cardano-wallet
```

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

The reusable Daedalus application package can be built and run without creating a platform installer. Its release status is platform-specific.

### Linux developer package

Build and run the wallet-only developer Nix package:

    nix build -L .#daedalus-mainnet
    nix run -L .#daedalus-mainnet

Cluster variants replace `mainnet` with `preprod`, `preview`, or `selfnode`.
This Nix-store package is for development only: it is not a Linux release
artifact, is not supported for production installation, and cannot enable the
dApp guest. Only installed fixed-path `.deb`, `.rpm`, or `.pkg.tar.zst`
packages can satisfy the Linux dApp package-identity and Chromium-sandbox gates.

### macOS (Intel, and Apple Silicon)

Build:

    nix build -L .#daedalus-mainnet

Run:

    nix run -L .#daedalus-mainnet

If you want to run an Intel build from an Apple Silicon machine:

    nix run -L .#packages.x86_64-darwin.daedalus-mainnet

## Installers

Platform-specific build commands facilitate building Daedalus installers the way it is built by the IOHK CI:

These commands require [Nix](https://nixos.org/nix/), optionally configured with the IOHK binary cache (recommended, see above).

### Linux

Linux releases ship only root-installable native `.deb`, `.rpm`, and
`.pkg.tar.zst` packages. Build a format on Linux:

    nix build -L .#deb-installer-mainnet
    nix build -L .#rpm-installer-mainnet
    nix build -L .#arch-installer-mainnet

The results contain
`daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.deb`,
`daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.rpm`, and
`daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.pkg.tar.zst`. Cluster
variants replace `mainnet` with `preprod`, `preview`, or `selfnode`. There is no
Linux `installer-<cluster>` output, so `nix build -L .#installer-mainnet` is not
available on Linux. Portable `.bin`, AppImage, Flatpak, and Snap artifacts are
not supported shipping channels.

All three formats install under `/opt/daedalus/<cluster>` and provide the exact
`/usr/bin/daedalus-<cluster>` launcher. They do not enable the dApp guest by
themselves. The dApp sandbox prerequisite is supported only by installed system
packages on Ubuntu 24.04/26.04, Debian 12/13, Fedora 43, Arch Linux installed
from the 2026.09.01 ISO, and Omarchy 4.0.2 installed from its 2026.08.31 image.
Later Arch or Omarchy snapshots and every other omitted distribution are
wallet-only until separately certified. Sandbox-disabling configuration,
package identity failure, or the local renderer canary failing also keeps dApp
launch unavailable. There is no unsandboxed retry, and the remaining guest and
release gates still apply.

#### Migrate a legacy portable installation

New releases do not provide a replacement `.bin` or portable automatic update.
Older portable clients receive an ordinary Linux release announcement linking
to the release notes and these manual installation instructions; the
announcement is not a `softwareUpdate` payload.

Use these non-destructive steps separately for each installed cluster:

1. Fully close Daedalus before installing or copying a backup. Record the
   existing state directory without moving or deleting it:

       state_root="${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus"
       printf 'Daedalus state: %s\n' "$state_root"

   Wallet and application state remains below that directory. If
   `XDG_DATA_HOME` has a custom value, retain the exact value. An optional
   offline backup must be taken only while Daedalus is stopped and to storage
   with enough free space; it does not replace the original directory during
   migration.
2. Check the legacy desktop override:

       desktop="$HOME/.local/share/applications/Daedalus-CLUSTER.desktop"
       readlink -- "$desktop"

   Replace `CLUSTER` with `mainnet`, `preprod`, `preview`, or `selfnode`. Only
   if this is a symbolic link whose printed target is the old
   `$HOME/.daedalus/<cluster>/` installation, move that symlink to an unused
   backup name, for example:

       mv -n -- "$desktop" "$desktop.legacy-portable"

   Do not move a regular file or an unrelated link. This prevents a user
   desktop entry from shadowing the package's
   `/usr/share/applications/Daedalus-<cluster>.desktop`.
3. Download the package for the same cluster from the official release
   announcement. From its download directory, replace the uppercase filename
   fields with the downloaded artifact's values and install exactly one
   matching format:

       sudo apt install ./daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.deb
       sudo dnf install ./daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.rpm
       sudo pacman -U ./daedalus-VERSION-BUILD-CLUSTER-REVISION-x86_64-linux.pkg.tar.zst

   Use `apt` on Debian/Ubuntu, `dnf` on Fedora, or `pacman -U` on the exact
   certified Arch/Omarchy snapshots. The Arch package aborts upgrades and
   removal while its exact packaged Electron process is running. Installation
   and package lifecycle scripts do not move, replace, or delete wallet state.
4. First launch the system package by its exact cluster command, for example:

       /usr/bin/daedalus-mainnet

   For a custom data home, supply the same value used by the portable
   installation:

       XDG_DATA_HOME=/exact/custom/data-home /usr/bin/daedalus-mainnet

   Configure that value consistently in the login/desktop environment before
   later desktop launches. A different or missing value can make wallets
   appear absent even though their data was not deleted.
5. Confirm the expected wallets and application state before considering any
   cleanup of the legacy executable. Keep both `$HOME/.daedalus` and
   `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus` untouched during this check.
   A stale
   `${XDG_DATA_HOME:-$HOME/.local/share}/Daedalus/<cluster>/namespaceHelper`
   symlink can remain harmlessly until it is separately identified and
   verified. This guide deliberately provides no broad removal command.

#### Upgrade an installed Linux package

Linux package upgrades are package-manager-mediated, not performed by
Electron. Fully close Daedalus, download the newer package for the same cluster
and format from the official release announcement, then run `sudo apt install
./DOWNLOADED.deb`, `sudo dnf install ./DOWNLOADED.rpm`, or `sudo pacman -U
./DOWNLOADED.pkg.tar.zst`. Roll back an Arch package with the same `pacman -U`
command and the retained older package. Restart through the same
`/usr/bin/daedalus-<cluster>` launcher with the same `XDG_DATA_HOME`. Leave the
Daedalus state directory untouched throughout. Daedalus never executes
downloaded Linux package bytes, invokes `sudo`, or mutates package-manager
state.

### macOS

Run this from a macOS machine:

    nix build -L .#installer-mainnet

### Windows

Run this from a Linux machine (cross-building):

    nix build -L .#installer-mainnet-x86_64-windows

The result will can be found at `result/daedalus-*.exe`.

### macOS (Intel, and Apple Silicon)

Run this from a macOS machine:

    nix build -L .#installer-mainnet

If you want to build an Intel version from an Apple Silicon machine:

    nix build -L .#packages.x86_64-darwin.installer-mainnet

The result can be found at `result/daedalus-*.pkg`.
