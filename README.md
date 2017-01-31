# daedalus
Daedalus - cryptocurrency wallet

## Install Dependencies.

```bash
$ npm install
```

## Development

run with one command:

```bash
$ npm run dev
```

Or run these two commands __simultaneously__ in different console tabs.

```bash
$ npm run hot-server
$ npm run start-hot
```

*Note: requires a node version >= 4 and an npm version >= 3. This project
defaults to 6.x*

## Development - with Cardano Wallet (daedalus-bridge)

Build and run daedalus-bridge [using instructions in the repo](https://github.com/input-output-hk/cardano-docs.iohk.io/blob/master/_docs/for-contributors/building-from-source.md#generating-types-for-daedalus-bridge)

Symlink the npm package in the subfolder `cardano-sl/daedalus`:
* `npm link` (inside the daedalus sub folder of the Cardano client)
* `npm link daedalus-client-api` (inside this daedalus frontend app)

Run with `CARDANO_API=true npm run dev`

## Testing

You can run the test suite in two different modes during development
(Currently you always need to run `npm run dev` before that) 

**One-time run:**
```bash
$ npm run test
```

**Watch & Rerun on file changes:**
```bash
$ npm run test-watch
```

## CSS Modules

This boilerplate out of the box is configured to use [css-modules](https://github.com/css-modules/css-modules).

All `.css` file extensions will use css-modules unless it has `.global.css`.

If you need global styles, stylesheets with `.global.css` will not go through the
css-modules loader. e.g. `app.global.css`


## Package

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


## Externals

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
