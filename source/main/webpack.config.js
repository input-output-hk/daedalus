const webpack = require('webpack');
const path = require('path');
const { spawn } = require('child_process');

class ManageElectronProcessPlugin {
  isRunning = false;
  _process = null;
  _shouldRestart = false;
  start() {
    this._process = spawn('yarn', ['electron', '.'], {
      stdio: 'inherit',
      shell: true,
    });
    this.isRunning = true;
    // Handle next electron shutdown
    this._process.once('close', () => this.onProcessClose());
  }
  restart() {
    this._shouldRestart = true;
    this._process.kill();
  }
  onProcessClose() {
    this._process = null;
    this.isRunning = false;
    if (this._shouldRestart) {
      this.start();
      this._shouldRestart = false;
    }
  }
  apply(compiler) {
    if (compiler.options.watch) {
      compiler.hooks.done.tap('RestartElectronPlugin', () => {
        if (this.isRunning) {
          this.restart();
        } else {
          this.start();
        }
      });
    }
  }
}

const isDevelopment = process.env.NODE_ENV === 'development';

module.exports = {
  entry: {
    index: './source/main/index.ts',
    preload: './source/main/preload.ts',
  },
  output: {
    path: path.join(process.cwd(), 'dist/main'),
    assetModuleFilename: 'assets/[hash][ext][query]',
  },
  mode: isDevelopment ? 'development' : 'production',
  target: 'electron-main',
  devtool: isDevelopment ? 'eval-source-map' : 'source-map',
  optimization: {
    minimize: false,
  },
  resolve: {
    symlinks: true, // for native libraries
    extensions: ['.ts', '.tsx', '.js', '.json'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: /source/,
        exclude: /source\/renderer/,
        loader: 'swc-loader',
        options: {
          jsc: {
            parser: {
              syntax: 'typescript',
            },
            target: 'es2019',
            loose: false,
          },
        },
      },
      {
        test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
        exclude: /\.inline\.svg$/,
        type: 'asset/resource',
      },
    ],
  },
  externalsPresets: { node: true }, // in order to ignore built-in modules like path, fs, etc.
  externals: [
    {
      'js-chain-libs-node': 'commonjs2 js-chain-libs-node',
      usb: 'commonjs2 usb',
      'node-hid': 'commonjs2 node-hid',
      'trezor-connect': 'commonjs2 trezor-connect',
      pdfkit: 'commonjs2 pdfkit',
      'usb-detection': 'commonjs2 usb-detection',
    },
  ],
  plugins: [
    new webpack.DefinePlugin(
      Object.assign(
        {},
        process.env.NODE_ENV === 'production'
          ? {
              // Only bake in NODE_ENV value for production builds.
              'process.env.NODE_ENV': '"production"',
            }
          : {}
      )
    ),
    new webpack.EnvironmentPlugin({
      API_VERSION: 'dev',
      NETWORK: 'development',
      BUILD_NUMBER: 'dev',
      IS_WATCH_MODE: 'false',
      KEEP_LOCAL_CLUSTER_RUNNING: 'false',
    }),
    new ManageElectronProcessPlugin(),
  ].filter(Boolean),
};
