const webpack = require('webpack');
const path = require('path');
const { exec } = require('child_process');

class ManageElectronProcessPlugin {
  apply(compiler) {
    if (compiler.options.watch) {
      let electronMainProcess = null;
      let isMainProcessBeingRestarted = false;
      compiler.hooks.done.tap('RestartElectronPlugin', () => {
        if (electronMainProcess === null) {
          electronMainProcess = exec('yarn electron .');
          electronMainProcess.once('close', () => {
            electronMainProcess = null;
            if (isMainProcessBeingRestarted) {
              electronMainProcess = exec('yarn electron .');
              isMainProcessBeingRestarted = false;
            }
          });
        } else if (!isMainProcessBeingRestarted) {
          isMainProcessBeingRestarted = true;
          electronMainProcess.kill();
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
  devtool: 'source-map',
  optimization: {
    minimize: false,
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js', '.json'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: /source/,
        exclude: /source\/renderer/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              cacheCompression: false,
              cacheDirectory: true,
              presets: [
                '@babel/preset-env',
                '@babel/preset-react',
                '@babel/preset-typescript',
              ],
            },
          },
        ],
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
      MOBX_DEV_TOOLS: 'false',
      BUILD_NUMBER: 'dev',
      IS_WATCH_MODE: 'false',
      KEEP_LOCAL_CLUSTER_RUNNING: 'false',
    }),
    new ManageElectronProcessPlugin(),
  ].filter(Boolean),
};
