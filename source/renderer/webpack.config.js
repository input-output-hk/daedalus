const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const AutoDllPlugin = require('autodll-webpack-plugin');
const yamljs = require('yamljs');

// TODO: enable again when hard-source is fixed
// https://github.com/mzgoddard/hard-source-webpack-plugin/issues/443

// const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');
// const lodash = require('lodash');
// const isCi = process.env.CI && process.env.CI !== '';

let reportUrl = '';
reportUrl = yamljs.parseFile('launcher-config.yaml').reportServer;

// Process env flags from buildkite and appveyor
const isTestEnv = process.env.NODE_ENV === 'test';

module.exports = {
  mode: 'development',
  devtool: 'cheap-module-source-map',
  entry: './source/renderer/index.js',
  optimization: {
    // https://github.com/webpack/webpack/issues/7470
    nodeEnv: false,
  },
  output: {
    path: path.join(__dirname, './dist/renderer'),
    filename: 'index.js'
  },
  // https://github.com/chentsulin/webpack-target-electron-renderer#how-this-module-works
  target: isTestEnv ? 'electron-renderer' : 'web',
  cache: true,
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: /source/,
        exclude: /source\/main/,
        use: {
          loader: 'babel-loader',
        },
      },
      {
        test: /\.scss/,
        use: [
          MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader',
            options: {
              sourceMap: true,
              modules: true,
              localIdentName: '[name]_[local]',
              importLoaders: true,
            }
          },
          {
            loader: 'sass-loader',
            options: {
              sourceMap: true
            }
          }
        ]
      },
      {
        test: /\.inline\.svg$/,
        use: 'svg-inline-loader',
      },
      {
        test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
        exclude: /\.inline\.svg$/,
        use: {
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            outputPath: 'assets/'
          }
        }
      },
      {
        test: /\.md$/,
        use: [
          { loader: 'html-loader', options: { importLoaders: true } },
          { loader: 'markdown-loader?gfm=false' },
        ]
      },
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: 'styles.css',
    }),
    new webpack.DefinePlugin(Object.assign({
      'process.env.API_VERSION': JSON.stringify(process.env.API_VERSION || 'dev'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.BUILD_NUMBER': JSON.stringify(process.env.BUILD_NUMBER || 'dev'),
      'process.env.REPORT_URL': JSON.stringify(reportUrl)
    }, process.env.NODE_ENV === 'production' ? {
      // Only bake in NODE_ENV value for production builds.
      'process.env.NODE_ENV': '"production"',
    } : {})),
    new AutoDllPlugin({
      filename: 'vendor.dll.js',
      context: path.join(__dirname, '..'),
      entry: {
        vendor: [
          'aes-js',
          'bignumber.js',
          'bip39',
          'blakejs',
          'bs58',
          'classnames',
          'es6-error',
          'humanize-duration',
          'lodash',
          'mobx',
          'mobx-react',
          'mobx-react-form',
          'mobx-react-router',
          'moment',
          'pbkdf2',
          'qrcode.react',
          'react',
          'react-copy-to-clipboard',
          'react-dom',
          'react-dropzone',
          'react-number-format',
          'react-router',
          'react-svg-inline',
          'recharts',
          'route-parser',
          'safe-buffer',
          'unorm',
          'validator'
        ]
      }
    }),
    // TODO: enable again when hard-source is fixed
    // https://github.com/mzgoddard/hard-source-webpack-plugin/issues/443

    // Dont use caching for CI builds!
    // !isCi && (
    //   new HardSourceWebpackPlugin({
    //     configHash: (webpackConfig) => (
    //       // Remove the `watch` flag to avoid different caches for static and incremental builds
    //       require('node-object-hash')({ sort: false }).hash(lodash.omit(webpackConfig, 'watch'))
    //     ),
    //     environmentPaths: {
    //       files: ['.babelrc', 'yarn.lock'],
    //     },
    //   })
    // )
  ].filter(Boolean)
};
