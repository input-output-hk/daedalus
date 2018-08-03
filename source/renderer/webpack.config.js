const path = require('path');
const webpack = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const AutoDllPlugin = require('autodll-webpack-plugin');
const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');
const lodash = require('lodash');
const yamljs = require('yamljs');

let reportUrl = '';
reportUrl = yamljs.parseFile('launcher-config.yaml').reportServer;

// Process env flags from buildkite and appveyor
const isCi = process.env.CI && process.env.CI !== '';

module.exports = {
  devtool: 'cheap-module-source-map',
  entry: './source/renderer/index.js',
  output: {
    path: path.join(__dirname, './dist/renderer'),
    filename: 'index.js'
  },
  // https://github.com/chentsulin/webpack-target-electron-renderer#how-this-module-works
  target: 'electron-renderer',
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
        use: ExtractTextPlugin.extract({
          use: [
            {
              loader: 'css-loader',
              options: {
                sourceMap: true,
                modules: true,
                localIdentName: '[name]_[local]',
                importLoaders: true,
              }
            },
            { loader: 'sass-loader', options: { sourceMap: true } }
          ],
          fallback: 'style-loader'
        })
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
            name: '[name]-[hash].[ext]',
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
      {
        test: /(pdfkit|linebreak|fontkit|unicode|brotli|png-js).*\.js$/,
        use: {
          loader: 'transform-loader?brfs',
        }
      }
    ]
  },
  plugins: [
    // Set the ExtractTextPlugin output filename
    new ExtractTextPlugin('styles.css', { allChunks: true }),
    new webpack.DefinePlugin({
      'process.env.API': JSON.stringify(process.env.API || 'ada'),
      'process.env.API_VERSION': JSON.stringify(process.env.API_VERSION || 'dev'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.BUILD_NUMBER': JSON.stringify(process.env.BUILD_NUMBER || 'dev'),
      'process.env.REPORT_URL': JSON.stringify(reportUrl),
    }),
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
          'react-addons-css-transition-group',
          'react-copy-to-clipboard',
          'react-css-themr',
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
    // Dont use caching for CI builds!
    !isCi && (
      new HardSourceWebpackPlugin({
        configHash: (webpackConfig) => (
          // Remove the `watch` flag to avoid different caches for static and incremental builds
          require('node-object-hash')({ sort: false }).hash(lodash.omit(webpackConfig, 'watch'))
        ),
        environmentPaths: {
          files: ['.babelrc', 'yarn.lock'],
        },
      })
    )
  ].filter(Boolean)
};
