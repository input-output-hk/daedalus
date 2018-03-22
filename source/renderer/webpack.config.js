const path = require('path');
const webpack = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const AutoDllPlugin = require('autodll-webpack-plugin');
const yamljs = require('yamljs');

const reportUrl = yamljs.parseFile('installers/launcher-config.yaml').reportServer;

module.exports = {
  devtool: 'cheap-source-map',
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
        use: {
          loader: 'babel-loader?cacheDirectory&cacheIdentifier=' + Math.random(),
          // Fix for https://github.com/yahoo/babel-plugin-react-intl/issues/47 ^^
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
          loader: 'url-loader',
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
    // Set the ExtractTextPlugin output filename
    new ExtractTextPlugin('styles.css', { allChunks: true }),
    new webpack.DefinePlugin({
      'process.env.API': JSON.stringify(process.env.API || 'ada'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.DAEDALUS_VERSION': JSON.stringify(process.env.DAEDALUS_VERSION || 'dev'),
      'process.env.REPORT_URL': JSON.stringify(reportUrl),
    }),
    new AutoDllPlugin({
      filename: 'vendor.dll.js',
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
    })
  ]
};
