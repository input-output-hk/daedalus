/* eslint-disable max-len */
/**
 * Build config for development process that uses Hot-Module-Replacement
 * https://webpack.github.io/docs/hot-module-replacement-with-webpack.html
 */

const path = require('path');
const webpack = require('webpack');
const validate = require('webpack-validator');
const merge = require('webpack-merge');
const formatter = ('eslint-formatter-pretty');
const Joi = require('webpack-validator').Joi;
const baseConfig = require('./webpack.config.base');

const port = process.env.PORT || 4000;

module.exports = validate(merge(baseConfig, {
  debug: true,

  devtool: 'cheap-module-eval-source-map',

  entry: [
    `webpack-hot-middleware/client?path=http://localhost:${port}/__webpack_hmr`,
    'babel-polyfill',
    './app/index'
  ],

  output: {
    publicPath: `http://localhost:${port}/dist/`
  },

  module: {
    preLoaders: [
      {
        test: /\.js$/,
        loader: 'eslint-loader',
        exclude: /[node_modules|pos\-haskell\-prototype]/
      }
    ],
    loaders: [
      {
        test: /\.global\.scss$/,
        loaders: [
          'style?sourceMap',
          'css?sourceMap',
          'sass?sourceMap'
        ]
      },
      {
        test: /^((?!\.global).)*\.scss$/,
        loaders: [
          'style?sourceMap',
          'css?sourceMap&modules&localIdentName=[name]_[local]&importLoaders=1',
          'sass?sourceMap'
        ]
      },
    ]
  },

  sassLoader: {
    data: '@import "' + './app/themes/daedalus/_theme.scss' + '";'
  },

  // eslint: {
  //   formatter: formatter
  // },

  plugins: [
    // https://webpack.github.io/docs/hot-module-replacement-with-webpack.html
    new webpack.HotModuleReplacementPlugin(),

    // “If you are using the CLI, the webpack process will not exit with an error code by enabling this plugin.”
    // https://github.com/webpack/docs/wiki/list-of-plugins#noerrorsplugin
    new webpack.NoErrorsPlugin(),

    // NODE_ENV should be production so that modules do not perform certain development checks
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development'),
    })
  ],

  // https://github.com/chentsulin/webpack-target-electron-renderer#how-this-module-works
  target: 'electron-renderer'
}),
  {
    schemaExtension: Joi.object({
      sassLoader: Joi.any()
    })
  }
);
