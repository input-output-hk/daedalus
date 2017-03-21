/**
 * Base webpack config used across other specific configs
 */

const path = require('path');
const validate = require('webpack-validator');
const webpack = require('webpack');

module.exports = validate({
  module: {
    loaders: [{
      test: /\.jsx?$/,
      loaders: ['babel-loader'],
      exclude: /node_modules/
    }, {
      test: /\.json$/,
      loader: 'json-loader'
    }, {
      test: /\.(?:png|jpg|svg|otf|ttf)$/,
      loader: 'url-loader'
    }]
  },

  output: {
    path: path.join(__dirname, '../dist'),
    filename: 'bundle.js',

    // https://github.com/webpack/webpack/issues/1114
    libraryTarget: 'commonjs2'
  },

  // https://webpack.github.io/docs/configuration.html#resolve
  resolve: {
    extensions: ['', '.js', '.jsx', '.json'],
    packageMains: ['webpack', 'browser', 'web', 'browserify', ['jam', 'main'], 'main']
  },

  plugins: [
    new webpack.DefinePlugin({
      'process.env.CARDANO_API': process.env.CARDANO_API || 1,
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.DAEDALUS_VERSION': JSON.stringify(process.env.DAEDALUS_VERSION || 'dev')
    }),
  ],

  externals: [
    // put your node 3rd party libraries which can't be built with webpack here
    // (mysql, mongodb, and so on..)
  ]
});
