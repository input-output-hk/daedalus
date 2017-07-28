/**
 * Base webpack config used across other specific configs
 */

const path = require('path');
const fs = require('fs');
const validate = require('webpack-validator');
const webpack = require('webpack');
const HappyPack = require('happypack');
const ContextReplacementPlugin = require("webpack/lib/ContextReplacementPlugin");
const DllReferencePlugin = require("webpack/lib/DllReferencePlugin");

module.exports = validate({
  cache: true,
  module: {
    loaders: [{
      test: /\.jsx?$/,
      loader: 'happypack/loader',
      include: [
        path.join(__dirname, '../app'),
        path.join(__dirname, '../lib'),
        path.join(__dirname, '../electron'),
      ],
      query: {
        cacheDirectory: true,
      }
    }, {
      test: /\.json$/,
      loader: 'json-loader',
    }, {
      test: /\.md$/,
      loader: "html!markdown?gfm=false",
    },
    {
      test: /\.(?:png|jpg|svg|otf|ttf)$/,
      loader: 'url-loader',
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
    packageMains: ['webpack', 'browser', 'web', 'browserify', ['jam', 'main'], 'main'],
  },

  plugins: [
    new webpack.DefinePlugin({
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.CARDANO_API': process.env.CARDANO_API || 1,
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.DAEDALUS_VERSION': JSON.stringify(process.env.DAEDALUS_VERSION || 'dev')
    }),
    new HappyPack({
      loaders: ['babel-loader'],
    }),
    new DllReferencePlugin({
      context: path.join(__dirname, "../app"),
      manifest: require("../dll/vendor-manifest.json")
    }),
  ],

  externals: [
    // put your node 3rd party libraries which can't be built with webpack here
    // (mysql, mongodb, and so on..)
  ],

});
