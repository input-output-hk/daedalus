/**
 * Base webpack config used across other specific configs
 */

const path = require('path');
const validate = require('webpack-validator');
const webpack = require('webpack');

module.exports = validate({
  cache: true,
  module: {
    loaders: [{
      test: /\.jsx?$/,
      loader: 'babel-loader',
      include: [
        path.join(__dirname, '../app'),
        path.join(__dirname, '../lib'),
        path.join(__dirname, '../electron'),
      ],
    }, {
      test: /\.json$/,
      loader: 'json-loader',
    }, {
      test: /\.md$/,
      loader: 'html!markdown?gfm=false',
    },
    {
      test: /\.(?:png|jpg|svg|otf|ttf)$/,
      loader: 'url-loader',
      exclude: /\.inline\.svg$/,
    },
    {
      test: /\.inline\.svg$/,
      loader: 'raw-loader',
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
      'process.env.API': JSON.stringify(process.env.API || 'ada'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.DAEDALUS_VERSION': JSON.stringify(process.env.DAEDALUS_VERSION || 'dev')
    }),
  ],

  externals: [
    // put your node 3rd party libraries which can't be built with webpack here
    // (mysql, mongodb, and so on..)
  ],

});
