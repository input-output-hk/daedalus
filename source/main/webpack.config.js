const webpack = require('webpack');
const yamljs = require('yamljs');
// const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');
// const lodash = require('lodash');

let reportUrl = '';
reportUrl = yamljs.parseFile('launcher-config.yaml').reportServer;

// Process env flags from buildkite and appveyor
// const isCi = process.env.CI && process.env.CI !== '';

module.exports = {
  mode: 'development',
  devtool: 'cheap-module-source-map',
  entry: {
    index: './source/main/index.js',
    preload: './source/main/preload.js',
  },
  optimization: {
    // https://github.com/webpack/webpack/issues/7470
    nodeEnv: false,
  },
  output: {
    filename: '[name].js',
  },
  /**
   * Set targed to Electron speciffic node.js env.
   * https://github.com/chentsulin/webpack-target-electron-renderer#how-this-module-works
   */
  target: 'electron-main',
  cache: true,
  /**
   * Disables webpack processing of __dirname and __filename.
   * If you run the bundle in node.js it falls back to these values of node.js.
   * https://github.com/webpack/webpack/issues/2010
   */
  node: {
    __dirname: false,
    __filename: false,
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: /source/,
        exclude: /source\/renderer/,
        use: {
          loader: 'babel-loader',
        },
      },
      {
        test: /(pdfkit|linebreak|fontkit|unicode|brotli|png-js).*\.js$/,
        use: {
          loader: 'transform-loader?brfs',
        }
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
    ]
  },
  plugins: [
    new webpack.DefinePlugin(Object.assign({
      'process.env.API_VERSION': JSON.stringify(process.env.API_VERSION || 'dev'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.BUILD_NUMBER': JSON.stringify(process.env.BUILD_NUMBER || 'dev'),
      'process.env.REPORT_URL': JSON.stringify(reportUrl),
      'process.env.IS_WATCH_MODE': process.env.IS_WATCH_MODE === 'true'
    }, process.env.NODE_ENV === 'production' ? {
      // Only bake in NODE_ENV value for production builds.
      'process.env.NODE_ENV': '"production"',
    } : {})),
    // Hard source plugin is broken for webpack 4 :(
    // https://github.com/mzgoddard/hard-source-webpack-plugin/issues/443
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
  ].filter(Boolean),
};
