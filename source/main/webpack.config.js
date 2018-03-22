const path = require('path');
const webpack = require('webpack');
const yamljs = require('yamljs');

const reportUrl = yamljs.parseFile('installers/launcher-config.yaml').reportServer;

module.exports = {
  devtool: 'cheap-source-map',
  entry: './source/main/index.js',
  output: {
    path: path.join(__dirname, './dist/main'),
    filename: 'index.js'
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
  plugins: [
    new webpack.DefinePlugin({
      'process.env.API': JSON.stringify(process.env.API || 'ada'),
      'process.env.NETWORK': JSON.stringify(process.env.NETWORK || 'development'),
      'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
      'process.env.DAEDALUS_VERSION': JSON.stringify(process.env.DAEDALUS_VERSION || 'dev'),
      'process.env.REPORT_URL': JSON.stringify(reportUrl),
    }),
  ],
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: /source/,
        use: {
          loader: 'babel-loader',
          options: {
            cacheDirectory: true,
          }
        },
      },
    ]
  },
};
