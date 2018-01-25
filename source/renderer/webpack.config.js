const path = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  devtool: 'source-map',
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
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            cacheDirectory: true,
          }
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
    new ExtractTextPlugin('styles.css', { allChunks: true })
  ]
};
