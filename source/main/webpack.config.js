const webpack = require('webpack');

const isCi = process.env.CI && process.env.CI !== '';

module.exports = {
  mode: 'development',
  devtool: 'cheap-module-source-map',
  entry: {
    index: './source/main/index.ts',
    preload: './source/main/preload.ts',
  },
  optimization: {
    // https://github.com/webpack/webpack/issues/7470
    nodeEnv: false,
  },
  output: {
    filename: '[name].js',
  },
  /**
   * Set target to Electron specific node.js env.
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
  externals: {
    'js-chain-libs-node': 'commonjs2 js-chain-libs-node',
    'trezor-connect': 'commonjs2 trezor-connect',
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js', '.json'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: /source/,
        exclude: /source\/renderer/,
        use: (isCi ? [] : ['cache-loader']).concat([
          {
            loader: 'babel-loader',
            options: {
              presets: [
                '@babel/preset-env',
                '@babel/preset-react',
                '@babel/preset-typescript',
              ],
            },
          },
        ]),
      },
      {
        test: /(pdfkit|linebreak|fontkit|unicode|brotli|png-js).*\.js$/,
        use: {
          loader: 'transform-loader?brfs',
        },
      },
      {
        test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
        exclude: /\.inline\.svg$/,
        use: {
          loader: 'file-loader',
          options: {
            name: '[name]-[hash].[ext]',
            outputPath: 'assets/',
          },
        },
      },
    ],
  },
  plugins: [
    new webpack.DefinePlugin(
      Object.assign(
        {
          'process.env.API_VERSION': JSON.stringify(
            process.env.API_VERSION || 'dev'
          ),
          'process.env.NETWORK': JSON.stringify(
            process.env.NETWORK || 'development'
          ),
          'process.env.MOCK_TOKEN_METADATA_SERVER_PORT':
            process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0,
          'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
          'process.env.BUILD_NUMBER': JSON.stringify(
            process.env.BUILD_NUMBER || 'dev'
          ),
          'process.env.IS_WATCH_MODE': process.env.IS_WATCH_MODE === 'true',
          'process.env.KEEP_LOCAL_CLUSTER_RUNNING':
            process.env.KEEP_LOCAL_CLUSTER_RUNNING === 'true',
        },
        process.env.NODE_ENV === 'production'
          ? {
              // Only bake in NODE_ENV value for production builds.
              'process.env.NODE_ENV': '"production"',
            }
          : {}
      )
    ),
  ].filter(Boolean),
};
