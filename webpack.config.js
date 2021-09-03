const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const AutoDllPlugin = require('autodll-webpack-plugin');

// Process env flags from buildkite
const isTestEnv = process.env.NODE_ENV === 'test';
const isCi = process.env.CI && process.env.CI !== '';

const webpackConfig = [
  // {
  //   mode: 'development',
  //   devtool: 'cheap-module-source-map',
  //   entry: {
  //     index: path.join(__dirname, 'source', 'main', 'index.js'),
  //     preload: path.join(__dirname, 'source', 'main', 'preload.js'),
  //   },
  //   optimization: {
  //     // https://github.com/webpack/webpack/issues/7470
  //     nodeEnv: false,
  //   },
  //   output: {
  //     filename: '[name].js',
  //   },
  //   target: 'electron-main',
  //   cache: true,
  //   node: {
  //     __dirname: false,
  //     __filename: false,
  //   },
  //   externals: {
  //     'js-chain-libs-node': 'commonjs2 js-chain-libs-node',
  //     'trezor-connect': 'commonjs2 trezor-connect',
  //   },
  //   module: {
  //     rules: [
  //       {
  //         test: /\.js?$/,
  //         include: /source/,
  //         exclude: /source\/renderer/,
  //         use: (isCi ? [] : ['cache-loader']).concat(['babel-loader']),
  //       },
  //       {
  //         test: /(pdfkit|linebreak|fontkit|unicode|brotli|png-js).*\.js$/,
  //         use: {
  //           loader: 'transform-loader?brfs',
  //         },
  //       },
  //       {
  //         test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
  //         exclude: /\.inline\.svg$/,
  //         use: {
  //           loader: 'file-loader',
  //           options: {
  //             name: '[name]-[hash].[ext]',
  //             outputPath: 'assets/',
  //           },
  //         },
  //       },
  //     ],
  //   },
  //   plugins: [
  //     new webpack.DefinePlugin(
  //       Object.assign(
  //         {
  //           'process.env.API_VERSION': JSON.stringify(
  //             process.env.API_VERSION || 'dev'
  //           ),
  //           'process.env.NETWORK': JSON.stringify(
  //             process.env.NETWORK || 'development'
  //           ),
  //           'process.env.MOCK_TOKEN_METADATA_SERVER_PORT':
  //             process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0,
  //           'process.env.MOBX_DEV_TOOLS': process.env.MOBX_DEV_TOOLS || 0,
  //           'process.env.BUILD_NUMBER': JSON.stringify(
  //             process.env.BUILD_NUMBER || 'dev'
  //           ),
  //           'process.env.IS_WATCH_MODE': process.env.IS_WATCH_MODE === 'true',
  //           'process.env.KEEP_LOCAL_CLUSTER_RUNNING':
  //             process.env.KEEP_LOCAL_CLUSTER_RUNNING === 'true',
  //         },
  //         process.env.NODE_ENV === 'production'
  //           ? {
  //               // Only bake in NODE_ENV value for production builds.
  //               'process.env.NODE_ENV': '"production"',
  //             }
  //           : {}
  //       )
  //     ),
  //   ].filter(Boolean),
  // },
  {
    entry: path.join(__dirname, 'source', 'renderer', 'index.js'),
    output: {
      path: path.join(__dirname, 'dist', 'renderer'),
      filename: 'index.js',
    },
    // https://github.com/chentsulin/webpack-target-electron-renderer#how-this-module-works
    target: 'electron-renderer',
    module: {
      rules: [
        // {
        //   test: /\.js$/,
        //   include: path.join(__dirname, 'source', 'renderer'),
        //   exclude: [path.join(__dirname, 'source', 'main'), path.join(__dirname, 'source', 'common')],
        //   use: ['babel-loader'],
        // },
        {
          enforce: 'pre',
          test: /\.(js|jsx)$/,
          include: path.join(__dirname, 'source', 'renderer'),
          exclude: [
            path.join(__dirname, 'node_modules'),
            path.join(__dirname, 'source', 'main'),
            path.join(__dirname, 'source', 'common'),
          ],
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-flow'],
          },
        },
        {
          test: /\.scss/,
          use: [
            MiniCssExtractPlugin.loader,
            {
              loader: 'css-loader',
              options: {
                sourceMap: true,
                modules: true,
                localIdentName: '[name]_[local]',
                importLoaders: true,
              },
            },
            {
              loader: 'sass-loader',
              options: {
                sourceMap: true,
              },
            },
          ],
        },
        {
          test: /\.css/,
          use: [
            MiniCssExtractPlugin.loader,
            { loader: 'css-loader', options: { sourceMap: true } },
          ],
        },
        {
          test: /\.inline\.svg$/,
          use: 'svg-inline-loader',
        },
        {
          test: /\.(jpe?g|png|gif)$/i,
          loaders: [
            'file-loader?hash=sha512&digest=hex&name=[hash].[ext]',
            'image-webpack-loader?bypassOnDebug&optimizationLevel=7&interlaced=false',
          ],
        },
        {
          test: /\.md$/,
          use: [
            { loader: 'html-loader', options: { importLoaders: true } },
            { loader: 'markdown-loader?gfm=false' },
          ],
        },
      ],
    },
  },
];

module.exports = webpackConfig;
