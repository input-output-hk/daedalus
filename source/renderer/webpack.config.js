const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ReactRefreshWebpackPlugin = require('@pmmmwh/react-refresh-webpack-plugin');

const isDevelopment = process.env.NODE_ENV === 'development';

module.exports = {
  entry: {
    index: './source/renderer/index.ts',
  },
  output: {
    path: path.join(process.cwd(), 'dist/renderer'),
    assetModuleFilename: 'assets/[hash][ext][query]',
  },
  mode: isDevelopment ? 'development' : 'production',
  target: 'web',
  devtool: isDevelopment ? 'eval-source-map' : 'source-map',
  optimization: {
    minimize: false,
  },
  devServer: {
    hot: true,
    static: {
      directory: path.join(__dirname, '../../dist'),
    },
    client: {
      overlay: true,
      progress: true,
    },
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: /source/,
        exclude: /source\/main/,
        loader: 'swc-loader',
        options: {
          jsc: {
            parser: {
              syntax: 'typescript',
              tsx: true,
              decorators: true,
            },
            transform: {
              react: {
                refresh: isDevelopment,
              },
            },
            target: 'es2019',
            loose: false,
          },
        },
      },
      {
        test: /\.scss/,
        use: [
          isDevelopment ? 'style-loader' : MiniCssExtractPlugin.loader,
          {
            loader: 'css-loader',
            options: {
              modules: {
                localIdentName: '[name]_[local]',
              },
              sourceMap: true,
              importLoaders: true,
            },
          },
          {
            loader: 'sass-loader',
            options: {
              sourceMap: true,
              implementation: require.resolve('sass'),
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
        test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
        exclude: /\.inline\.svg$/,
        type: 'asset/resource',
      },
      {
        test: /\.md$/,
        use: ['html-loader', 'markdown-loader?gfm=false'],
      },
    ],
  },
  resolve: {
    symlinks: true, // for native libraries
    extensions: ['.ts', '.tsx', '.js', '.json'],
    alias: {
      react: require.resolve('react'), // else, it’s added a few times to index.js 🙄
    },
    fallback: {
      process: require.resolve('process/browser'),
      path: require.resolve('path-browserify'),
      crypto: require.resolve('crypto-browserify'),
      stream: require.resolve('stream-browserify'),
      http: require.resolve('stream-http'),
      https: require.resolve('https-browserify'),
      url: require.resolve('url'),
      buffer: require.resolve('buffer/'), // https://www.npmjs.com/package/buffer#usage
    },
  },
  experiments: {
    syncWebAssembly: true,
  },
  plugins: [
    new webpack.ProvidePlugin({
      process: 'process/browser',
      Buffer: ['buffer', 'Buffer'],
    }),
    new webpack.DefinePlugin(
      Object.assign(
        {},
        process.env.NODE_ENV === 'production'
          ? {
              // Only bake in NODE_ENV value for production builds.
              'process.env.NODE_ENV': '"production"',
            }
          : {}
      )
    ),
    new webpack.EnvironmentPlugin({
      API_VERSION: 'dev',
      NETWORK: 'development',
      BUILD_NUMBER: 'dev',
    }),
    new HtmlWebpackPlugin({
      template: 'source/renderer/index.ejs',
      inject: 'body',
      scriptLoading: 'blocking',
    }),
    new MiniCssExtractPlugin({
      filename: 'styles.css',
    }),
    isDevelopment && new ReactRefreshWebpackPlugin(),
  ].filter(Boolean),
};
