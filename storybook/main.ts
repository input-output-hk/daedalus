// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'mini-css-extract-plugin'.
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'webpack'.
const webpack = require('webpack');

const isCi = process.env.CI && process.env.CI !== '';
module.exports = {
  stories: ['../storybook/stories/index.ts'],
  addons: [
    '@storybook/addon-knobs',
    '@storybook/addon-actions',
    '@storybook/addon-links',
    './addons/DaedalusMenu/register',
  ],
  core: {
    builder: 'webpack5',
  },
  // Make whatever fine-grained changes you need
  webpackFinal: async (config, { configType }) => {
    // `configType` has a value of 'DEVELOPMENT' or 'PRODUCTION'
    // You can change the configuration based on that.
    // 'PRODUCTION' is used when building the static version of storybook.
    // Make whatever fine-grained changes you need

    config.plugins = [
      ...config.plugins,
      new webpack.ProvidePlugin({
        Buffer: ['buffer', 'Buffer'],
      }),
      new webpack.ProvidePlugin({
        process: 'process/browser',
      }),
    ];
    config.experiments = {
      syncWebAssembly: true,
    };
    config.resolve = {
      extensions: ['.ts', '.tsx', '.js', '.json'],
      fallback: {
        process: require.resolve('process/browser'),
        path: require.resolve('path-browserify'),
        crypto: require.resolve('crypto-browserify'),
        stream: require.resolve('stream-browserify'),
        http: require.resolve('stream-http'),
        https: require.resolve('https-browserify'),
        url: require.resolve('url'),
        buffer: require.resolve('buffer/'), // https://www.npmjs.com/package/buffer#usage
        os: require.resolve('os-browserify/browser'),
      },
    };
    config.module.rules.push(
      {
        test: /\.tsx?$/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              cacheCompression: false,
              cacheDirectory: true,
              presets: [
                '@babel/preset-env',
                '@babel/preset-react',
                '@babel/preset-typescript',
              ],
            },
          },
        ],
      },
      {
        test: /\.scss/,
        use: [
          'style-loader',
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
        type: 'asset/source',
      },
      {
        test: /\.(woff2?|eot|ttf|otf|png|jpe?g|gif|svg)(\?.*)?$/,
        exclude: /\.inline\.svg$/,
        type: 'asset/resource',
      }
    );

    // Return the altered config
    return config;
  },
};
