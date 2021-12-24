const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const path = require('path');
const isCi = process.env.CI && process.env.CI !== '';

module.exports = {
  stories: ['../source/**/*.stories.js', './stories/index.js'],
  addons: [
    '@storybook/addon-essentials',
    '@storybook/addon-knobs',
    '@storybook/addon-actions',
    '@storybook/addon-links',
    '@storybook/builder-webpack5',
    '@storybook/manager-webpack5',
    './addons/DaedalusMenu/register',
  ],
  previewHead: (head) => `
    ${head}
    ${isCi ? '' : '<script src="vendor.dll.js"></script>'}
  `,
  core: {
    builder: 'webpack5',
  },
  // Make whatever fine-grained changes you need
  webpackFinal: async (config, { configType }) => {
    // `configType` has a value of 'DEVELOPMENT' or 'PRODUCTION'
    // You can change the configuration based on that.
    // 'PRODUCTION' is used when building the static version of storybook.

    // Make whatever fine-grained changes you need
    config.module.rules.push({
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
        {
          loader: 'babel-loader',
          options: {
            cacheCompression: false,
            cacheDirectory: true,
            plugins: [require.resolve('react-refresh/babel')].filter(Boolean),
          },
        },
      ],
    });

    // Return the altered config
    return config;
  },
};
