const AutoDllPlugin = require('autodll-webpack-plugin');

module.exports = async ({ config }) => {
  const [jsxRule] = config.module.rules;
  const [htmlWebpackPlugin] = config.plugins;
  const { templateParameters } = htmlWebpackPlugin.options;
  htmlWebpackPlugin.options.templateParameters = (...args) =>
    Object.assign(templateParameters.call(null, ...args), {
      dlls: ['./vendor.dll.js'],
    });
  // console.log(htmlWebpackPlugin.options.);
  config.plugins.push(
    new AutoDllPlugin({
      inject: true,
      filename: '[name].dll.js',
      entry: {
        vendor: [
          '@storybook/addon-actions',
          '@storybook/addon-knobs',
          '@storybook/addon-links',
          '@storybook/addon-notes',
          '@storybook/addons',
          '@storybook/core',
          '@storybook/react',
          'aes-js',
          'bignumber.js',
          'bip39',
          'blakejs',
          'bs58',
          'classnames',
          'es6-error',
          'faker',
          'humanize-duration',
          'lodash',
          'mobx',
          'mobx-react',
          'mobx-react-form',
          'mobx-react-router',
          'moment',
          'pbkdf2',
          'qrcode.react',
          'react',
          'react-copy-to-clipboard',
          'react-dom',
          'react-dropzone',
          'react-number-format',
          'react-router',
          'react-svg-inline',
          'recharts',
          'route-parser',
          'safe-buffer',
          'unorm',
          'validator',
        ],
      },
    })
  );
  return {
    ...config,
    cache: true,
    module: {
      rules: [
        jsxRule,
        {
          test: /\.scss/,
          use: [
            {
              loader: 'style-loader',
            },
            {
              loader: 'css-loader',
              options: {
                sourceMap: true,
                modules: true,
                localIdentName: '[name]_[local]',
                importLoaders: true,
              },
            },
            { loader: 'sass-loader', options: { sourceMap: true } },
          ],
        },
        {
          test: /\.css/,
          use: [{ loader: 'css-loader', options: { sourceMap: true } }],
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
            options: {
              limit: 10000,
            },
          },
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
    node: {
      fs: 'empty',
    },
  };
};
