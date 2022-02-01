const AutoDllPlugin = require('autodll-webpack-plugin');

const isCi = process.env.CI && process.env.CI !== '';

module.exports = async ({ config }) => {
  const [jsxRule] = config.module.rules;
  jsxRule.use.unshift('thread-loader');
  // Use Auto DLL plugin for faster development builds
  if (!isCi) {
    const [htmlWebpackPlugin] = config.plugins;
    const { templateParameters } = htmlWebpackPlugin.options;
    htmlWebpackPlugin.options.templateParameters = (...args) =>
      Object.assign(templateParameters.call(null, ...args), {
        dlls: ['./vendor.dll.js'],
      });
    config.plugins.push(
      new AutoDllPlugin({
        inject: true,
        filename: 'vendor.dll.js',
        entry: {
          vendor: [
            '@storybook/addon-actions',
            '@storybook/addon-knobs',
            '@storybook/addon-links',
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
            'react-datetime',
            'react-dom',
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
  }
  return {
    ...config,
    cache: !isCi,
    devtool: isCi ? 'none' : config.devtool,
    optimization: {
      minimize: false,
    },
    resolve: {
      extensions: ['.tsx', '.ts', '.js', '.json'],
    },
    module: {
      rules: [
        jsxRule,
        {
          test: /.tsx?$/,
          loader: 'babel-loader',
          options: {
            presets: [
              '@babel/preset-env',
              '@babel/preset-react',
              '@babel/preset-typescript',
            ],
          },
        },
        {
          test: /\.scss/,
          use: [
            {
              loader: 'style-loader',
            },
            {
              loader: 'css-loader',
              options: {
                sourceMap: !isCi,
                modules: true,
                localIdentName: '[name]_[local]',
                importLoaders: true,
              },
            },
            { loader: 'fast-sass-loader', options: { sourceMap: !isCi } },
          ],
        },
        {
          test: /\.css/,
          use: [{ loader: 'css-loader', options: { sourceMap: !isCi } }],
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
