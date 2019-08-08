const isCi = process.env.CI && process.env.CI !== '';

module.exports = async ({ config }) => {
  const [jsxRule] = config.module.rules;
  jsxRule.use.unshift('thread-loader');
  return {
    ...config,
    cache: false,
    devtool: isCi ? 'none' : config.devtool,
    optimization: {
      minimize: false,
    },
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
