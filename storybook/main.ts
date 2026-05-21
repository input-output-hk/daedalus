const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const webpack = require('webpack');

module.exports = {
  core: {
    builder: 'webpack5',
  },
  stories: ['../storybook/stories/index.ts'],
  addons: [
    '@storybook/addon-knobs',
    '@storybook/addon-actions',
    '@storybook/addon-links',
    require.resolve('./addons/DaedalusMenu/register.tsx'),
  ],
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
      new webpack.NormalModuleReplacementPlugin(
        /@trezor[\\/]transport[\\/]lib[\\/]transports[\\/]nodeusb\.js$/,
        require.resolve('@trezor/transport/lib/transports/nodeusb.browser.js')
      ),
      new webpack.NormalModuleReplacementPlugin(
        /@trezor[\\/]transport[\\/]lib[\\/]transports[\\/]udp\.js$/,
        require.resolve('@trezor/transport/lib/transports/udp.browser.js')
      ),
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
        // child_process is only used in the Electron main process (ARM detection).
        // Provide an empty stub so Storybook's webpack can bundle environment.ts.
        // The execFileSync call is unreachable in a browser context (isMacOS === false).
        child_process: false,
        // dgram is a Node.js UDP socket API used by @trezor/transport for Node USB
        // transport. Stub it out — Storybook runs in a browser and never opens USB.
        dgram: false,
        // fs is used by node-gyp-build (inside usb) to load native .node files.
        // Stub it out — native modules can't load in a browser context.
        fs: false,
        usb: false,
        'node-gyp-build': false,
      },
    };
    config.module.rules.push(
      {
        test: /\.tsx?$/,
        loader: 'swc-loader',
        options: {
          parseMap: true,
          sourceMaps: true,
          jsc: {
            parser: {
              syntax: 'typescript',
              tsx: true,
              decorators: true,
            },
            transform: {
              // MobX 5 uses legacy (Stage 1) decorators; without this SWC 1.7+
              // uses the new TC39 Stage 3 proposal which breaks @observable etc.
              legacyDecorator: true,
              // Class fields must use assignment (not Object.defineProperty) so
              // MobX prototype setters can intercept them during initialization.
              useDefineForClassFields: false,
              react: {
                runtime: 'automatic',
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
        use: 'svg-inline-loader',
        type: 'javascript/auto',
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
