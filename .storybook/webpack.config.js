// you can use this file to add your custom webpack plugins, loaders and anything you like.
// This is just the basic way to add addional webpack configurations.
// For more information refer the docs: https://goo.gl/qPbSyX

// IMPORTANT
// When you add this file, we won't add the default configurations which is similar
// to "React Create App". This only has babel loader to load JavaScript.
devConfig = require('../webpack/webpack.config.development');
const HappyPack = require('happypack');

module.exports = {
  module: devConfig.module,
  sassLoader: devConfig.sassLoader,
  plugins: [
    new HappyPack({ loaders: ['babel-loader'] }),
  ]
};
