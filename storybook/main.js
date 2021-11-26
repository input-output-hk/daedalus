const isCi = process.env.CI && process.env.CI !== '';

module.exports = {
  stories: ['../source/**/*.stories.js', './stories/index.js'],
  addons: [
    '@storybook/addon-essentials',
    '@storybook/addon-knobs',
    '@storybook/addon-actions',
    '@storybook/addon-links',
    './addons/DaedalusMenu/register',
  ],
  previewHead: (head) => `
    ${head}
    ${isCi ? '' : '<script src="vendor.dll.js"></script>'}
  `,
};
