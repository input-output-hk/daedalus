import path from 'path';

// This preset fixes loading DaedalusMenu addon
// https://github.com/storybookjs/storybook/issues/7196#issuecomment-505858914
export default [path.resolve(__dirname, './ts-preset')];
