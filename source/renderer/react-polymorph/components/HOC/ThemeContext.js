'use strict';
exports.__esModule = true;
exports.ThemeContext = void 0;
var create_react_context_1 = require('create-react-context');
var API_1 = require('../../themes/API');
var defaultContext = {
  skins: {},
  theme: API_1.ROOT_THEME_API,
  ROOT_THEME_API: API_1.ROOT_THEME_API,
};
exports.ThemeContext = (0, create_react_context_1['default'])(defaultContext);
