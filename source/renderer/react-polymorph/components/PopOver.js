'use strict';
exports.__esModule = true;
exports.PopOver = void 0;
var react_1 = require('react');
var themes_1 = require('../utils/themes');
var ThemeContext_1 = require('./HOC/ThemeContext');
var index_1 = require('./index');
function PopOver(props) {
  var context = props.context,
    skin = props.skin,
    theme = props.theme,
    themeId = props.themeId,
    themeOverrides = props.themeOverrides;
  // Theme
  var themeContext =
    context || (0, react_1.useContext)(ThemeContext_1.ThemeContext);
  if (!themeContext) {
    throw new Error('No theming context provided.');
  }
  var composedTheme = (0, themes_1.composeTheme)(
    (0, themes_1.addThemeId)(theme || themeContext.theme, themeId),
    (0, themes_1.addThemeId)(themeOverrides, themeId),
    themeContext.ROOT_THEME_API
  );
  // Skin
  var PopOverSkin = skin || themeContext.skins[index_1.IDENTIFIERS.POP_OVER];
  return <PopOverSkin {...props} theme={composedTheme} />;
}
exports.PopOver = PopOver;
PopOver.defaultProps = {
  allowHTML: false,
  theme: null,
  themeId: index_1.IDENTIFIERS.POP_OVER,
  themeOverrides: {},
  themeVariables: {},
  popperOptions: {},
};
