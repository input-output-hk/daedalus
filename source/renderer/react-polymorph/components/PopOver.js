exports.__esModule = true;
exports.PopOver = void 0;
const react_1 = require('react');
const themes_1 = require('../utils/themes');
const ThemeContext_1 = require('./HOC/ThemeContext');
const index_1 = require('./index');

function PopOver(props) {
  const { context } = props;
  const { skin } = props;
  const { theme } = props;
  const { themeId } = props;
  const { themeOverrides } = props;
  // Theme
  const themeContext =
    context || (0, react_1.useContext)(ThemeContext_1.ThemeContext);
  if (!themeContext) {
    throw new Error('No theming context provided.');
  }
  const composedTheme = (0, themes_1.composeTheme)(
    (0, themes_1.addThemeId)(theme || themeContext.theme, themeId),
    (0, themes_1.addThemeId)(themeOverrides, themeId),
    themeContext.ROOT_THEME_API
  );
  // Skin
  const PopOverSkin = skin || themeContext.skins[index_1.IDENTIFIERS.POP_OVER];
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
