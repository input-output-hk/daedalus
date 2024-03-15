const __rest =
  (this && this.__rest) ||
  function (s, e) {
    const t = {};
    for (var p in s)
      if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === 'function')
      for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
        if (
          e.indexOf(p[i]) < 0 &&
          Object.prototype.propertyIsEnumerable.call(s, p[i])
        )
          t[p[i]] = s[p[i]];
      }
    return t;
  };
exports.__esModule = true;
exports.PasswordInput = exports.calculatePasswordScore = void 0;
// @ts-nocheck
const fast_password_entropy_1 = require('fast-password-entropy');
const react_1 = require('react');
const ThemeContext_1 = require('./HOC/ThemeContext');
// internal utility functions
const themes_1 = require('../utils/themes');
// import constants
const _1 = require('.');

const calculatePasswordScore = function (password, entropyFactor) {
  if (entropyFactor === void 0) {
    entropyFactor = 0.01;
  }
  return Math.min(
    ((0, fast_password_entropy_1.default)(password) * entropyFactor).toFixed(2),
    1
  );
};
exports.calculatePasswordScore = calculatePasswordScore;
const STATE = {
  DEFAULT: 'default',
  ERROR: 'error',
  INSECURE: 'insecure',
  WEAK: 'weak',
  STRONG: 'strong',
};
// eslint-disable-next-line react/function-component-definition
const PasswordInput = function (props) {
  const { context } = props;
  const { passwordFeedbacks } = props;
  const { entropyFactor } = props;
  const { error } = props;
  const { minLength } = props;
  const { maxLength } = props;
  const { minStrongScore } = props;
  const { isPasswordRepeat } = props;
  const { repeatPassword } = props;
  const { skin } = props;
  const { state } = props;
  const { theme } = props;
  const { tooltip } = props;
  const rest = __rest(props, [
    'context',
    'passwordFeedbacks',
    'entropyFactor',
    'error',
    'minLength',
    'maxLength',
    'minStrongScore',
    'isPasswordRepeat',
    'repeatPassword',
    'skin',
    'state',
    'theme',
    'tooltip',
  ]);
  // Theme
  const themeContext =
    context || (0, react_1.useContext)(ThemeContext_1.ThemeContext);
  const composedTheme = (0, themes_1.composeTheme)(
    (0, themes_1.addThemeId)(theme || themeContext.theme, props.themeId),
    (0, themes_1.addThemeId)(props.themeOverrides, props.themeId),
    themeContext.ROOT_THEME_API
  );
  // Skin
  const PasswordInputSkin =
    skin || themeContext.skins[_1.IDENTIFIERS.PASSWORD_INPUT];
  // Logic
  let dynamicState = exports.PasswordInput.STATE.DEFAULT;
  let passwordFeedback = null;
  const password = props.value;
  const score = (0, exports.calculatePasswordScore)(password, entropyFactor);
  const isValidPassword =
    password.length >= minLength && password.length <= maxLength;
  const isNotEmpty = password.length > 0;
  if (error) {
    dynamicState = exports.PasswordInput.STATE.ERROR;
    passwordFeedback = error;
  } else if (tooltip) {
    passwordFeedback = tooltip;
  } else if (isPasswordRepeat) {
    if (repeatPassword === props.value) {
      dynamicState = exports.PasswordInput.STATE.DEFAULT;
      passwordFeedback = null;
    } else {
      dynamicState = exports.PasswordInput.STATE.ERROR;
      passwordFeedback = passwordFeedbacks.noMatch;
    }
  } else if (isValidPassword) {
    if (score < minStrongScore) {
      dynamicState = exports.PasswordInput.STATE.WEAK;
      passwordFeedback = passwordFeedbacks[exports.PasswordInput.STATE.WEAK];
    } else {
      dynamicState = exports.PasswordInput.STATE.STRONG;
      passwordFeedback = passwordFeedbacks[exports.PasswordInput.STATE.STRONG];
    }
  } else if (isNotEmpty) {
    dynamicState = exports.PasswordInput.STATE.INSECURE;
    passwordFeedback = passwordFeedbacks[exports.PasswordInput.STATE.INSECURE];
  }
  return (
    <PasswordInputSkin
      error={error}
      theme={composedTheme}
      score={score}
      state={state || dynamicState}
      tooltip={passwordFeedback}
      {...rest}
    />
  );
};
exports.PasswordInput = PasswordInput;
// Static Properties
exports.PasswordInput.STATE = STATE;
exports.PasswordInput.displayName = 'PasswordInput';
exports.PasswordInput.defaultProps = {
  debounceDelay: 1000,
  entropyFactor: 0.01,
  passwordFeedbacks: {
    insecure: 'insecure',
    weak: 'weak',
    strong: 'strong',
    noMatch: "doesn't match",
  },
  isPasswordRepeat: false,
  isTooltipOpen: false,
  isShowingTooltipOnFocus: true,
  isShowingTooltipOnHover: true,
  minLength: 10,
  maxLength: 255,
  minStrongScore: 0.75,
  readOnly: false,
  theme: null,
  themeId: _1.IDENTIFIERS.PASSWORD_INPUT,
  themeOverrides: {},
  useDebounce: true,
  value: '',
};
