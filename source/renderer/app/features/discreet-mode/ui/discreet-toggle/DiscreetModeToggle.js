'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DiscreetModeToggle = exports.DiscreetModeToggleComponent = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/reve... Remove this comment to see the full error message
const reveal_key_inline_svg_1 = __importDefault(
  require('../../../../assets/images/reveal-key.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/hide... Remove this comment to see the full error message
const hide_key_inline_svg_1 = __importDefault(
  require('../../../../assets/images/hide-key.inline.svg')
);
const context_1 = require('../../context');
const DiscreetModeToggle_scss_1 = __importDefault(
  require('./DiscreetModeToggle.scss')
);
function DiscreetModeToggleComponent({ className, isDiscreetMode, onToggle }) {
  return react_1.default.createElement(
    'button',
    {
      className: (0, classnames_1.default)(
        DiscreetModeToggle_scss_1.default.root,
        className
      ),
      onClick: onToggle,
      'aria-label': 'discreetModeToggle',
    },
    react_1.default.createElement(react_svg_inline_1.default, {
      svg: isDiscreetMode
        ? hide_key_inline_svg_1.default
        : reveal_key_inline_svg_1.default,
      className: (0, classnames_1.default)(
        DiscreetModeToggle_scss_1.default.icon,
        isDiscreetMode && DiscreetModeToggle_scss_1.default.hideIcon
      ),
    })
  );
}
exports.DiscreetModeToggleComponent = DiscreetModeToggleComponent;
function DiscreetModeToggleContainer({ className }) {
  const { isDiscreetMode, toggleDiscreetMode } = (0,
  context_1.useDiscreetModeFeature)();
  return react_1.default.createElement(DiscreetModeToggleComponent, {
    className: className,
    isDiscreetMode: isDiscreetMode,
    onToggle: toggleDiscreetMode,
  });
}
exports.DiscreetModeToggle = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(DiscreetModeToggleContainer)
);
//# sourceMappingURL=DiscreetModeToggle.js.map
