'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const DiscreetToggleTopBar_scss_1 = __importDefault(
  require('./DiscreetToggleTopBar.scss')
);
const DiscreetToggleTopBar_messages_1 = require('./DiscreetToggleTopBar.messages');
const context_1 = require('../../context');
const DiscreetModeToggle_1 = require('../discreet-toggle/DiscreetModeToggle');
const timingConfig_1 = require('../../../../config/timingConfig');
function DiscreetToggleTopBar({ intl, hasTadaIcon }) {
  const { isDiscreetMode, toggleDiscreetMode } = (0,
  context_1.useDiscreetModeFeature)();
  return react_1.default.createElement(
    'div',
    {
      className: (0, classnames_1.default)(
        DiscreetToggleTopBar_scss_1.default.root,
        hasTadaIcon && DiscreetToggleTopBar_scss_1.default.hasTadaIcon
      ),
    },
    react_1.default.createElement(
      PopOver_1.PopOver,
      {
        appendTo: 'parent',
        delay: timingConfig_1.TOOLTIP_DELAY,
        offset: [0, 0],
        className: DiscreetToggleTopBar_scss_1.default.popOverRoot,
        content: react_1.default.createElement(
          'span',
          { className: DiscreetToggleTopBar_scss_1.default.popOverContent },
          intl.formatMessage(
            DiscreetToggleTopBar_messages_1.messages[
              isDiscreetMode ? 'off' : 'on'
            ]
          ),
          ` `,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...DiscreetToggleTopBar_messages_1.messages.description,
          })
        ),
      },
      react_1.default.createElement(
        DiscreetModeToggle_1.DiscreetModeToggleComponent,
        {
          className: DiscreetToggleTopBar_scss_1.default.discreetToggle,
          isDiscreetMode: isDiscreetMode,
          onToggle: toggleDiscreetMode,
        }
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(DiscreetToggleTopBar)
);
//# sourceMappingURL=DiscreetToggleTopBar.js.map
