'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.OversaturationText = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const OversaturationText_scss_1 = __importDefault(
  require('./OversaturationText.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  oversaturationWarning: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.oversaturationWarning',
    defaultMessage:
      '!!!The selected stake pool will become oversaturated by {oversaturationPercentage}%, which will reduce future rewards for all delegators to that pool.',
    description:
      'Warning shown if pool is going to be saturated if delegation happens',
  },
});
function OversaturationTextComponent(props) {
  const { oversaturationPercentage, centerText } = props;
  const oversaturationClasses = (0, classnames_1.default)([
    OversaturationText_scss_1.default.component,
    centerText ? OversaturationText_scss_1.default.centerText : null,
  ]);
  return react_1.default.createElement(
    'p',
    { className: oversaturationClasses },
    react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
      ...messages.oversaturationWarning,
      values: {
        oversaturationPercentage,
      },
    })
  );
}
exports.OversaturationText = (0, react_intl_1.injectIntl)(
  OversaturationTextComponent
);
//# sourceMappingURL=OversaturationText.js.map
