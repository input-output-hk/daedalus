'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const NormalSwitch_1 = __importDefault(
  require('../../widgets/forms/NormalSwitch')
);
const SecuritySettings_scss_1 = __importDefault(
  require('./SecuritySettings.scss')
);
const SecuritySettings_messages_1 = __importDefault(
  require('./SecuritySettings.messages')
);
function SecuritySettings({
  intl,
  discreetMode,
  openDiscreetMode,
  onDiscreetModeToggle,
  onOpenDiscreetModeToggle,
}) {
  return react_1.default.createElement(
    'div',
    { className: SecuritySettings_scss_1.default.root },
    react_1.default.createElement(
      'div',
      { className: SecuritySettings_scss_1.default.setting },
      react_1.default.createElement(
        'div',
        { className: SecuritySettings_scss_1.default.title },
        intl.formatMessage(
          SecuritySettings_messages_1.default.discreetModeTitle
        )
      ),
      react_1.default.createElement(
        'div',
        { className: SecuritySettings_scss_1.default.settingContent },
        react_1.default.createElement(
          'p',
          { className: SecuritySettings_scss_1.default.description },
          intl.formatMessage(
            SecuritySettings_messages_1.default.discreetModeDescription
          )
        ),
        react_1.default.createElement(NormalSwitch_1.default, {
          checked: discreetMode,
          onChange: onDiscreetModeToggle,
        })
      )
    ),
    react_1.default.createElement(
      'div',
      { className: SecuritySettings_scss_1.default.setting },
      react_1.default.createElement(
        'div',
        { className: SecuritySettings_scss_1.default.title },
        intl.formatMessage(
          SecuritySettings_messages_1.default.openInDiscreetModeTitle
        )
      ),
      react_1.default.createElement(
        'div',
        { className: SecuritySettings_scss_1.default.settingContent },
        react_1.default.createElement(
          'p',
          { className: SecuritySettings_scss_1.default.description },
          intl.formatMessage(
            SecuritySettings_messages_1.default.openInDiscreetModeDescription
          )
        ),
        react_1.default.createElement(NormalSwitch_1.default, {
          checked: openDiscreetMode,
          onChange: onOpenDiscreetModeToggle,
        })
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(SecuritySettings);
//# sourceMappingURL=SecuritySettings.js.map
