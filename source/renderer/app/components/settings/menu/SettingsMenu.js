'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const SettingsMenuItem_1 = __importDefault(require('./SettingsMenuItem'));
const SettingsMenu_scss_1 = __importDefault(require('./SettingsMenu.scss'));
const routes_config_1 = require('../../../routes-config');
const SettingsMenu_messages_1 = __importDefault(
  require('./SettingsMenu.messages')
);
function SettingsMenu({ intl, onItemClick, isActiveItem, isFlight }) {
  return react_1.default.createElement(
    'div',
    null,
    react_1.default.createElement(
      'div',
      { className: SettingsMenu_scss_1.default.component },
      react_1.default.createElement(SettingsMenuItem_1.default, {
        label: intl.formatMessage(SettingsMenu_messages_1.default.general),
        onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.GENERAL),
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.GENERAL),
        className: 'general',
      }),
      react_1.default.createElement(SettingsMenuItem_1.default, {
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.SECURITY),
        label: intl.formatMessage(SettingsMenu_messages_1.default.security),
        onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.SECURITY),
        className: 'security',
      }),
      react_1.default.createElement(SettingsMenuItem_1.default, {
        label: intl.formatMessage(SettingsMenu_messages_1.default.wallets),
        onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.WALLETS),
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.WALLETS),
        className: 'wallets',
      }),
      react_1.default.createElement(SettingsMenuItem_1.default, {
        label: intl.formatMessage(SettingsMenu_messages_1.default.stakePools),
        onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.STAKE_POOLS),
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.STAKE_POOLS),
        className: 'stakePools',
      }),
      !isFlight &&
        react_1.default.createElement(SettingsMenuItem_1.default, {
          label: intl.formatMessage(SettingsMenu_messages_1.default.display),
          onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.DISPLAY),
          active: isActiveItem(routes_config_1.ROUTES.SETTINGS.DISPLAY),
          className: 'display',
        }),
      react_1.default.createElement(SettingsMenuItem_1.default, {
        label: intl.formatMessage(SettingsMenu_messages_1.default.termsOfUse),
        onClick: () =>
          onItemClick(routes_config_1.ROUTES.SETTINGS.TERMS_OF_USE),
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.TERMS_OF_USE),
        className: 'termsOfService',
      }),
      react_1.default.createElement(SettingsMenuItem_1.default, {
        label: intl.formatMessage(SettingsMenu_messages_1.default.support),
        onClick: () => onItemClick(routes_config_1.ROUTES.SETTINGS.SUPPORT),
        active: isActiveItem(routes_config_1.ROUTES.SETTINGS.SUPPORT),
        className: 'support',
      })
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(SettingsMenu)
);
//# sourceMappingURL=SettingsMenu.js.map
