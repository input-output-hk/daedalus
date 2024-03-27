'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.messages = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const WalletSettings_scss_1 = __importDefault(require('./WalletSettings.scss'));
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const WalletSettingsRemoveButton_1 = __importDefault(
  require('./WalletSettingsRemoveButton')
);
const WalletSettingsRemoveConfirmationDialog_1 = __importDefault(
  require('./WalletSettingsRemoveConfirmationDialog')
);
exports.messages = (0, react_intl_1.defineMessages)({
  unpairWalletHeader: {
    id: 'wallet.settings.unpairWallet.header',
    defaultMessage: '!!!Unpair wallet',
    description: 'Unpair wallet header on the wallet settings page.',
  },
  unpairWalletWarning: {
    id: 'wallet.settings.unpairWallet.warning',
    defaultMessage:
      '!!!Once you unpair this wallet it will be removed from the Daedalus interface and you will lose access to any remaining funds in the wallet. The only way to regain access after deletion is by restoring it using your wallet recovery phrase.',
    description: 'Unpair wallet warning explaining the consequences.',
  },
  unpairButton: {
    id: 'wallet.settings.unpairWalletButtonLabel',
    defaultMessage: '!!!Unpair wallet',
    description: 'Label for the unpair button on wallet settings',
  },
});
const UnpairWallet = (0, mobx_react_1.observer)(
  ({
    openDialogAction,
    isDialogOpen,
    unpairWalletDialogContainer,
    onBlockForm,
    intl,
  }) => {
    const label = intl.formatMessage(exports.messages.unpairButton);
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        BorderedBox_1.default,
        { className: WalletSettings_scss_1.default.unpairWalletBox },
        react_1.default.createElement(
          'div',
          { className: WalletSettings_scss_1.default.title },
          intl.formatMessage(exports.messages.unpairWalletHeader)
        ),
        react_1.default.createElement(
          'div',
          { className: WalletSettings_scss_1.default.contentBox },
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...exports.messages.unpairWalletWarning,
              })
            )
          ),
          react_1.default.createElement(WalletSettingsRemoveButton_1.default, {
            label: label,
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
            onClick: react_1.default.useCallback(() => {
              onBlockForm();
              openDialogAction({
                dialog: WalletSettingsRemoveConfirmationDialog_1.default,
              });
            }),
          })
        )
      ),
      isDialogOpen(WalletSettingsRemoveConfirmationDialog_1.default)
        ? unpairWalletDialogContainer
        : false
    );
  }
);
exports.default = (0, react_intl_1.injectIntl)(UnpairWallet);
//# sourceMappingURL=UnpairWallet.js.map
