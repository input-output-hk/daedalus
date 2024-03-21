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
  deleteWalletHeader: {
    id: 'wallet.settings.deleteWallet.header',
    defaultMessage: '!!!Delete wallet',
    description: 'Delete wallet header on the wallet settings page.',
  },
  deleteWalletWarning1: {
    id: 'wallet.settings.deleteWallet.warning1',
    defaultMessage:
      '!!!Once you delete this wallet it will be removed from the Daedalus interface and you will lose access to any remaining funds in the wallet. The only way to regain access after deletion is by restoring it using your wallet recovery phrase.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  deleteWalletWarning2: {
    id: 'wallet.settings.deleteWallet.warning2',
    defaultMessage:
      '!!!You may wish to verify your recovery phrase before deletion to ensure that you can restore this wallet in the future, if desired.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  deleteButton: {
    id: 'wallet.settings.deleteWalletButtonLabel',
    defaultMessage: '!!!Delete wallet',
    description: 'Label for the delete button on wallet settings',
  },
});
const DeleteWallet = (0, mobx_react_1.observer)(
  ({
    openDialogAction,
    isDialogOpen,
    deleteWalletDialogContainer,
    onBlockForm,
    intl,
  }) => {
    const label = intl.formatMessage(exports.messages.deleteButton);
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        BorderedBox_1.default,
        { className: WalletSettings_scss_1.default.deleteWalletBox },
        react_1.default.createElement(
          'div',
          { className: WalletSettings_scss_1.default.title },
          intl.formatMessage(exports.messages.deleteWalletHeader)
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
              intl.formatMessage(exports.messages.deleteWalletWarning1)
            ),
            react_1.default.createElement(
              'p',
              null,
              intl.formatMessage(exports.messages.deleteWalletWarning2)
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
        ? deleteWalletDialogContainer
        : false
    );
  }
);
exports.default = (0, react_intl_1.injectIntl)(DeleteWallet);
//# sourceMappingURL=DeleteWallet.js.map
