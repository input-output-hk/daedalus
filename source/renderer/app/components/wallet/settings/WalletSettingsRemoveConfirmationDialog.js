'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DeleteWalletConfirmationDialog_scss_1 = __importDefault(
  require('./DeleteWalletConfirmationDialog.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const timingConfig_1 = require('../../../config/timingConfig');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const DeleteWalletConfirmation_1 = __importDefault(
  require('./DeleteWalletConfirmation')
);
const WalletSettingsRemoveConfirmationDialog = (0, mobx_react_1.observer)(
  (props) => {
    const {
      countdownFn,
      isBackupNoticeAccepted = false,
      onAcceptBackupNotice,
      onCancel,
      onContinue,
      walletName,
      confirmationValue = '',
      onConfirmationValueChange,
      isSubmitting,
      isTest = false,
      isUnpair = false,
      messages,
      intl,
    } = props;
    const countdownRemaining = countdownFn(
      isTest ? 0 : timingConfig_1.DELETE_WALLET_COUNTDOWN
    );
    const countdownDisplay =
      !isUnpair && countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
    const isCountdownFinished = countdownRemaining <= 0;
    const isWalletNameConfirmationCorrect =
      confirmationValue.normalize('NFKC') === walletName.normalize('NFKC');
    // Always normalize non-breaking space into regular space.
    const isDisabled =
      !isUnpair &&
      (!isCountdownFinished ||
        !isBackupNoticeAccepted ||
        !isWalletNameConfirmationCorrect);
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    const handleSubmit = react_1.default.useCallback(
      () => !isDisabled && onContinue()
    );
    const buttonLabel = !isSubmitting
      ? `${intl.formatMessage(messages.confirmButtonLabel)} ${countdownDisplay}`
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        label: intl.formatMessage(global_messages_1.default.cancel),
        onClick: onCancel,
      },
      {
        className: 'attention',
        label: buttonLabel,
        onClick: onContinue,
        disabled: isDisabled,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.dialogTitle),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onCancel,
        className: DeleteWalletConfirmationDialog_scss_1.default.dialog,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onCancel }
        ),
      },
      react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
        tagName: 'p',
        ...messages.confirmationQuestion,
        values: {
          walletName,
        },
      }),
      !isUnpair &&
        react_1.default.createElement(DeleteWalletConfirmation_1.default, {
          isBackupNoticeAccepted: isBackupNoticeAccepted,
          confirmationValue: confirmationValue,
          onAcceptBackupNotice: onAcceptBackupNotice,
          handleSubmit: handleSubmit,
          onConfirmationValueChange: onConfirmationValueChange,
          checkboxLabel: intl.formatMessage(messages.confirmBackupNotice),
          inputLabel: intl.formatMessage(messages.enterRecoveryWordLabel),
        })
    );
  }
);
exports.default = (0, react_intl_1.injectIntl)(
  WalletSettingsRemoveConfirmationDialog
);
//# sourceMappingURL=WalletSettingsRemoveConfirmationDialog.js.map
