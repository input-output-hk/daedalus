'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const TransferFundsStep1Dialog_scss_1 = __importDefault(
  require('./TransferFundsStep1Dialog.scss')
);
const WalletsDropdown_1 = __importDefault(
  require('../../widgets/forms/WalletsDropdown')
);
const ItemDropdownOption_1 = __importDefault(
  require('../../widgets/forms/ItemDropdownOption')
);
const formatters_1 = require('../../../utils/formatters');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog1.title',
    defaultMessage: '!!!Transfer funds',
    description: 'Title  in the transfer funds form.',
  },
  sourceWallet: {
    id: 'wallet.transferFunds.dialog1.sourceWallet',
    defaultMessage: '!!!From Byron legacy wallet',
    description: 'sourceWallet in the transfer funds form.',
  },
  targetWallet: {
    id: 'wallet.transferFunds.dialog1.targetWallet',
    defaultMessage: '!!!To Shelley-compatible wallet',
    description: 'targetWallet in the transfer funds form.',
  },
  buttonLabel: {
    id: 'wallet.transferFunds.dialog1.continueLabel',
    defaultMessage: '!!!Continue',
    description: 'buttonLabel in the transfer funds form.',
  },
});
class TransferFundsStep1Dialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue,
      onSetSourceWallet,
      targetWalletId,
      sourceWallet,
      wallets,
      numberOfStakePools,
      getStakePoolById,
      isSubmitting,
      error,
    } = this.props;
    const onClick = error ? onClose : onContinue;
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.dialogTitle),
        actions: [
          {
            className: isSubmitting
              ? TransferFundsStep1Dialog_scss_1.default.isSubmitting
              : null,
            label: intl.formatMessage(
              error ? global_messages_1.default.close : messages.buttonLabel
            ),
            onClick: !isSubmitting ? onClick : () => null,
            primary: true,
            disabled: isSubmitting || !this.props.targetWalletId,
          },
        ],
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'p',
        { className: TransferFundsStep1Dialog_scss_1.default.label },
        intl.formatMessage(messages.sourceWallet)
      ),
      react_1.default.createElement(
        'div',
        { className: TransferFundsStep1Dialog_scss_1.default.sourceWallet },
        react_1.default.createElement(ItemDropdownOption_1.default, {
          label: sourceWallet.name,
          detail: (0, formatters_1.formattedWalletAmount)(sourceWallet.amount),
          selected: true,
        })
      ),
      react_1.default.createElement(
        WalletsDropdown_1.default,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ label: any; wallets: Partial<Wallet>[]; on... Remove this comment to see the full error message
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ label: any; wallets: Partial<Wallet>[]; on... Remove this comment to see the full error message
          label: intl.formatMessage(messages.targetWallet),
          wallets: wallets,
          onChange: onSetSourceWallet,
          value: targetWalletId,
          numberOfStakePools: numberOfStakePools,
          getStakePoolById: getStakePoolById,
        }
      ),
      error &&
        react_1.default.createElement(
          'p',
          { className: TransferFundsStep1Dialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
}
exports.default = TransferFundsStep1Dialog;
//# sourceMappingURL=TransferFundsStep1Dialog.js.map
