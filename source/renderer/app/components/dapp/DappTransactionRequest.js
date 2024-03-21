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
const classnames_1 = __importDefault(require('classnames'));
const Select_1 = require('@react-polymorph/components/Select');
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const DappTransactionRequest_scss_1 = __importDefault(
  require('./DappTransactionRequest.scss')
);
const Dialog_1 = __importDefault(require('../widgets/Dialog'));
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const WalletsDropdown_1 = __importDefault(
  require('../widgets/forms/WalletsDropdown')
);
const AssetsTransactionConfirmation_1 = __importDefault(
  require('../assets/AssetsTransactionConfirmation')
);
const formatters_1 = require('../../utils/formatters');
const assets_1 = require('../../utils/assets');
const MonospaceTextBlock_1 = require('../widgets/monospace-text-block/MonospaceTextBlock');
const CollapsibleSection_1 = require('../widgets/collapsible-section/CollapsibleSection');
const Separator_1 = require('../widgets/separator/Separator');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'dapp.transaction.request.title',
    defaultMessage: '!!!Transaction request',
    description: '"title" in the dApp transaction request dialog',
  },
  subtitle: {
    id: 'dapp.transaction.request.subtitle',
    defaultMessage: '!!!triggered from {triggeredFrom}',
    description: '"subtitle" in the dApp transaction request dialog',
  },
  fromWalletLabel: {
    id: 'dapp.transaction.request.fromWalletLabel',
    defaultMessage: '!!!From wallet',
    description: '"fromWalletLabel" in the dApp transaction request dialog',
  },
  receiverLabel: {
    id: 'dapp.transaction.request.receiver.label',
    defaultMessage: '!!!Receiver',
    description: '"receiver" in the dApp transaction request dialog',
  },
  walletsDropdownPlaceholder: {
    id: 'dapp.transaction.request.walletsDropdown.placeholder',
    defaultMessage: '!!!Select a wallet',
    description:
      '"walletsDropdownPlaceholder" in the dApp transaction request dialog',
  },
  addWalletLabel: {
    id: 'dapp.transaction.request.walletsDropdown.addWalletLabel',
    defaultMessage: '!!!Add a wallet',
    description: '"addWalletLabel" in the dApp transaction request dialog',
  },
  transactionFeeLabel: {
    id: 'dapp.transaction.request.transactionFee.label',
    defaultMessage: '!!!Transaction fee',
    description: '"transactionFeeLabel" in the dApp transaction request dialog',
  },
  additionalDataLabel: {
    id: 'dapp.transaction.request.additionalData.label',
    defaultMessage: '!!!Additional data',
    description: '"additionalDataLabel" in the dApp transaction request dialog',
  },
  metaDataLabel: {
    id: 'dapp.transaction.request.metaData.label',
    defaultMessage: '!!!Meta data',
    description: '"metaDataLabel" in the dApp transaction request dialog',
  },
  insufficientBalanceErrorMessage: {
    id: 'dapp.transaction.request.error.notEnoughAda',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {adaBalanceRequired} which is required for delegation to be available. Please select a wallet with <b>a minimum amount of {adaBalanceRequired}</b>.',
    description:
      '"Not enough ada" error in the dApp transaction request dialog',
  },
});
const DappTransactionRequest = (0, mobx_react_1.observer)((props) => {
  const {
    adaAmount,
    address,
    additionalData,
    assets,
    assetsAmounts,
    intl,
    metadata,
    onClose,
    onAddWallet,
    onSelectWallet,
    onSubmit,
    selectedWallet,
    transactionFee,
    triggeredFrom,
    wallets,
  } = props;
  const hasTokenError = (0, react_1.useMemo)(
    () =>
      assets.reduce((result, token, index) => {
        if (!selectedWallet) return false;
        if (result) return true;
        return (
          (0, assets_1.isTokenMissingInWallet)(selectedWallet, token) ||
          !(0, assets_1.tokenHasBalance)(token, assetsAmounts[index])
        );
      }, false),
    [assets, selectedWallet]
  );
  const adaBalanceRequired = adaAmount.plus(transactionFee);
  const walletsDropdownHasError = selectedWallet?.amount.isLessThan(
    adaBalanceRequired
  );
  const adaAmountErrorMessage = walletsDropdownHasError
    ? react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
        ...messages.insufficientBalanceErrorMessage,
        values: {
          adaBalanceRequired: (0, formatters_1.formattedWalletAmount)(
            adaBalanceRequired
          ),
        },
      })
    : null;
  const walletsDropdownStyles = (0, classnames_1.default)([
    DappTransactionRequest_scss_1.default.walletsDropdown,
    walletsDropdownHasError || hasTokenError
      ? DappTransactionRequest_scss_1.default.error
      : null,
  ]);
  const canSubmit =
    !!selectedWallet && !hasTokenError && !walletsDropdownHasError;
  const actions = [
    {
      label: intl.formatMessage(global_messages_1.default.cancel),
      onClick: onClose,
    },
    {
      label: intl.formatMessage(
        global_messages_1.default.dialogButtonContinueLabel
      ),
      primary: true,
      onClick: onSubmit,
      disabled: !canSubmit,
    },
  ];
  const componentStyles = (0, classnames_1.default)([
    DappTransactionRequest_scss_1.default.component,
  ]);
  return react_1.default.createElement(
    Dialog_1.default,
    {
      className: componentStyles,
      title: intl.formatMessage(messages.title),
      subtitle: intl.formatMessage(messages.subtitle, {
        triggeredFrom,
      }),
      actions: actions,
    },
    react_1.default.createElement(
      'p',
      { className: DappTransactionRequest_scss_1.default.label },
      intl.formatMessage(messages.fromWalletLabel)
    ),
    wallets.length
      ? react_1.default.createElement(WalletsDropdown_1.default, {
          className: walletsDropdownStyles,
          getStakePoolById: () => {},
          numberOfStakePools: 100,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; getStakePoolById: () => vo... Remove this comment to see the full error message
          onChange: onSelectWallet,
          placeholder: intl.formatMessage(messages.walletsDropdownPlaceholder),
          value: selectedWallet?.id,
          wallets: wallets,
        })
      : react_1.default.createElement(Select_1.Select, {
          onChange: onAddWallet,
          placeholder: intl.formatMessage(messages.walletsDropdownPlaceholder),
          value: '',
          options: [
            {
              label: intl.formatMessage(messages.addWalletLabel),
              value: 'add-wallet',
            },
          ],
          className: DappTransactionRequest_scss_1.default.addWalletSelect,
        }),
    react_1.default.createElement(Separator_1.Separator, null),
    react_1.default.createElement(
      'p',
      { className: DappTransactionRequest_scss_1.default.label },
      intl.formatMessage(messages.receiverLabel)
    ),
    react_1.default.createElement(
      'p',
      { className: DappTransactionRequest_scss_1.default.address },
      address
    ),
    react_1.default.createElement(AssetsTransactionConfirmation_1.default, {
      adaAmount: adaAmount,
      assets: assets,
      assetsAmounts: assetsAmounts,
      wallet: selectedWallet,
      adaError: adaAmountErrorMessage,
    }),
    react_1.default.createElement(
      'p',
      { className: DappTransactionRequest_scss_1.default.label },
      intl.formatMessage(messages.transactionFeeLabel)
    ),
    react_1.default.createElement(
      'div',
      { className: DappTransactionRequest_scss_1.default.transactionFee },
      '+',
      (0, formatters_1.formattedWalletAmount)(transactionFee)
    ),
    react_1.default.createElement(
      CollapsibleSection_1.CollapsibleSection,
      { header: intl.formatMessage(messages.additionalDataLabel) },
      react_1.default.createElement(
        MonospaceTextBlock_1.MonospaceTextBlock,
        null,
        additionalData
      )
    ),
    react_1.default.createElement(
      CollapsibleSection_1.CollapsibleSection,
      { header: intl.formatMessage(messages.metaDataLabel) },
      react_1.default.createElement(
        MonospaceTextBlock_1.MonospaceTextBlock,
        null,
        metadata
      )
    )
  );
});
exports.default = (0, react_intl_1.injectIntl)(DappTransactionRequest);
//# sourceMappingURL=DappTransactionRequest.js.map
