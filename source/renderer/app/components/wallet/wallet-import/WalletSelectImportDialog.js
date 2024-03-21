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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Button_1 = require('@react-polymorph/components/Button');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const WalletSelectImportDialog_scss_1 = __importDefault(
  require('./WalletSelectImportDialog.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const walletExportTypes_1 = require('../../../types/walletExportTypes');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const InlineEditingSmallInput_1 = __importDefault(
  require('../../widgets/forms/InlineEditingSmallInput')
);
const check_w_inline_svg_1 = __importDefault(
  require('../../../assets/images/check-w.inline.svg')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.select.import.dialog.title',
    defaultMessage: '!!!Found wallets',
    description: 'Select import wallets dialog title',
  },
  description: {
    id: 'wallet.select.import.dialog.description',
    defaultMessage:
      '!!!These wallets were found in your Daedalus state directory.<p>Please select the wallets you want to import.</p>',
    description:
      'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
  },
  unnamedWalletsTitle: {
    id: 'wallet.select.import.dialog.unnamedWalletsTitle',
    defaultMessage: '!!!Unnamed wallets',
    description: 'unnamedWalletsTitle',
  },
  passwordProtected: {
    id: 'wallet.select.import.dialog.passwordProtected',
    defaultMessage: '!!!Password protected',
    description: 'Password protected',
  },
  walletExists: {
    id: 'wallet.select.import.dialog.walletExists',
    defaultMessage: '!!!Wallet already exists',
    description: 'Wallet already exists',
  },
  noPassword: {
    id: 'wallet.select.import.dialog.noPassword',
    defaultMessage: '!!!No password',
    description: 'No password',
  },
  importingWallet: {
    id: 'wallet.select.import.dialog.importingWallet',
    defaultMessage: '!!!Importing wallet...',
    description: 'Importing wallet...',
  },
  walletName: {
    id: 'wallet.select.import.dialog.walletName',
    defaultMessage: '!!!Enter wallet name',
    description: 'Enter wallet name',
  },
  notFound: {
    id: 'wallet.select.import.dialog.notFound',
    defaultMessage: '!!!Name not found',
    description: 'Name not found',
  },
  enterWalletNameTooltip: {
    id: 'wallet.select.import.dialog.enterWalletNameTooltip',
    defaultMessage: '!!!Enter a wallet name first',
    description: 'Enter a wallet name first',
  },
  maxWalletsReachedTooltip: {
    id: 'wallet.select.import.dialog.maxWalletsReachedTooltip',
    defaultMessage:
      '!!!Daedalus supports up to {maxWalletsCount} wallets. You will need to remove another wallet before you can import this one.',
    description: 'Max number of wallets reached',
  },
  walletImported: {
    id: 'wallet.select.import.dialog.walletImported',
    defaultMessage: '!!!Wallet imported',
    description: 'Wallet imported',
  },
  buttonLabel: {
    id: 'wallet.select.import.dialog.buttonLabel',
    defaultMessage: '!!!Import selected wallets',
    description: 'Import selected wallets',
  },
  linkLabel: {
    id: 'wallet.select.import.dialog.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'wallet.select.import.dialog.linkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900000623463',
    description: '"Learn more" link URL on the wallet import file dialog',
  },
  closeWindow: {
    id: 'wallet.select.import.dialog.closeWindow',
    defaultMessage: '!!!Close window',
    description: 'Close window',
  },
});
let WalletSelectImportDialog = class WalletSelectImportDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  getWalletStatus = (wallet) => {
    const { intl } = this.context;
    const importingStatus = intl.formatMessage(messages.importingWallet);
    const noPasswordStatus = intl.formatMessage(messages.noPassword);
    const hasPasswordStatus = intl.formatMessage(messages.passwordProtected);
    const alreadyExistsStatus = intl.formatMessage(messages.walletExists);
    const walletImportedStatus = intl.formatMessage(messages.walletImported);
    let walletStatus;
    if (
      wallet.import.status === walletExportTypes_1.WalletImportStatuses.RUNNING
    ) {
      walletStatus = importingStatus;
    } else if (
      wallet.import.status === walletExportTypes_1.WalletImportStatuses.EXISTS
    ) {
      walletStatus = alreadyExistsStatus;
    } else if (
      wallet.import.status ===
      walletExportTypes_1.WalletImportStatuses.COMPLETED
    ) {
      walletStatus = walletImportedStatus;
    } else if (wallet.isEmptyPassphrase) {
      walletStatus = noPasswordStatus;
    } else {
      walletStatus = hasPasswordStatus;
    }
    return walletStatus;
  };
  getWalletStatusIcon = (wallet, index) => {
    const {
      nameValidator,
      onToggleWalletImportSelection,
      isMaxNumberOfWalletsReached,
    } = this.props;
    let statusIcon;
    if (
      wallet.import.status ===
        walletExportTypes_1.WalletImportStatuses.UNSTARTED ||
      wallet.import.status ===
        walletExportTypes_1.WalletImportStatuses.PENDING ||
      wallet.import.status === walletExportTypes_1.WalletImportStatuses.ERRORED
    ) {
      const invalidWalletName =
        wallet.name === null || !nameValidator(wallet.name);
      const walletNotSelectable =
        isMaxNumberOfWalletsReached &&
        wallet.import.status !==
          walletExportTypes_1.WalletImportStatuses.PENDING;
      const disabled = invalidWalletName || walletNotSelectable;
      let isOpeningUpward = true;
      const checkboxes = document.getElementsByClassName(
        'SimpleCheckbox_check'
      );
      if (checkboxes.length && wallet.hasName) {
        const topWrapper = document.getElementsByClassName(
          'WalletSelectImportDialog_topWrapper'
        );
        if (checkboxes[index] && topWrapper.length) {
          const checkboxTopOffset = checkboxes[index].getBoundingClientRect()
            .top;
          const topWrapperTopOffset = topWrapper[0].getBoundingClientRect().top;
          const topPart = topWrapperTopOffset + 121;
          const spaceForTooltip = checkboxTopOffset - topPart;
          if (
            (walletNotSelectable && spaceForTooltip < 85) ||
            (invalidWalletName && spaceForTooltip < 27)
          ) {
            isOpeningUpward = false;
          }
        }
      }
      statusIcon = react_1.default.createElement(Checkbox_1.Checkbox, {
        onChange: () => {
          onToggleWalletImportSelection({
            index: wallet.index,
          });
        },
        checked:
          wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.PENDING,
        disabled: disabled,
        skin: CheckboxSkin_1.CheckboxSkin,
      });
      if (disabled) {
        statusIcon = react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content: invalidWalletName
              ? this.context.intl.formatMessage(messages.enterWalletNameTooltip)
              : react_1.default.createElement(react_intl_1.FormattedMessage, {
                  ...messages.maxWalletsReachedTooltip,
                  values: {
                    maxWalletsCount: numbersConfig_1.MAX_ADA_WALLETS_COUNT,
                  },
                }),
            isBounded: walletNotSelectable,
            placement: isOpeningUpward ? 'top' : 'bottom',
          },
          statusIcon
        );
      }
    } else if (
      wallet.import.status === walletExportTypes_1.WalletImportStatuses.RUNNING
    ) {
      statusIcon = react_1.default.createElement(LoadingSpinner_1.default, {
        medium: true,
      });
    } else if (
      wallet.import.status ===
        walletExportTypes_1.WalletImportStatuses.COMPLETED ||
      wallet.import.status === walletExportTypes_1.WalletImportStatuses.EXISTS
    ) {
      statusIcon = react_1.default.createElement(react_svg_inline_1.default, {
        svg: check_w_inline_svg_1.default,
        className:
          WalletSelectImportDialog_scss_1.default.walletsStatusIconCheckmark,
      });
    }
    return statusIcon;
  };
  getInlineEditingSmallInput = (
    wallet,
    validationMessage,
    placeholderMessage,
    nameValidator,
    onWalletNameChange
  ) => {
    return react_1.default.createElement(InlineEditingSmallInput_1.default, {
      isActive: false,
      className: WalletSelectImportDialog_scss_1.default.walletsInputFieldInner,
      isDisabled:
        wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.COMPLETED ||
        wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.EXISTS ||
        wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.RUNNING,
      inputFieldValue: wallet.name || '',
      placeholder: placeholderMessage,
      isValid: nameValidator,
      validationErrorMessage: validationMessage,
      onSubmit: (name) =>
        onWalletNameChange({
          index: wallet.index,
          name,
        }),
      maxLength: 40,
      successfullyUpdated: true,
    });
  };
  render() {
    const { intl } = this.context;
    const {
      isSubmitting,
      exportedWallets,
      pendingImportWalletsCount,
      onContinue,
      onClose,
      onOpenExternalLink,
      onWalletNameChange,
      nameValidator,
    } = this.props;
    const title = intl.formatMessage(messages.title);
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.buttonLabel)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const onLinkClick = () =>
      onOpenExternalLink(intl.formatMessage(messages.linkUrl));
    const walletsWithNames = exportedWallets.filter(({ hasName }) => hasName);
    const walletsWithoutNames = exportedWallets.filter(
      ({ hasName }) => !hasName
    );
    let previousWalletId = '';
    let rowNumber = 1;
    const anyWalletWithoutName = walletsWithoutNames.filter(
      (item) =>
        (!item.name || item.name.length < 3) &&
        item.import.status === walletExportTypes_1.WalletImportStatuses.PENDING
    );
    const isDisabled =
      isSubmitting || anyWalletWithoutName.length || !pendingImportWalletsCount;
    const buttonClasses = (0,
    classnames_1.default)(
      WalletSelectImportDialog_scss_1.default.actionButton,
      [isDisabled ? WalletSelectImportDialog_scss_1.default.disabled : null]
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: WalletSelectImportDialog_scss_1.default.dialog,
        closeOnOverlayClick: false,
        onClose: onClose,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onRequestClose: onClose,
        shouldCloseOnOverlayClick: false,
        shouldCloseOnEsc: false,
        defaultThemeOverrides: true,
      },
      react_1.default.createElement(
        'div',
        { className: WalletSelectImportDialog_scss_1.default.component },
        react_1.default.createElement(DialogCloseButton_1.default, {
          className: WalletSelectImportDialog_scss_1.default.closeButton,
          icon: close_cross_thin_inline_svg_1.default,
          onClose: onClose,
        }),
        react_1.default.createElement(
          'div',
          { className: WalletSelectImportDialog_scss_1.default.content },
          react_1.default.createElement(
            'div',
            { className: WalletSelectImportDialog_scss_1.default.topWrapper },
            react_1.default.createElement(
              'div',
              { className: WalletSelectImportDialog_scss_1.default.title },
              title
            ),
            react_1.default.createElement(
              'div',
              {
                className: WalletSelectImportDialog_scss_1.default.description,
              },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages.description,
              })
            ),
            react_1.default.createElement('hr', {
              className: WalletSelectImportDialog_scss_1.default.separatorTop,
            })
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                WalletSelectImportDialog_scss_1.default.walletsContainer,
            },
            walletsWithNames.map((wallet, index) => {
              const isDuplicate = previousWalletId === wallet.id;
              const rowClasses = (0, classnames_1.default)([
                WalletSelectImportDialog_scss_1.default.walletsRow,
                'namedWalletsRow',
              ]);
              const walletRow = react_1.default.createElement(
                'div',
                { className: rowClasses, key: `${wallet.id}-${wallet.index}` },
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsCounter,
                  },
                  !isDuplicate && `${rowNumber}.`
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsInputField,
                  },
                  this.getInlineEditingSmallInput(
                    wallet,
                    intl.formatMessage(
                      global_messages_1.default.invalidWalletName
                    ),
                    intl.formatMessage(messages.walletName),
                    nameValidator,
                    onWalletNameChange
                  )
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsStatus,
                  },
                  this.getWalletStatus(wallet)
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsStatusIcon,
                  },
                  this.getWalletStatusIcon(wallet, index)
                )
              );
              if (!isDuplicate) {
                previousWalletId = wallet.id;
                rowNumber++;
              }
              return walletRow;
            }),
            !!walletsWithoutNames.length &&
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletSelectImportDialog_scss_1.default.unnamedWalletsTitle,
                },
                !!walletsWithNames.length &&
                  react_1.default.createElement('hr', {
                    className:
                      WalletSelectImportDialog_scss_1.default.separatorMiddle,
                  }),
                react_1.default.createElement(
                  'p',
                  null,
                  intl.formatMessage(messages.unnamedWalletsTitle)
                )
              ),
            walletsWithoutNames.map((wallet, index) => {
              const isDuplicate = previousWalletId === wallet.id;
              const rowClasses = (0, classnames_1.default)([
                WalletSelectImportDialog_scss_1.default.walletsRow,
                'unnamedWalletsRow',
              ]);
              const walletRow = react_1.default.createElement(
                'div',
                { className: rowClasses, key: `${wallet.id}-${wallet.index}` },
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsCounter,
                  },
                  !isDuplicate && `${rowNumber}.`
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsInputField,
                  },
                  !wallet.name
                    ? react_1.default.createElement(
                        PopOver_1.PopOver,
                        {
                          content: intl.formatMessage(
                            messages.enterWalletNameTooltip
                          ),
                        },
                        this.getInlineEditingSmallInput(
                          wallet,
                          intl.formatMessage(
                            global_messages_1.default.invalidWalletName
                          ),
                          intl.formatMessage(messages.notFound),
                          nameValidator,
                          onWalletNameChange
                        )
                      )
                    : react_1.default.createElement(
                        react_1.default.Fragment,
                        null,
                        this.getInlineEditingSmallInput(
                          wallet,
                          intl.formatMessage(
                            global_messages_1.default.invalidWalletName
                          ),
                          intl.formatMessage(messages.notFound),
                          nameValidator,
                          onWalletNameChange
                        )
                      )
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsStatus,
                  },
                  this.getWalletStatus(wallet)
                ),
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletSelectImportDialog_scss_1.default.walletsStatusIcon,
                  },
                  this.getWalletStatusIcon(wallet, index)
                )
              );
              if (!isDuplicate) {
                previousWalletId = wallet.id;
                rowNumber++;
              }
              return walletRow;
            })
          ),
          react_1.default.createElement(
            'div',
            { className: WalletSelectImportDialog_scss_1.default.action },
            react_1.default.createElement(Button_1.Button, {
              className: buttonClasses,
              disabled: isDisabled,
              label: buttonLabel,
              onClick: onContinue,
              skin: ButtonSkin_1.ButtonSkin,
            }),
            react_1.default.createElement(
              'div',
              null,
              react_1.default.createElement(Link_1.Link, {
                className:
                  WalletSelectImportDialog_scss_1.default.learnMoreLink,
                onClick: onLinkClick,
                label: linkLabel,
                skin: LinkSkin_1.LinkSkin,
              }),
              react_1.default.createElement(Link_1.Link, {
                className:
                  WalletSelectImportDialog_scss_1.default.closeWindowLink,
                onClick: onClose,
                label: intl.formatMessage(messages.closeWindow),
                skin: LinkSkin_1.LinkSkin,
                hasIconAfter: false,
              })
            )
          )
        )
      )
    );
  }
};
WalletSelectImportDialog = __decorate(
  [mobx_react_1.observer],
  WalletSelectImportDialog
);
exports.default = WalletSelectImportDialog;
//# sourceMappingURL=WalletSelectImportDialog.js.map
