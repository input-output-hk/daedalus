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
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const WalletImportFileDialog_scss_1 = __importDefault(
  require('./WalletImportFileDialog.scss')
);
const RadioSet_1 = __importDefault(require('../../widgets/RadioSet'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const pen_inline_svg_1 = __importDefault(
  require('../../../assets/images/pen.inline.svg')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const walletExportTypes_1 = require('../../../types/walletExportTypes');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.import.file.dialog.title',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets dialog title',
  },
  description: {
    id: 'wallet.import.file.dialog.description',
    defaultMessage:
      '!!!<p>This feature enables you to import wallets from the production version of Daedalus, or from the Daedalus state directory. </p> <p>If you don’t have the complete state directory, then you will need either the ‘Secrets’ or ‘Secrets-1.0’ folder containing the ‘secret.key’ file to be able to import a wallet, although without the complete state directory Daedalus won’t be able to detect your wallet names. </p> <p>If you don’t have either the ‘Secrets’ or the ‘Secrets-1.0’ folder containing the ‘secret.key’ file, then you cannot import wallets using this feature.</p>',
    description:
      '<p>This feature enables you to import wallets from the production version of Daedalus, or from the Daedalus state directory. </p> <p>If you don’t have the complete state directory, then you will need either the ‘Secrets’ or ‘Secrets-1.0’ folder containing the ‘secret.key’ file to be able to import a wallet, although without the complete state directory Daedalus won’t be able to detect your wallet names. </p> <p>If you don’t have either the ‘Secrets’ or the ‘Secrets-1.0’ folder containing the ‘secret.key’ file, then you cannot import wallets using this feature.</p>',
  },
  stateFolderLabel: {
    id: 'wallet.import.file.dialog.stateFolderLabel',
    defaultMessage: '!!!Select Daedalus state folder:',
    description: 'Select Daedalus state folder:',
  },
  secretFileLabel: {
    id: 'wallet.import.file.dialog.secretFileLabel',
    defaultMessage: "!!!Select Daedalus 'secret.key' file:",
    description: "Select Daedalus 'secret.key' file:",
  },
  buttonLabel: {
    id: 'wallet.import.file.dialog.buttonLabel',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets',
  },
  stateDirNoWallets: {
    id: 'wallet.import.file.dialog.stateDirNoWallets',
    defaultMessage:
      '!!!No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
    description:
      'No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
  },
  secretFileNoWallets: {
    id: 'wallet.import.file.dialog.secretFileNoWallets',
    defaultMessage:
      '!!!No wallets found. Make sure you have selected a valid `secret.key` file.',
    description:
      'No wallets found. Make sure you have selected a valid `secret.key` file.',
  },
  linkLabel: {
    id: 'wallet.import.file.dialog.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'wallet.import.file.dialog.linkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900000623463',
    description: '"Learn more" link URL on the wallet import file dialog',
  },
  importFromLabel: {
    id: 'wallet.import.file.dialog.importFromLabel',
    defaultMessage: '!!!Import from:',
    description: 'Import from:',
  },
  stateDirOptionLabel: {
    id: 'wallet.import.file.dialog.stateDirOptionLabel',
    defaultMessage: '!!!Daedalus state directory',
    description: 'Daedalus state directory',
  },
  secretFileOptionLabel: {
    id: 'wallet.import.file.dialog.secretFileOptionLabel',
    defaultMessage: "!!!Daedalus 'secret.key' file",
    description: "Daedalus 'secret.key' file",
  },
});
let WalletImportFileDialog = class WalletImportFileDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    importFrom: walletExportTypes_1.ImportFromOptions.STATE_DIR,
  };
  importPathInput;
  componentDidMount() {
    this.props.onOpen();
  }
  onSetImportFromOption = (importFrom) => {
    if (this.state.importFrom !== importFrom) {
      this.props.onResetExportSourcePath();
      this.setState({
        importFrom,
      });
    }
  };
  get input() {
    const fallbackInput = {
      blur: () => {},
      focus: () => {},
    };
    return (0, lodash_1.get)(
      this,
      'importPathInput.inputElement.current',
      fallbackInput
    );
  }
  isImportFromStateDir = (importFrom) =>
    importFrom === walletExportTypes_1.ImportFromOptions.STATE_DIR;
  isImportFromSecretFile = (importFrom) =>
    importFrom === walletExportTypes_1.ImportFromOptions.SECRET_FILE;
  render() {
    const { intl } = this.context;
    const { importFrom } = this.state;
    const {
      exportErrors,
      isSubmitting,
      onContinue,
      onClose,
      onOpenExternalLink,
      onSelectExportSourcePath,
      exportSourcePath,
      defaultExportSourcePath,
    } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = react_1.default.createElement(
      react_intl_1.FormattedHTMLMessage,
      { ...messages.description }
    );
    const stateFolderLabel = intl.formatMessage(messages.stateFolderLabel);
    const secretFileLabel = intl.formatMessage(messages.secretFileLabel);
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.buttonLabel)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const noWalletError = intl.formatMessage(
      messages[`${importFrom}NoWallets`]
    );
    const onLinkClick = () =>
      onOpenExternalLink(intl.formatMessage(messages.linkUrl));
    const error = exportErrors !== '';
    const inputClasses = (0, classnames_1.default)([
      WalletImportFileDialog_scss_1.default.stateFolderInput,
      error ? WalletImportFileDialog_scss_1.default.error : null,
    ]);
    const buttonClasses = (0,
    classnames_1.default)(WalletImportFileDialog_scss_1.default.actionButton, [
      isSubmitting ||
      error ||
      (this.isImportFromSecretFile(importFrom) && !exportSourcePath)
        ? WalletImportFileDialog_scss_1.default.disabled
        : null,
    ]);
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: WalletImportFileDialog_scss_1.default.dialog,
        closeOnOverlayClick: false,
        onClose: onClose,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onRequestClose: onClose,
        shouldCloseOnOverlayClick: false,
        shouldCloseOnEsc: false,
        ariaHideApp: false,
        defaultThemeOverrides: true,
      },
      react_1.default.createElement(
        'div',
        { className: WalletImportFileDialog_scss_1.default.component },
        react_1.default.createElement(DialogCloseButton_1.default, {
          icon: close_cross_thin_inline_svg_1.default,
          className: WalletImportFileDialog_scss_1.default.closeButton,
          onClose: onClose,
        }),
        react_1.default.createElement(
          'div',
          { className: WalletImportFileDialog_scss_1.default.content },
          react_1.default.createElement(
            'div',
            { className: WalletImportFileDialog_scss_1.default.title },
            title
          ),
          react_1.default.createElement(
            'div',
            { className: WalletImportFileDialog_scss_1.default.description },
            description
          ),
          react_1.default.createElement(
            'div',
            { className: WalletImportFileDialog_scss_1.default.radioButtons },
            react_1.default.createElement(RadioSet_1.default, {
              label: intl.formatMessage(messages.importFromLabel),
              items: Object.keys(walletExportTypes_1.ImportFromOptions).map(
                (key) => {
                  const importFromOption =
                    walletExportTypes_1.ImportFromOptions[key];
                  return {
                    key: importFromOption,
                    label: intl.formatMessage(
                      messages[`${importFromOption}OptionLabel`]
                    ),
                    selected: importFrom === importFromOption,
                    onChange: () =>
                      this.onSetImportFromOption(importFromOption),
                  };
                }
              ),
              verticallyAligned: true,
            })
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                WalletImportFileDialog_scss_1.default.stateFolderContainer,
            },
            react_1.default.createElement(
              'p',
              {
                className:
                  WalletImportFileDialog_scss_1.default.stateFolderLabel,
              },
              this.isImportFromStateDir(importFrom)
                ? stateFolderLabel
                : secretFileLabel
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  WalletImportFileDialog_scss_1.default.stateFolderInputWrapper,
              },
              react_1.default.createElement(Input_1.Input, {
                type: 'text',
                className: inputClasses,
                ref: (input) => {
                  this.importPathInput = input;
                },
                skin: InputSkin_1.InputSkin,
                value:
                  exportSourcePath ||
                  (this.isImportFromStateDir(importFrom)
                    ? defaultExportSourcePath
                    : ''),
                placeholder: this.isImportFromStateDir(importFrom)
                  ? defaultExportSourcePath
                  : 'secret.key',
                readOnly: true,
              }),
              react_1.default.createElement(Button_1.Button, {
                className:
                  WalletImportFileDialog_scss_1.default
                    .selectStateDirectoryButton,
                onClick: () =>
                  onSelectExportSourcePath({
                    importFrom,
                  }),
                label: react_1.default.createElement(
                  react_svg_inline_1.default,
                  {
                    svg: pen_inline_svg_1.default,
                    className: WalletImportFileDialog_scss_1.default.penIcon,
                  }
                ),
                skin: ButtonSkin_1.ButtonSkin,
              })
            ),
            error &&
              react_1.default.createElement(
                'p',
                {
                  className:
                    WalletImportFileDialog_scss_1.default.noWalletError,
                },
                noWalletError
              )
          ),
          react_1.default.createElement(
            'div',
            { className: WalletImportFileDialog_scss_1.default.action },
            react_1.default.createElement(Button_1.Button, {
              className: buttonClasses,
              disabled:
                isSubmitting ||
                error ||
                (this.isImportFromSecretFile(importFrom) && !exportSourcePath),
              label: buttonLabel,
              onClick: onContinue,
              skin: ButtonSkin_1.ButtonSkin,
            })
          ),
          react_1.default.createElement(Link_1.Link, {
            className: WalletImportFileDialog_scss_1.default.learnMoreLink,
            onClick: onLinkClick,
            label: linkLabel,
            skin: LinkSkin_1.LinkSkin,
          })
        )
      )
    );
  }
};
WalletImportFileDialog = __decorate(
  [mobx_react_1.observer],
  WalletImportFileDialog
);
exports.default = WalletImportFileDialog;
//# sourceMappingURL=WalletImportFileDialog.js.map
