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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
// import { Input } from '@react-polymorph/components/Input';
// import { InputSkin } from '@react-polymorph/skins/simple/InputSkin';
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ExportWalletToFileDialog.scs... Remove this comment to see the full error message
const ExportWalletToFileDialog_scss_1 = __importDefault(
  require('./ExportWalletToFileDialog.scss')
);
const timingConfig_1 = require('../../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'wallet.settings.exportToFile.dialog.headline',
    defaultMessage: '!!!Export Wallet',
    description: 'headline for "export wallet to file" dialog.',
  },
  introduction: {
    id: 'wallet.settings.exportToFile.dialog.introduction',
    defaultMessage:
      '!!!You are exporting <strong>{walletName}</strong> to a file.',
    description: 'headline for "export wallet to file" dialog.',
  },
  exportButtonLabel: {
    id: 'wallet.settings.exportToFile.dialog.submit.label',
    defaultMessage: '!!!Export',
    description: 'Label for export wallet to file submit button.',
  }, // TODO: re-enable when we have full/readOnly exports
  // fullTabTitle: {
  //   id: 'wallet.export.choices.tab.title.full',
  //   defaultMessage: '!!!Full',
  //   description: 'Tab title "Full" on wallet export dialog.'
  // },
  // readOnlyTabTitle: {
  //   id: 'wallet.export.choices.tab.title.readOnly',
  //   defaultMessage: '!!!Read-only',
  //   description: 'Tab title "Read-only" on wallet export dialog.'
  // },
});
const EXPORT_TYPE = {
  FULL: 'full',
  READ_ONLY: 'readOnly',
};
let ExportWalletToFileDialog = class ExportWalletToFileDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  constructor(props, children) {
    super(props, children);
    this.state = {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'ExportTyp... Remove this comment to see the full error message
      exportType: EXPORT_TYPE.FULL,
    };
  }
  // onChangeExportType(exportType: ExportType) {
  //   this.setState({ exportType });
  // }
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            global_messages_1.default.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            global_messages_1.default.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            () => {
              // if (field.value === '') {
              //   return [
              //     false,
              //     this.context.intl.formatMessage(
              //       globalMessages.fieldIsRequired
              //     ),
              //   ];
              // }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: async (form) => {
        const { spendingPassword } = form.values();
        const formData = {
          exportType: this.state.exportType,
          password: spendingPassword || null,
        };
        await this.props.onSubmit(formData);
      },
    });
  };
  // handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);
  render() {
    // const { form } = this;
    const { intl } = this.context;
    const { onClose, walletName, isSubmitting, error } = this.props;
    // const { exportType } = this.state;
    const dialogClasses = (0, classnames_1.default)([
      ExportWalletToFileDialog_scss_1.default.component,
      'WalletExportDialog',
    ]);
    const actions = [
      {
        className: isSubmitting
          ? ExportWalletToFileDialog_scss_1.default.isSubmitting
          : null,
        label: intl.formatMessage(messages.exportButtonLabel),
        primary: true,
        onClick: this.submit,
      },
    ];
    // const spendingPasswordField = form.$('spendingPassword');
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.headline),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: ExportWalletToFileDialog_scss_1.default.introduction },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.introduction,
          values: {
            walletName,
          },
        })
      ),
      error &&
        react_1.default.createElement(
          'p',
          { className: ExportWalletToFileDialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
};
ExportWalletToFileDialog = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object, Object])],
  ExportWalletToFileDialog
);
exports.default = ExportWalletToFileDialog;
//# sourceMappingURL=ExportWalletToFileDialog.js.map
