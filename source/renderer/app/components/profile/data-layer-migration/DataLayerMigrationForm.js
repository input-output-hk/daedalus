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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const react_intl_1 = require('react-intl');
const DataLayerMigrationForm_scss_1 = __importDefault(
  require('./DataLayerMigrationForm.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'profile.dataLayerMigration.title',
    defaultMessage: '!!!Wallet data migration',
    description: 'Title for the Data Layer Migration screen.',
  },
  content1: {
    id: 'profile.dataLayerMigration.content1',
    defaultMessage:
      '!!!You have installed a version of Daedalus that changes how wallet data is stored and managed. Because of this, all of your wallets need to be restored and synchronized with the complete history of the Cardano blockchain.',
    description: 'Content for the Data Layer Migration screen.',
  },
  content2: {
    id: 'profile.dataLayerMigration.content2',
    defaultMessage:
      '!!!This is an automatic process and does not require any action on your behalf.',
    description: 'Content for the Data Layer Migration screen.',
  },
  content3: {
    id: 'profile.dataLayerMigration.content3',
    defaultMessage:
      '!!!Your transaction history and used addresses will appear in your wallets as they are recovered during the restoration process. Addresses that were not used will not be recovered because they are not recorded on the blockchain. If funds were sent to those addresses you will receive the funds and those addresses will appear in your wallet.',
    description: 'Content for the Data Layer Migration screen.',
  },
  submitLabel: {
    id: 'profile.dataLayerMigration.submitLabel',
    defaultMessage: '!!!Start migration',
    description: 'Submit label for the Data Layer Migration screen.',
  },
});
let DataLayerMigrationForm = class DataLayerMigrationForm extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  submit = () => {
    this.props.onSubmit();
  };
  render() {
    const { intl } = this.context;
    const { error } = this.props;
    return react_1.default.createElement(
      'div',
      { className: DataLayerMigrationForm_scss_1.default.component },
      react_1.default.createElement(
        'h1',
        { className: DataLayerMigrationForm_scss_1.default.title },
        intl.formatMessage(messages.title)
      ),
      react_1.default.createElement(
        'p',
        { className: DataLayerMigrationForm_scss_1.default.content1 },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.content1,
        })
      ),
      react_1.default.createElement(
        'p',
        { className: DataLayerMigrationForm_scss_1.default.content2 },
        intl.formatMessage(messages.content2)
      ),
      react_1.default.createElement(
        'p',
        { className: DataLayerMigrationForm_scss_1.default.content3 },
        intl.formatMessage(messages.content3)
      ),
      error &&
        react_1.default.createElement(
          'p',
          { className: DataLayerMigrationForm_scss_1.default.error },
          intl.formatMessage(error)
        ),
      react_1.default.createElement(Button_1.Button, {
        className: DataLayerMigrationForm_scss_1.default.submitButton,
        label: intl.formatMessage(messages.submitLabel),
        onClick: this.submit,
        skin: ButtonSkin_1.ButtonSkin,
      })
    );
  }
};
DataLayerMigrationForm = __decorate(
  [mobx_react_1.observer],
  DataLayerMigrationForm
);
exports.default = DataLayerMigrationForm;
//# sourceMappingURL=DataLayerMigrationForm.js.map
