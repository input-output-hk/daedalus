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
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const UndelegateWalletSuccessDialog_scss_1 = __importDefault(
  require('./UndelegateWalletSuccessDialog.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/untada.... Remove this comment to see the full error message
const untada_inline_svg_1 = __importDefault(
  require('../../../assets/images/untada.inline.svg')
);
const humanizeDurationByLocale_1 = __importDefault(
  require('../../../utils/humanizeDurationByLocale')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.settings.undelegate.result.dialog.title',
    defaultMessage: '!!!Wallet undelegated',
    description: 'Title for the "Undelegate Result" dialog.',
  },
  description1: {
    id: 'wallet.settings.undelegate.result.dialog.description1',
    defaultMessage:
      '!!!The stake from your wallet <strong>{walletName}</strong> is no longer delegated and you will soon stop earning rewards for this wallet.',
    description: 'Description 1 for the "Undelegate Result" dialog.',
  },
  description2: {
    id: 'wallet.settings.undelegate.result.dialog.description2',
    defaultMessage:
      '!!!Your new delegation preferences are now posted on the blockchain <strong>and will take effect after both the current and next Cardano epochs have completed in {timeUntilNextEpochStart}</strong>. During this time, your previous delegation preferences are still active.',
    description: 'Description 2 for the "Undelegate Result" dialog.',
  },
});
let UndelegateWalletSuccessDialog = class UndelegateWalletSuccessDialog extends react_1.Component {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler = null;
  state = {
    timeUntilNextEpochStart: 0,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidMount() {
    this.updateTimeUntilNextEpochStart();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilNextEpochStart(),
      stakingConfig_1.EPOCH_COUNTDOWN_INTERVAL
    );
  }
  updateTimeUntilNextEpochStart = () => {
    const { futureEpochStartTime } = this.props;
    const timeUntilNextEpochStart = Math.max(
      0,
      new Date(futureEpochStartTime).getTime() - new Date().getTime()
    );
    this.setState({
      timeUntilNextEpochStart,
    });
  };
  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }
  render() {
    const { intl } = this.context;
    const { walletName, onClose, currentLocale } = this.props;
    const actions = [
      {
        label: intl.formatMessage(global_messages_1.default.close),
        onClick: onClose,
        primary: true,
      },
    ];
    const timeUntilNextEpochStart = (0, humanizeDurationByLocale_1.default)(
      this.state.timeUntilNextEpochStart,
      currentLocale
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        className: UndelegateWalletSuccessDialog_scss_1.default.dialog,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            UndelegateWalletSuccessDialog_scss_1.default.sadLogoContainer,
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: untada_inline_svg_1.default,
          className: UndelegateWalletSuccessDialog_scss_1.default.sadLogoIcon,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: UndelegateWalletSuccessDialog_scss_1.default.description },
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description1,
            values: {
              walletName,
            },
          })
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description2,
            values: {
              timeUntilNextEpochStart,
            },
          })
        )
      )
    );
  }
};
UndelegateWalletSuccessDialog = __decorate(
  [mobx_react_1.observer],
  UndelegateWalletSuccessDialog
);
exports.default = UndelegateWalletSuccessDialog;
//# sourceMappingURL=UndelegateWalletSuccessDialog.js.map
