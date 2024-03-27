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
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const DelegationSteps_scss_1 = __importDefault(
  require('./DelegationSteps.scss')
);
const DelegationStepsSuccessDialog_scss_1 = __importDefault(
  require('./DelegationStepsSuccessDialog.scss')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
const tada_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/tada-ic.inline.svg')
);
const humanizeDurationByLocale_1 = __importDefault(
  require('../../../utils/humanizeDurationByLocale')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.delegationSetup.success.step.dialog.title',
    defaultMessage: '!!!Wallet Delegated',
    description:
      'Title "Wallet Delegated" on the delegation setup "success" step dialog.',
  },
  descriptionLine1: {
    id: 'staking.delegationSetup.success.step.dialog.description.line1',
    defaultMessage:
      '!!!The stake from your wallet <span>{delegatedWalletName}</span> is now delegated to the <span>[{delegatedStakePoolTicker}] {delegatedStakePoolName}</span> stake pool.',
    description:
      'Description "line 1" on the delegation setup "success" step dialog.',
  },
  descriptionLine2: {
    id: 'staking.delegationSetup.success.step.dialog.description.line2',
    defaultMessage:
      '!!!Your new delegation preferences are now posted on the Cardano blockchain. <strong>These preferences will take effect after both the current and the next Cardano epochs have completed in {timeUntilNextEpochStart}.</strong> During this time, your previous delegation preferences remain active.',
    description:
      'Description "line 2" on the delegation setup "success" step dialog.',
  },
  closeButtonLabel: {
    id: 'staking.delegationSetup.success.step.dialog.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      'Label for Close button on the delegation setup "success" step dialog.',
  },
});
let DelegationStepsSuccessDialog = class DelegationStepsSuccessDialog extends react_1.Component {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler = null;
  state = {
    timeUntilNextEpochStart: 0,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidMount() {
    this.configureUpdateTimer();
  }
  configureUpdateTimer = () => {
    this.updateTimeUntilNextEpochStart();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilNextEpochStart(),
      stakingConfig_1.EPOCH_COUNTDOWN_INTERVAL
    );
  };
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
    const {
      delegatedWallet,
      delegatedStakePool,
      currentLocale,
      onClose,
    } = this.props;
    const actions = [
      {
        className: 'closeButton',
        label: intl.formatMessage(messages.closeButtonLabel),
        onClick: onClose,
        primary: true,
      },
    ];
    const dialogClassName = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.delegationSteps,
      DelegationStepsSuccessDialog_scss_1.default
        .delegationStepsSuccessDialogWrapper,
    ]);
    const contentClasses = (0, classnames_1.default)([
      DelegationSteps_scss_1.default.content,
      DelegationStepsSuccessDialog_scss_1.default.content,
    ]);
    const delegatedWalletName = (0, lodash_1.get)(delegatedWallet, 'name');
    const delegatedStakePoolName = (0, lodash_1.get)(
      delegatedStakePool,
      'name'
    );
    const delegatedStakePoolTicker = (0, lodash_1.get)(
      delegatedStakePool,
      'ticker'
    );
    const timeUntilNextEpochStart = (0, humanizeDurationByLocale_1.default)(
      this.state.timeUntilNextEpochStart,
      currentLocale
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        className: dialogClassName,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'div',
        { className: contentClasses },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: tada_ic_inline_svg_1.default,
          className: DelegationStepsSuccessDialog_scss_1.default.tadaImage,
        }),
        react_1.default.createElement(
          'div',
          {
            className: DelegationStepsSuccessDialog_scss_1.default.description1,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.descriptionLine1,
            values: {
              delegatedWalletName,
              delegatedStakePoolTicker,
              delegatedStakePoolName,
            },
          })
        ),
        react_1.default.createElement(
          'div',
          {
            className: DelegationStepsSuccessDialog_scss_1.default.description2,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.descriptionLine2,
            values: {
              timeUntilNextEpochStart,
            },
          })
        )
      )
    );
  }
};
DelegationStepsSuccessDialog = __decorate(
  [mobx_react_1.observer],
  DelegationStepsSuccessDialog
);
exports.default = DelegationStepsSuccessDialog;
//# sourceMappingURL=DelegationStepsSuccessDialog.js.map
