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
const lodash_1 = require('lodash');
const DelegationCenterHeader_scss_1 = __importDefault(
  require('./DelegationCenterHeader.scss')
);
const CountdownWidget_1 = __importDefault(
  require('../../widgets/CountdownWidget')
);
const humanizeDurationByLocale_1 = __importDefault(
  require('../../../utils/humanizeDurationByLocale')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const helpers_1 = require('./helpers');
const messages = (0, react_intl_1.defineMessages)({
  epoch: {
    id: 'staking.delegationCenter.epoch',
    defaultMessage: '!!!Epoch',
    description: 'Headline for the Delegation center.',
  },
  currentSlot: {
    id: 'staking.delegationCenter.currentSlot',
    defaultMessage: '!!!Current slot',
    description: 'Headline for the Delegation center.',
  },
  totalSlots: {
    id: 'staking.delegationCenter.totalSlots',
    defaultMessage: '!!!Total slots',
    description: 'Headline for the Delegation center.',
  },
  headingLeft: {
    id: 'staking.delegationCenter.headingLeft',
    defaultMessage: '!!!Next Cardano epoch starts in',
    description: 'Headline for the Delegation center.',
  },
  headingRight: {
    id: 'staking.delegationCenter.headingRight',
    defaultMessage: '!!!Current Cardano epoch',
    description: 'Headline for the Delegation center.',
  },
  description: {
    id: 'staking.delegationCenter.description',
    defaultMessage:
      '!!!Changes to delegation preferences will take effect after both the current and next Cardano epochs have completed. Epochs on the Incentivized Testnet last one day. Any changes made now will take effect in {timeUntilFutureEpoch}.',
    description: 'Delegation description for the Delegation center.',
  },
});
let DelegationCenterHeader = class DelegationCenterHeader extends react_1.Component {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  intervalHandler = null;
  state = {
    timeUntilFutureEpoch: 0,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidMount() {
    this.configureUpdateTimer();
  }
  configureUpdateTimer = () => {
    this.updateTimeUntilFutureEpoch();
    this.intervalHandler = setInterval(
      () => this.updateTimeUntilFutureEpoch(),
      stakingConfig_1.EPOCH_COUNTDOWN_INTERVAL
    );
  };
  updateTimeUntilFutureEpoch = () => {
    const { futureEpoch } = this.props;
    if (!futureEpoch) return;
    const { epochStart } = futureEpoch;
    if (epochStart) {
      const timeUntilFutureEpoch = Math.max(
        0,
        new Date(epochStart).getTime() - new Date().getTime()
      );
      this.setState({
        timeUntilFutureEpoch,
      });
    }
  };
  componentWillUnmount() {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
    }
  }
  generateCurrentEpochPanels = (epoch, slots, totalSlots) => {
    const { intl } = this.context;
    const epochLabel = intl.formatMessage(messages.epoch);
    const currentSlotLabel = intl.formatMessage(messages.currentSlot);
    const totalSlotsLabel = intl.formatMessage(messages.totalSlots);
    const labels = [epochLabel, currentSlotLabel, totalSlotsLabel];
    const values = [epoch, slots, totalSlots];
    const keys = ['epoch', 'slots', 'totalSlots'];
    return labels.map((label, index) =>
      react_1.default.createElement(
        react_1.Fragment,
        { key: keys[index] },
        (0, helpers_1.generateFieldPanel)(labels, values, index)
      )
    );
  };
  render() {
    const { intl } = this.context;
    const { networkTip, epochLength, nextEpoch, currentLocale } = this.props;
    const epoch = (0, lodash_1.get)(networkTip, 'epoch', '-');
    const nextEpochStart = (0, lodash_1.get)(nextEpoch, 'epochStart', '');
    const nextEpochNumber = (0, lodash_1.get)(nextEpoch, 'epochNumber', 0);
    const slot = (0, lodash_1.get)(networkTip, 'slot', '-');
    const headingFirst = intl.formatMessage(messages.headingRight);
    const headingSecond = intl.formatMessage(messages.headingLeft);
    const timeUntilFutureEpoch = (0, humanizeDurationByLocale_1.default)(
      this.state.timeUntilFutureEpoch,
      currentLocale
    );
    const description = intl.formatMessage(messages.description, {
      timeUntilFutureEpoch,
    });
    if (!epochLength) {
      return null;
    }
    const fieldPanels = this.generateCurrentEpochPanels(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number | "-"' is not assignable ... Remove this comment to see the full error message
      epoch,
      slot,
      epochLength
    );
    const showNextEpochCountdown = nextEpochNumber > 0;
    return react_1.default.createElement(
      'div',
      { className: DelegationCenterHeader_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: DelegationCenterHeader_scss_1.default.mainContent },
        react_1.default.createElement(
          'div',
          { className: DelegationCenterHeader_scss_1.default.mainContainer },
          react_1.default.createElement(
            'div',
            {
              className:
                DelegationCenterHeader_scss_1.default.countdownContainer,
            },
            react_1.default.createElement(
              'div',
              { className: DelegationCenterHeader_scss_1.default.heading },
              headingFirst
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  DelegationCenterHeader_scss_1.default.epochsContainer,
              },
              react_1.default.createElement(
                'div',
                { className: DelegationCenterHeader_scss_1.default.epochs },
                fieldPanels
              )
            )
          ),
          showNextEpochCountdown &&
            react_1.default.createElement(
              'div',
              {
                className:
                  DelegationCenterHeader_scss_1.default.countdownContainer,
              },
              react_1.default.createElement(
                'div',
                { className: DelegationCenterHeader_scss_1.default.heading },
                headingSecond
              ),
              react_1.default.createElement(CountdownWidget_1.default, {
                startDateTime: nextEpochStart,
                format: 'DD-HH-mm-ss',
              })
            )
        ),
        react_1.default.createElement(
          'div',
          { className: DelegationCenterHeader_scss_1.default.description },
          description
        )
      )
    );
  }
};
DelegationCenterHeader = __decorate(
  [mobx_react_1.observer],
  DelegationCenterHeader
);
exports.default = DelegationCenterHeader;
//# sourceMappingURL=DelegationCenterHeader.js.map
