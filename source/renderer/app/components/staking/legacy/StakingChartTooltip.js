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
const moment_1 = __importDefault(require('moment'));
const StakingChartTooltipItem_1 = __importDefault(
  require('./StakingChartTooltipItem')
);
const StakingChartTooltip_scss_1 = __importDefault(
  require('./StakingChartTooltip.scss')
);
const dateFormat = 'YYYY-MM-DD-HH:mm';
const messages = (0, react_intl_1.defineMessages)({
  slot: {
    id: 'staking.chart.tooltip.slot.label',
    defaultMessage: '!!!slot',
    description: '"slot" label on staking chart tooltip.',
  },
  transactions: {
    id: 'staking.chart.tooltip.transactions.label',
    defaultMessage: '!!!transactions',
    description: '"transactions" label on staking chart tooltip.',
  },
  mpcPhase: {
    id: 'staking.chart.tooltip.mpc.phase.label',
    defaultMessage: '!!!MPC phase',
    description: '"MPC phase" label on staking chart tooltip.',
  },
  commitments: {
    id: 'staking.chart.tooltip.commitments.label',
    defaultMessage: '!!!commitments',
    description: '"commitments" label on staking chart tooltip.',
  },
  openings: {
    id: 'staking.chart.tooltip.openings.label',
    defaultMessage: '!!!openings',
    description: '"openings" label on staking chart tooltip.',
  },
  shares: {
    id: 'staking.chart.tooltip.shares.label',
    defaultMessage: '!!!shares',
    description: '"shares" label on staking chart tooltip.',
  },
});
let StakingChartTooltip = class StakingChartTooltip extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      slot,
      time,
      hash,
      numberOfTransactions,
      mpcPhase,
      commitments,
      openings,
      shares,
    } = this.props;
    return react_1.default.createElement(
      'div',
      { className: StakingChartTooltip_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakingChartTooltip_scss_1.default.slot },
        `#${slot}`,
        ' ',
        intl.formatMessage(messages.slot)
      ),
      react_1.default.createElement(
        'div',
        { className: StakingChartTooltip_scss_1.default.date },
        (0, moment_1.default)(time).format(dateFormat)
      ),
      react_1.default.createElement(
        'div',
        { className: StakingChartTooltip_scss_1.default.hash },
        hash
      ),
      react_1.default.createElement(StakingChartTooltipItem_1.default, {
        key: 'transactions',
        label: intl.formatMessage(messages.transactions),
        value: numberOfTransactions.toString(),
      }),
      react_1.default.createElement(StakingChartTooltipItem_1.default, {
        key: 'mpcPhase',
        label: intl.formatMessage(messages.mpcPhase),
        value: mpcPhase,
      }),
      react_1.default.createElement(StakingChartTooltipItem_1.default, {
        key: 'commitments',
        label: intl.formatMessage(messages.commitments),
        value: commitments,
      }),
      react_1.default.createElement(StakingChartTooltipItem_1.default, {
        key: 'openings',
        label: intl.formatMessage(messages.openings),
        value: openings,
      }),
      react_1.default.createElement(StakingChartTooltipItem_1.default, {
        key: 'shares',
        label: intl.formatMessage(messages.shares),
        value: shares,
      })
    );
  }
};
StakingChartTooltip = __decorate([mobx_react_1.observer], StakingChartTooltip);
exports.default = StakingChartTooltip;
//# sourceMappingURL=StakingChartTooltip.js.map
