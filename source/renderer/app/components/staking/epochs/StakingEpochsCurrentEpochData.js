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
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const StakingEpochsNoData_1 = __importDefault(require('./StakingEpochsNoData'));
const StakingEpochsDataTable_1 = __importDefault(
  require('./StakingEpochsDataTable')
);
const helpers_1 = require('./helpers');
const StakingEpochs_scss_1 = __importDefault(require('./StakingEpochs.scss'));
const messages = (0, react_intl_1.defineMessages)({
  tableHeaderPool: {
    id: 'staking.epochs.currentEpoch.tableHeader.pool',
    defaultMessage: '!!!Stake pool',
    description: 'Table header "Stake pool" label on staking epochs page',
  },
  tableHeaderSlotsElected: {
    id: 'staking.epochs.currentEpoch.tableHeader.slotsElected',
    defaultMessage: '!!!Slots elected',
    description: 'Table header "Slots elected" label on staking epochs page',
  },
});
let StakingEpochsCurrentEpochData = class StakingEpochsCurrentEpochData extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    isLoading: false,
  };
  constructor() {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1-2 arguments, but got 0.
    super();
    this.state = {
      currentEpochDataOrder: 'desc',
      currentEpochDataSortBy: 'pool',
    };
  }
  handleDataSort = (newSortBy) => {
    const { currentEpochDataOrder, currentEpochDataSortBy } = this.state;
    let newOrder;
    if (currentEpochDataSortBy === newSortBy) {
      newOrder = currentEpochDataOrder === 'asc' ? 'desc' : 'asc';
    } else {
      newOrder = 'desc';
    }
    this.setState({
      currentEpochDataSortBy: newSortBy,
      currentEpochDataOrder: newOrder,
    });
  };
  render() {
    const { currentEpochData, isLoading } = this.props;
    const { currentEpochDataOrder, currentEpochDataSortBy } = this.state;
    const { intl } = this.context;
    const noData = (0, helpers_1.noDataExisting)(isLoading, currentEpochData);
    const hasData = (0, helpers_1.hasDataExisting)(isLoading, currentEpochData);
    const tableHeaders = (0, helpers_1.getTableHeadersForCurrentEpoch)(
      intl,
      messages
    );
    let sortedData = null;
    if (noData) {
      return react_1.default.createElement(StakingEpochsNoData_1.default, null);
    }
    if (hasData) {
      sortedData = (0, helpers_1.sortData)(
        currentEpochData,
        currentEpochDataOrder,
        currentEpochDataSortBy
      );
    }
    const tableBody = react_1.default.createElement(
      'tbody',
      null,
      (0, lodash_1.map)(sortedData, (row, key) => {
        const poolTicker = (0, lodash_1.get)(row, ['pool', 'ticker'], '');
        const poolName = (0, lodash_1.get)(row, ['pool', 'name'], '');
        const slotsElected = (0, lodash_1.get)(row, 'slotsElected', [0]);
        return react_1.default.createElement(
          'tr',
          { key: key },
          react_1.default.createElement(
            'td',
            null,
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(
                'span',
                { className: StakingEpochs_scss_1.default.stakePoolReference },
                '[',
                poolTicker,
                ']'
              ),
              ' ',
              poolName
            )
          ),
          react_1.default.createElement(
            'td',
            null,
            react_1.default.createElement(
              'span',
              { className: StakingEpochs_scss_1.default.mediumText },
              `${slotsElected[0]}%`
            )
          )
        );
      })
    );
    return react_1.default.createElement(
      StakingEpochsDataTable_1.default,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      {
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        tableHeaders: tableHeaders,
        tableBody: tableBody,
        order: currentEpochDataOrder,
        sortBy: currentEpochDataSortBy,
        handleDataSort: this.handleDataSort,
      }
    );
  }
};
StakingEpochsCurrentEpochData = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [])],
  StakingEpochsCurrentEpochData
);
exports.default = StakingEpochsCurrentEpochData;
//# sourceMappingURL=StakingEpochsCurrentEpochData.js.map
