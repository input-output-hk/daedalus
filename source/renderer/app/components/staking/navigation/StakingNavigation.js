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
const Navigation_1 = __importDefault(require('../../navigation/Navigation'));
const messages = (0, react_intl_1.defineMessages)({
  delegation_center: {
    id: 'staking.navigation.delegation_center',
    defaultMessage: '!!!Delegation center',
    description:
      'Label for the "Delegation" nav button in the staking navigation.',
  },
  stake_pools: {
    id: 'staking.navigation.stake_pools',
    defaultMessage: '!!!Stake pools',
    description: 'Label for the "Stake" nav button in the staking navigation.',
  },
  rewards: {
    id: 'staking.navigation.rewards',
    defaultMessage: '!!!Rewards',
    description:
      'Label for the "Rewards" nav button in the staking navigation.',
  },
  epochs: {
    id: 'staking.navigation.epochs',
    defaultMessage: '!!!Epochs',
    description: 'Label for the "Epochs" nav button in the staking navigation.',
  },
  info: {
    id: 'staking.navigation.info',
    defaultMessage: '!!!Info',
    description: 'Label for the "Info" nav button in the staking navigation.',
  },
});
let StakingNavigation = class StakingNavigation extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const {
      onNavItemClick,
      activeItem,
      isActiveNavItem,
      showInfoTab,
    } = this.props;
    const { intl } = this.context;
    const navigationItems = [
      {
        id: 'delegation-center',
        label: intl.formatMessage(messages.delegation_center),
      },
      {
        id: 'stake-pools',
        label: intl.formatMessage(messages.stake_pools),
      },
      {
        id: 'rewards',
        label: intl.formatMessage(messages.rewards),
      }, // {
      //   id: 'epochs',
      //   label: intl.formatMessage(messages.epochs),
      // },
    ];
    if (showInfoTab) {
      navigationItems.push({
        id: 'info',
        label: intl.formatMessage(messages.info),
      });
    }
    return react_1.default.createElement(Navigation_1.default, {
      activeItem: activeItem,
      isActiveNavItem: isActiveNavItem,
      onNavItemClick: onNavItemClick,
      items: navigationItems,
    });
  }
};
StakingNavigation = __decorate([mobx_react_1.observer], StakingNavigation);
exports.default = StakingNavigation;
//# sourceMappingURL=StakingNavigation.js.map
