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
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const WalletRow_1 = __importDefault(require('./WalletRow'));
const DelegationCenterBody_scss_1 = __importDefault(
  require('./DelegationCenterBody.scss')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const messages = (0, react_intl_1.defineMessages)({
  bodyTitle: {
    id: 'staking.delegationCenter.bodyTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title for the Delegation center body section.',
  },
  currentEpochTitle: {
    id: 'staking.delegationCenter.currentEpochTitle',
    defaultMessage: '!!!Now',
    description: 'Title for the Delegation current epoch.',
  },
  loadingStakePoolsMessage: {
    id: 'staking.delegationCenter.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
});
let DelegationCenterBody = class DelegationCenterBody extends react_1.Component {
  loadingSpinner;
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      wallets,
      numberOfStakePools,
      numberOfRankedStakePools,
      onDelegate,
      onUndelegate,
      getStakePoolById,
      isLoading,
      nextEpoch,
      futureEpoch,
      isListActive,
      currentTheme,
      onOpenExternalLink,
      containerClassName,
      setListActive,
      listName,
    } = this.props;
    const title = intl.formatMessage(messages.bodyTitle);
    const currentEpochTitle = intl.formatMessage(messages.currentEpochTitle);
    const nextEpochNumber = (0, lodash_1.get)(nextEpoch, 'epochNumber', 0);
    const futureEpochNumber = (0, lodash_1.get)(futureEpoch, 'epochNumber', 0);
    const loadingSpinner = react_1.default.createElement(
      LoadingSpinner_1.default,
      {
        big: true,
        ref: (component) => {
          this.loadingSpinner = component;
        },
      }
    );
    const componentClasses = (0, classnames_1.default)([
      DelegationCenterBody_scss_1.default.component,
      isLoading ? DelegationCenterBody_scss_1.default.isLoading : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      isLoading
        ? react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'p',
              null,
              intl.formatMessage(messages.loadingStakePoolsMessage)
            ),
            loadingSpinner
          )
        : react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'div',
              { className: DelegationCenterBody_scss_1.default.bodyTitle },
              react_1.default.createElement('div', null, title),
              nextEpochNumber &&
                futureEpochNumber &&
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      DelegationCenterBody_scss_1.default.rightBodyTitle,
                  },
                  react_1.default.createElement(
                    'span',
                    null,
                    currentEpochTitle
                  ),
                  react_1.default.createElement('span', null, nextEpochNumber),
                  react_1.default.createElement('span', null, futureEpochNumber)
                )
            ),
            react_1.default.createElement(
              'div',
              { className: DelegationCenterBody_scss_1.default.mainContent },
              wallets.map((wallet) =>
                react_1.default.createElement(WalletRow_1.default, {
                  key: wallet.id,
                  wallet: wallet,
                  // @ts-ignore ts-migrate(2322) FIXME: Type '{ key: string; wallet: Wallet; numberOfStake... Remove this comment to see the full error message
                  numberOfStakePools: numberOfStakePools,
                  numberOfRankedStakePools: numberOfRankedStakePools,
                  onDelegate: () => onDelegate(wallet.id),
                  onUndelegate: () => onUndelegate(wallet.id),
                  delegatedStakePool: getStakePoolById(
                    wallet.delegatedStakePoolId
                  ),
                  getStakePoolById: getStakePoolById,
                  nextEpochNumber: nextEpochNumber,
                  futureEpochNumber: futureEpochNumber,
                  currentTheme: currentTheme,
                  isListActive: isListActive,
                  listName: listName,
                  onOpenExternalLink: onOpenExternalLink,
                  containerClassName: containerClassName,
                  setListActive: setListActive,
                })
              )
            )
          )
    );
  }
};
DelegationCenterBody = __decorate(
  [mobx_react_1.observer],
  DelegationCenterBody
);
exports.default = DelegationCenterBody;
//# sourceMappingURL=DelegationCenterBody.js.map
