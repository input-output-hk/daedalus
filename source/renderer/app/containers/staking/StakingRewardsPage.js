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
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const StakingRewards_1 = require('../../components/staking/rewards/StakingRewards');
const strings_1 = require('../../utils/strings');
const network_1 = require('../../utils/network');
const messages = (0, react_intl_1.defineMessages)({
  learnMoreLinkUrl: {
    id: 'staking.rewards.learnMore.linkUrl',
    defaultMessage: '!!!https://staking.cardano.org/',
    description: '"Learn more" link URL in the staking rewards page',
  },
});
let StakingRewardsPage = class StakingRewardsPage extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleLearnMoreClick = (event) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };
  onOpenExternalLink = (rewardsAddress) => {
    const { app } = this.props.stores;
    const {
      environment: { network },
    } = app;
    const cardanoExplorerLink = `${(0, network_1.getNetworkExplorerUrl)(
      network
    )}/address/${rewardsAddress}`;
    this.props.stores.app.openExternalLink(cardanoExplorerLink);
  };
  handleCopyAddress = (copiedAddress) => {
    const address = (0, strings_1.ellipsis)(copiedAddress, 15, 15);
    this.props.actions.wallets.copyAddress.trigger({
      address,
    });
  };
  render() {
    const {
      staking: { rewards },
      wallets,
    } = this.props.stores;
    const { requestCSVFile } = this.props.actions.staking;
    return react_1.default.createElement(StakingRewards_1.StakingRewards, {
      rewards: rewards,
      isLoading: false,
      isExporting: wallets.generatingRewardsCsvInProgress,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ rewards: any; isLoading: false; isExportin... Remove this comment to see the full error message
      onLearnMoreClick: this.handleLearnMoreClick,
      onExportCsv: requestCSVFile.trigger,
      onCopyAddress: this.handleCopyAddress,
      onOpenExternalLink: this.onOpenExternalLink,
    });
  }
};
StakingRewardsPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  StakingRewardsPage
);
exports.default = StakingRewardsPage;
//# sourceMappingURL=StakingRewardsPage.js.map
