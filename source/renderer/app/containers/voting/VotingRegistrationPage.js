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
const MainLayout_1 = __importDefault(require('../MainLayout'));
const votingConfig_1 = require('../../config/votingConfig');
const VerticalFlexContainer_1 = __importDefault(
  require('../../components/layout/VerticalFlexContainer')
);
const VotingInfo_1 = __importDefault(
  require('../../components/voting/voting-info/VotingInfo')
);
const VotingNoWallets_1 = __importDefault(
  require('../../components/voting/VotingNoWallets')
);
const VotingUnavailable_1 = __importDefault(
  require('../../components/voting/VotingUnavailable')
);
const VotingRegistrationDialog_1 = __importDefault(
  require('../../components/voting/voting-registration-wizard-steps/widgets/VotingRegistrationDialog')
);
const routes_config_1 = require('../../routes-config');
const VotingRegistrationDialogContainer_1 = __importDefault(
  require('./dialogs/VotingRegistrationDialogContainer')
);
const VotingFooterLinks_1 = require('../../components/voting/VotingFooterLinks');
let VotingRegistrationPage = class VotingRegistrationPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.ADD,
    });
  };
  getInnerContent = (isVotingRegistrationDialogOpen) => {
    const { app, networkStatus, wallets, profile, voting } = this.props.stores;
    const { isSynced, syncPercentage } = networkStatus;
    const { openExternalLink } = app;
    if (!isSynced && !isVotingRegistrationDialogOpen) {
      return react_1.default.createElement(VotingUnavailable_1.default, {
        syncPercentage: syncPercentage,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ syncPercentage: any; onExternalLinkClick: ... Remove this comment to see the full error message
        onExternalLinkClick: openExternalLink,
      });
    }
    if (!wallets.allWallets.length) {
      return react_1.default.createElement(VotingNoWallets_1.default, {
        onGoToCreateWalletClick: this.handleGoToCreateWalletClick,
        minVotingFunds: votingConfig_1.VOTING_REGISTRATION_MIN_WALLET_FUNDS,
      });
    }
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    return react_1.default.createElement(VotingInfo_1.default, {
      fundInfo: voting.catalystFund,
      fundPhase: voting.fundPhase,
      currentLocale: currentLocale,
      currentDateFormat: currentDateFormat,
      currentTimeFormat: currentTimeFormat,
      onRegisterToVoteClick: () =>
        this.props.actions.dialogs.open.trigger({
          dialog: VotingRegistrationDialog_1.default,
        }),
      onExternalLinkClick: openExternalLink,
    });
  };
  render() {
    const { stores } = this.props;
    const { app, uiDialogs } = stores;
    const { openExternalLink } = app;
    const isVotingRegistrationDialogOpen = uiDialogs.isOpen(
      VotingRegistrationDialog_1.default
    );
    const innerContent = this.getInnerContent(isVotingRegistrationDialogOpen);
    return react_1.default.createElement(
      MainLayout_1.default,
      null,
      react_1.default.createElement(
        VerticalFlexContainer_1.default,
        null,
        innerContent,
        react_1.default.createElement(VotingFooterLinks_1.VotingFooterLinks, {
          onClickExternalLink: openExternalLink,
        })
      ),
      isVotingRegistrationDialogOpen &&
        react_1.default.createElement(
          VotingRegistrationDialogContainer_1.default,
          null
        )
    );
  }
};
VotingRegistrationPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  VotingRegistrationPage
);
exports.default = VotingRegistrationPage;
//# sourceMappingURL=VotingRegistrationPage.js.map
