'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.Routes = void 0;
const react_1 = __importDefault(require('react'));
const react_router_dom_1 = require('react-router-dom');
const routes_config_1 = require('./routes-config');
// PAGES
const Root_1 = __importDefault(require('./containers/Root'));
const InitialSettingsPage_1 = __importDefault(
  require('./containers/profile/InitialSettingsPage')
);
const Settings_1 = __importDefault(require('./containers/settings/Settings'));
const GeneralSettingsPage_1 = __importDefault(
  require('./containers/settings/categories/GeneralSettingsPage')
);
const WalletsSettingsPage_1 = __importDefault(
  require('./containers/settings/categories/WalletsSettingsPage')
);
const StakePoolsSettingsPage_1 = __importDefault(
  require('./containers/settings/categories/StakePoolsSettingsPage')
);
const SupportSettingsPage_1 = __importDefault(
  require('./containers/settings/categories/SupportSettingsPage')
);
const TermsOfUseSettingsPage_1 = __importDefault(
  require('./containers/settings/categories/TermsOfUseSettingsPage')
);
const SecuritySettingsPage_1 = __importDefault(
  require('./containers/settings/categories/SecuritySettingsPage')
);
const TermsOfUsePage_1 = __importDefault(
  require('./containers/profile/TermsOfUsePage')
);
const DataLayerMigrationPage_1 = __importDefault(
  require('./containers/profile/DataLayerMigrationPage')
);
const DisplaySettingsPage_1 = __importDefault(
  require('./containers/settings/categories/DisplaySettingsPage')
);
const PaperWalletCreateCertificatePage_1 = __importDefault(
  require('./containers/wallet/PaperWalletCreateCertificatePage')
);
const Staking_1 = __importDefault(require('./containers/staking/Staking'));
const DelegationCenterPage_1 = __importDefault(
  require('./containers/staking/DelegationCenterPage')
);
const StakingEpochsPage_1 = __importDefault(
  require('./containers/staking/StakingEpochsPage')
);
const StakingInfoPage_1 = __importDefault(
  require('./containers/staking/StakingInfoPage')
);
const StakingRewardsPage_1 = __importDefault(
  require('./containers/staking/StakingRewardsPage')
);
const StakePoolsListPage_1 = __importDefault(
  require('./containers/staking/StakePoolsListPage')
);
const StakingCountdownPage_1 = __importDefault(
  require('./containers/staking/StakingCountdownPage')
);
const RedeemItnRewardsContainer_1 = __importDefault(
  require('./containers/staking/RedeemItnRewardsContainer')
);
const Wallet_1 = __importDefault(require('./containers/wallet/Wallet'));
const WalletAddPage_1 = __importDefault(
  require('./containers/wallet/WalletAddPage')
);
const WalletSummaryPage_1 = __importDefault(
  require('./containers/wallet/WalletSummaryPage')
);
const WalletSendPage_1 = __importDefault(
  require('./containers/wallet/WalletSendPage')
);
const WalletReceivePage_1 = __importDefault(
  require('./containers/wallet/WalletReceivePage')
);
const WalletTransactionsPage_1 = __importDefault(
  require('./containers/wallet/WalletTransactionsPage')
);
const WalletTokensPage_1 = __importDefault(
  require('./containers/wallet/WalletTokensPage')
);
const WalletSettingsPage_1 = __importDefault(
  require('./containers/wallet/WalletSettingsPage')
);
const WalletUtxoPage_1 = __importDefault(
  require('./containers/wallet/WalletUtxoPage')
);
const VotingRegistrationPage_1 = __importDefault(
  require('./containers/voting/VotingRegistrationPage')
);
const stakingConfig_1 = require('./config/stakingConfig');
const AnalyticsConsentPage_1 = __importDefault(
  require('./containers/profile/AnalyticsConsentPage')
);
const TrackedRoute_1 = __importDefault(require('./analytics/TrackedRoute'));
exports.Routes = (0, react_router_dom_1.withRouter)(() =>
  react_1.default.createElement(
    react_router_dom_1.Route,
    { path: routes_config_1.ROUTES.ROOT },
    react_1.default.createElement(
      Root_1.default,
      null,
      react_1.default.createElement(
        react_router_dom_1.Switch,
        null,
        react_1.default.createElement(react_router_dom_1.Route, {
          exact: true,
          path: routes_config_1.ROUTES.ROOT,
          component: () =>
            react_1.default.createElement(react_router_dom_1.Redirect, {
              to: routes_config_1.ROUTES.WALLETS.ROOT,
            }),
        }),
        react_1.default.createElement(react_router_dom_1.Route, {
          path: routes_config_1.ROUTES.PROFILE.INITIAL_SETTINGS,
          component: InitialSettingsPage_1.default,
        }),
        react_1.default.createElement(react_router_dom_1.Route, {
          path: routes_config_1.ROUTES.PROFILE.TERMS_OF_USE,
          component: TermsOfUsePage_1.default,
        }),
        react_1.default.createElement(react_router_dom_1.Route, {
          path: routes_config_1.ROUTES.PROFILE.ANALYTICS,
          component: AnalyticsConsentPage_1.default,
        }),
        react_1.default.createElement(TrackedRoute_1.default, {
          pageTitle: 'Data Layer Migration Page',
          path: routes_config_1.ROUTES.PROFILE.DATA_LAYER_MIGRATION,
          component: DataLayerMigrationPage_1.default,
        }),
        react_1.default.createElement(TrackedRoute_1.default, {
          pageTitle: 'Add Wallet',
          path: routes_config_1.ROUTES.WALLETS.ADD,
          component: WalletAddPage_1.default,
        }),
        react_1.default.createElement(
          react_router_dom_1.Route,
          { path: routes_config_1.ROUTES.WALLETS.ROOT },
          react_1.default.createElement(
            Wallet_1.default,
            null,
            react_1.default.createElement(react_router_dom_1.Route, {
              exact: true,
              path: routes_config_1.ROUTES.WALLETS.ROOT,
              component: () =>
                react_1.default.createElement(react_router_dom_1.Redirect, {
                  to: routes_config_1.ROUTES.WALLETS.SUMMARY,
                }),
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Wallet Summary',
              path: routes_config_1.ROUTES.WALLETS.SUMMARY,
              component: WalletSummaryPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Send Screen',
              path: routes_config_1.ROUTES.WALLETS.SEND,
              component: WalletSendPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Receive Screen',
              path: routes_config_1.ROUTES.WALLETS.RECEIVE,
              component: WalletReceivePage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Tokens',
              path: routes_config_1.ROUTES.WALLETS.TOKENS,
              component: WalletTokensPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Transactions',
              path: routes_config_1.ROUTES.WALLETS.TRANSACTIONS,
              component: WalletTransactionsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Wallet Settings',
              path: routes_config_1.ROUTES.WALLETS.SETTINGS,
              component: WalletSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Wallet UTxO distribution',
              path: routes_config_1.ROUTES.WALLETS.UTXO,
              component: WalletUtxoPage_1.default,
            })
          )
        ),
        react_1.default.createElement(
          react_router_dom_1.Route,
          { path: routes_config_1.ROUTES.SETTINGS.ROOT },
          react_1.default.createElement(
            Settings_1.default,
            null,
            react_1.default.createElement(react_router_dom_1.Route, {
              exact: true,
              path: routes_config_1.ROUTES.SETTINGS.ROOT,
              component: () =>
                react_1.default.createElement(react_router_dom_1.Redirect, {
                  to: routes_config_1.ROUTES.SETTINGS.GENERAL,
                }),
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'General Settings',
              path: routes_config_1.ROUTES.SETTINGS.GENERAL,
              component: GeneralSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Wallets Settings',
              path: routes_config_1.ROUTES.SETTINGS.WALLETS,
              component: WalletsSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Stake Pools Settings',
              path: routes_config_1.ROUTES.SETTINGS.STAKE_POOLS,
              component: StakePoolsSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Terms of Use',
              path: routes_config_1.ROUTES.SETTINGS.TERMS_OF_USE,
              component: TermsOfUseSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Support',
              path: routes_config_1.ROUTES.SETTINGS.SUPPORT,
              component: SupportSettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Display Settings',
              path: routes_config_1.ROUTES.SETTINGS.DISPLAY,
              component: DisplaySettingsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Security Settings',
              path: routes_config_1.ROUTES.SETTINGS.SECURITY,
              component: SecuritySettingsPage_1.default,
            })
          )
        ),
        react_1.default.createElement(react_router_dom_1.Route, {
          path: routes_config_1.ROUTES.PAPER_WALLET_CREATE_CERTIFICATE,
          component: PaperWalletCreateCertificatePage_1.default,
        }),
        react_1.default.createElement(
          react_router_dom_1.Route,
          { path: routes_config_1.ROUTES.STAKING.ROOT },
          react_1.default.createElement(
            Staking_1.default,
            null,
            react_1.default.createElement(react_router_dom_1.Route, {
              exact: true,
              path: routes_config_1.ROUTES.STAKING.ROOT,
              component: () =>
                react_1.default.createElement(react_router_dom_1.Redirect, {
                  to: routes_config_1.ROUTES.STAKING.DELEGATION_CENTER,
                }),
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Staking Countdown',
              path: routes_config_1.ROUTES.STAKING.COUNTDOWN,
              component: StakingCountdownPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Delegation Center',
              path: routes_config_1.ROUTES.STAKING.DELEGATION_CENTER,
              component: DelegationCenterPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Stake Pools List',
              path: routes_config_1.ROUTES.STAKING.STAKE_POOLS,
              component: StakePoolsListPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Staking Rewards',
              path: routes_config_1.ROUTES.STAKING.REWARDS,
              component: StakingRewardsPage_1.default,
            }),
            react_1.default.createElement(TrackedRoute_1.default, {
              pageTitle: 'Staking Epochs',
              path: routes_config_1.ROUTES.STAKING.EPOCHS,
              component: StakingEpochsPage_1.default,
            }),
            stakingConfig_1.IS_STAKING_INFO_PAGE_AVAILABLE &&
              react_1.default.createElement(TrackedRoute_1.default, {
                pageTitle: 'Staking info',
                path: routes_config_1.ROUTES.STAKING.INFO,
                component: StakingInfoPage_1.default,
              })
          ),
          react_1.default.createElement(TrackedRoute_1.default, {
            pageTitle: 'Redeem ITN rewards',
            path: routes_config_1.ROUTES.REDEEM_ITN_REWARDS,
            component: RedeemItnRewardsContainer_1.default,
          })
        ),
        react_1.default.createElement(TrackedRoute_1.default, {
          pageTitle: 'Voting Registration',
          path: routes_config_1.ROUTES.VOTING.REGISTRATION,
          component: VotingRegistrationPage_1.default,
        })
      )
    )
  )
);
//# sourceMappingURL=Routes.js.map
