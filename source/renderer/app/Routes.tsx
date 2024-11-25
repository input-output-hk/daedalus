import React from 'react';
import { Redirect, Route, Switch, withRouter } from 'react-router-dom';
import { ROUTES } from './routes-config';
// PAGES
import Root from './containers/Root';
import InitialSettingsPage from './containers/profile/InitialSettingsPage';
import Settings from './containers/settings/Settings';
import GeneralSettingsPage from './containers/settings/categories/GeneralSettingsPage';
import WalletsSettingsPage from './containers/settings/categories/WalletsSettingsPage';
import StakePoolsSettingsPage from './containers/settings/categories/StakePoolsSettingsPage';
import SupportSettingsPage from './containers/settings/categories/SupportSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';
import SecuritySettingsPage from './containers/settings/categories/SecuritySettingsPage';
import TermsOfUsePage from './containers/profile/TermsOfUsePage';
import DataLayerMigrationPage from './containers/profile/DataLayerMigrationPage';
import DisplaySettingsPage from './containers/settings/categories/DisplaySettingsPage';
import PaperWalletCreateCertificatePage from './containers/wallet/PaperWalletCreateCertificatePage';
import Staking from './containers/staking/Staking';
import DelegationCenterPage from './containers/staking/DelegationCenterPage';
import StakingEpochsPage from './containers/staking/StakingEpochsPage';
import StakingInfoPage from './containers/staking/StakingInfoPage';
import StakingRewardsPage from './containers/staking/StakingRewardsPage';
import StakePoolsListPage from './containers/staking/StakePoolsListPage';
import StakingCountdownPage from './containers/staking/StakingCountdownPage';
import RedeemItnRewardsContainer from './containers/staking/RedeemItnRewardsContainer';
import Wallet from './containers/wallet/Wallet';
import WalletAddPage from './containers/wallet/WalletAddPage';
import WalletSummaryPage from './containers/wallet/WalletSummaryPage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import WalletTransactionsPage from './containers/wallet/WalletTransactionsPage';
import WalletTokensPage from './containers/wallet/WalletTokensPage';
import WalletSettingsPage from './containers/wallet/WalletSettingsPage';
import WalletUtxoPage from './containers/wallet/WalletUtxoPage';
import VotingRegistrationPage from './containers/voting/VotingRegistrationPage';
import { IS_STAKING_INFO_PAGE_AVAILABLE } from './config/stakingConfig';
import AnalyticsConsentPage from './containers/profile/AnalyticsConsentPage';
import TrackedRoute from './analytics/TrackedRoute';
import Voting from './containers/voting/Voting';
import VotingGovernancePage from './containers/voting/VotingGovernancePage';

export const Routes = withRouter(() => (
  <Route path={ROUTES.ROOT}>
    <Root>
      <Switch>
        <Route
          exact
          path={ROUTES.ROOT}
          component={() => <Redirect to={ROUTES.WALLETS.ROOT} />}
        />
        <Route
          path={ROUTES.PROFILE.INITIAL_SETTINGS}
          component={InitialSettingsPage}
        />
        <Route path={ROUTES.PROFILE.TERMS_OF_USE} component={TermsOfUsePage} />
        <Route
          path={ROUTES.PROFILE.ANALYTICS}
          component={AnalyticsConsentPage}
        />
        <TrackedRoute
          pageTitle="Data Layer Migration Page"
          path={ROUTES.PROFILE.DATA_LAYER_MIGRATION}
          component={DataLayerMigrationPage}
        />
        <TrackedRoute
          pageTitle="Add Wallet"
          path={ROUTES.WALLETS.ADD}
          component={WalletAddPage}
        />
        <Route path={ROUTES.WALLETS.ROOT}>
          <Wallet>
            <Route
              exact
              path={ROUTES.WALLETS.ROOT}
              component={() => <Redirect to={ROUTES.WALLETS.SUMMARY} />}
            />
            <TrackedRoute
              pageTitle="Wallet Summary"
              path={ROUTES.WALLETS.SUMMARY}
              component={WalletSummaryPage}
            />
            <TrackedRoute
              pageTitle="Send Screen"
              path={ROUTES.WALLETS.SEND}
              component={WalletSendPage}
            />
            <TrackedRoute
              pageTitle="Receive Screen"
              path={ROUTES.WALLETS.RECEIVE}
              component={WalletReceivePage}
            />
            <TrackedRoute
              pageTitle="Tokens"
              path={ROUTES.WALLETS.TOKENS}
              component={WalletTokensPage}
            />
            <TrackedRoute
              pageTitle="Transactions"
              path={ROUTES.WALLETS.TRANSACTIONS}
              component={WalletTransactionsPage}
            />
            <TrackedRoute
              pageTitle="Wallet Settings"
              path={ROUTES.WALLETS.SETTINGS}
              component={WalletSettingsPage}
            />
            <TrackedRoute
              pageTitle="Wallet UTxO distribution"
              path={ROUTES.WALLETS.UTXO}
              component={WalletUtxoPage}
            />
          </Wallet>
        </Route>
        <Route path={ROUTES.SETTINGS.ROOT}>
          <Settings>
            <Route
              exact
              path={ROUTES.SETTINGS.ROOT}
              component={() => <Redirect to={ROUTES.SETTINGS.GENERAL} />}
            />
            <TrackedRoute
              pageTitle="General Settings"
              path={ROUTES.SETTINGS.GENERAL}
              component={GeneralSettingsPage}
            />
            <TrackedRoute
              pageTitle="Wallets Settings"
              path={ROUTES.SETTINGS.WALLETS}
              component={WalletsSettingsPage}
            />
            <TrackedRoute
              pageTitle="Stake Pools Settings"
              path={ROUTES.SETTINGS.STAKE_POOLS}
              component={StakePoolsSettingsPage}
            />
            <TrackedRoute
              pageTitle="Terms of Use"
              path={ROUTES.SETTINGS.TERMS_OF_USE}
              component={TermsOfUseSettingsPage}
            />
            <TrackedRoute
              pageTitle="Support"
              path={ROUTES.SETTINGS.SUPPORT}
              component={SupportSettingsPage}
            />
            <TrackedRoute
              pageTitle="Display Settings"
              path={ROUTES.SETTINGS.DISPLAY}
              component={DisplaySettingsPage}
            />
            <TrackedRoute
              pageTitle="Security Settings"
              path={ROUTES.SETTINGS.SECURITY}
              component={SecuritySettingsPage}
            />
          </Settings>
        </Route>
        <Route
          path={ROUTES.PAPER_WALLET_CREATE_CERTIFICATE}
          component={PaperWalletCreateCertificatePage}
        />
        <Route path={ROUTES.STAKING.ROOT}>
          <Staking>
            <Route
              exact
              path={ROUTES.STAKING.ROOT}
              component={() => (
                <Redirect to={ROUTES.STAKING.DELEGATION_CENTER} />
              )}
            />
            <TrackedRoute
              pageTitle="Staking Countdown"
              path={ROUTES.STAKING.COUNTDOWN}
              component={StakingCountdownPage}
            />
            <TrackedRoute
              pageTitle="Delegation Center"
              path={ROUTES.STAKING.DELEGATION_CENTER}
              component={DelegationCenterPage}
            />
            <TrackedRoute
              pageTitle="Stake Pools List"
              path={ROUTES.STAKING.STAKE_POOLS}
              component={StakePoolsListPage}
            />
            <TrackedRoute
              pageTitle="Staking Rewards"
              path={ROUTES.STAKING.REWARDS}
              component={StakingRewardsPage}
            />
            <TrackedRoute
              pageTitle="Staking Epochs"
              path={ROUTES.STAKING.EPOCHS}
              component={StakingEpochsPage}
            />
            {IS_STAKING_INFO_PAGE_AVAILABLE && (
              <TrackedRoute
                pageTitle="Staking info"
                path={ROUTES.STAKING.INFO}
                component={StakingInfoPage}
              />
            )}
          </Staking>
          <TrackedRoute
            pageTitle="Redeem ITN rewards"
            path={ROUTES.REDEEM_ITN_REWARDS}
            component={RedeemItnRewardsContainer}
          />
        </Route>
        <Route path={ROUTES.VOTING.ROOT}>
          <Voting>
            <TrackedRoute
              pageTitle="Voting Registration"
              path={ROUTES.VOTING.REGISTRATION}
              component={VotingRegistrationPage}
            />
            <TrackedRoute
              pageTitle="Voting Governance"
              path={ROUTES.VOTING.GOVERNANCE}
              component={VotingGovernancePage}
            />
          </Voting>
        </Route>
      </Switch>
    </Root>
  </Route>
));
