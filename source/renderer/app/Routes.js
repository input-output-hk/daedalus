// @flow
import React from 'react';
import { Route, Redirect } from 'react-router';
import { ROUTES } from './routes-config';

// PAGES
import Root from './containers/Root';
import InitialSettingsPage from './containers/profile/InitialSettingsPage';
import Settings from './containers/settings/Settings';
import GeneralSettingsPage from './containers/settings/categories/GeneralSettingsPage';
import SupportSettingsPage from './containers/settings/categories/SupportSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';
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
import Wallet from './containers/wallet/Wallet';
import WalletAddPage from './containers/wallet/WalletAddPage';
import WalletSummaryPage from './containers/wallet/WalletSummaryPage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import WalletTransactionsPage from './containers/wallet/WalletTransactionsPage';
import WalletSettingsPage from './containers/wallet/WalletSettingsPage';
import WalletUtxoPage from './containers/wallet/WalletUtxoPage';

export const Routes = (
  <Route path={ROUTES.ROOT} component={Root}>
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
      path={ROUTES.PROFILE.DATA_LAYER_MIGRATION}
      component={DataLayerMigrationPage}
    />
    <Route path={ROUTES.WALLETS.ADD} component={WalletAddPage} />
    <Route path={ROUTES.WALLETS.ROOT} component={Wallet}>
      <Route path={ROUTES.WALLETS.SUMMARY} component={WalletSummaryPage} />
      <Route
        path={ROUTES.WALLETS.TRANSACTIONS}
        component={WalletTransactionsPage}
      />
      <Route path={ROUTES.WALLETS.SEND} component={WalletSendPage} />
      <Route path={ROUTES.WALLETS.RECEIVE} component={WalletReceivePage} />
      <Route path={ROUTES.WALLETS.SETTINGS} component={WalletSettingsPage} />
      <Route path={ROUTES.WALLETS.UTXO} component={WalletUtxoPage} />
    </Route>
    <Route path={ROUTES.SETTINGS.ROOT} component={Settings}>
      <Route
        exact
        path={ROUTES.SETTINGS.ROOT}
        component={() => <Redirect to={ROUTES.SETTINGS.GENERAL} />}
      />
      <Route path={ROUTES.SETTINGS.GENERAL} component={GeneralSettingsPage} />
      <Route
        path={ROUTES.SETTINGS.TERMS_OF_USE}
        component={TermsOfUseSettingsPage}
      />
      <Route path={ROUTES.SETTINGS.SUPPORT} component={SupportSettingsPage} />
      <Route path={ROUTES.SETTINGS.DISPLAY} component={DisplaySettingsPage} />
    </Route>
    <Route
      path={ROUTES.PAPER_WALLET_CREATE_CERTIFICATE}
      component={PaperWalletCreateCertificatePage}
    />
    <Route path={ROUTES.STAKING.ROOT} component={Staking}>
      <Route
        exact
        path={ROUTES.STAKING.ROOT}
        component={() => <Redirect to={ROUTES.STAKING.INFO} />}
      />
      <Route path={ROUTES.STAKING.COUNTDOWN} component={StakingCountdownPage} />
      <Route
        path={ROUTES.STAKING.DELEGATION_CENTER}
        component={DelegationCenterPage}
      />
      <Route path={ROUTES.STAKING.STAKE_POOLS} component={StakePoolsListPage} />
      <Route path={ROUTES.STAKING.REWARDS} component={StakingRewardsPage} />
      <Route path={ROUTES.STAKING.EPOCHS} component={StakingEpochsPage} />
      <Route path={ROUTES.STAKING.INFO} component={StakingInfoPage} />
    </Route>
  </Route>
);
