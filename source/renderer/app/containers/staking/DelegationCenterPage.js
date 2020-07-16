// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import UndelegateDialogContainer from './dialogs/UndelegateDialogContainer';
import UndelegateConfirmationDialog from '../../components/staking/delegation-center/UndelegateConfirmationDialog';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import DelegationCenterNoWallets from '../../components/staking/delegation-center/DelegationCenterNoWallets';
import { ROUTES } from '../../routes-config';
import { MIN_DELEGATION_FUNDS } from '../../config/stakingConfig';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class DelegationCenterPage extends Component<Props> {
  static defaultProps = { stores: null };

  handleDelegate = (walletId: string) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;

    actions.dialogs.open.trigger({ dialog: DelegationSetupWizardDialog });
    updateDataForActiveDialog.trigger({
      data: { walletId },
    });
  };

  handleUndelegate = async (walletId: string) => {
    const { actions, stores } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    const { isOpen } = stores.uiDialogs;
    const { calculateDelegationFee } = stores.staking;

    actions.dialogs.open.trigger({ dialog: UndelegateConfirmationDialog });
    const dialogData = {
      walletId,
      stakePoolQuitFee: null,
    };
    updateDataForActiveDialog.trigger({ data: dialogData });

    // Update dialog one more time when quit fee is calculated
    const stakePoolQuitFee = await calculateDelegationFee({ walletId });

    // Update dialog data only if UndelegateConfirmationDialog is still active
    // and fee calculation was successful
    if (isOpen(UndelegateConfirmationDialog) && stakePoolQuitFee) {
      updateDataForActiveDialog.trigger({
        data: {
          ...dialogData,
          stakePoolQuitFee,
        },
      });
    }
  };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  render() {
    const { stores } = this.props;
    const { app, uiDialogs, staking, wallets, networkStatus, profile } = stores;
    const { stakePools, getStakePoolById, fetchingStakePoolsFailed } = staking;
    const { isSynced, networkTip, nextEpoch, futureEpoch } = networkStatus;
    const { currentLocale } = profile;

    if (!wallets.allWallets.length) {
      return (
        <DelegationCenterNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
          minDelegationFunds={MIN_DELEGATION_FUNDS}
        />
      );
    }

    return (
      <Fragment>
        <DelegationCenter
          wallets={wallets.allWallets}
          numberOfStakePools={stakePools.length}
          onDelegate={this.handleDelegate}
          onUndelegate={this.handleUndelegate}
          networkTip={networkTip}
          nextEpoch={nextEpoch}
          futureEpoch={futureEpoch}
          getStakePoolById={getStakePoolById}
          isLoading={
            !isSynced || fetchingStakePoolsFailed || !stakePools.length
          }
          currentLocale={currentLocale}
        />
        {uiDialogs.isOpen(UndelegateConfirmationDialog) ? (
          <UndelegateDialogContainer
            onExternalLinkClick={app.openExternalLink}
          />
        ) : null}
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}
