// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import BigNumber from 'bignumber.js';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import UndelegateDialogContainer from './dialogs/UndelegateDialogContainer';
import UndelegateConfirmationDialog from '../../components/staking/delegation-center/UndelegateConfirmationDialog';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import DelegationCenterNoWallets from '../../components/staking/delegation-center/DelegationCenterNoWallets';
import { ROUTES } from '../../routes-config';
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
    const { estimateQuitFee } = stores.staking;

    actions.dialogs.open.trigger({ dialog: UndelegateConfirmationDialog });
    const dialogData = {
      walletId,
      stakePoolQuitFee: new BigNumber(0),
    };
    updateDataForActiveDialog.trigger({ data: dialogData });

    // Update dialog one more time when quit fee is calculated
    const stakePoolQuitFee = await estimateQuitFee({ walletId });
    updateDataForActiveDialog.trigger({
      data: {
        ...dialogData,
        stakePoolQuitFee,
      },
    });
  };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  render() {
    const { stores } = this.props;
    const { app, uiDialogs, staking, wallets, networkStatus } = stores;
    const { stakePools, getStakePoolById } = staking;
    const { networkTip, nextEpoch } = networkStatus;

    if (!wallets.allWallets.length) {
      return (
        <DelegationCenterNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
        />
      );
    }

    return (
      <div>
        <DelegationCenter
          wallets={wallets.allWallets}
          numberOfStakePools={stakePools.length}
          onDelegate={this.handleDelegate}
          onUndelegate={this.handleUndelegate}
          networkTip={networkTip}
          nextEpoch={nextEpoch}
          getStakePoolById={getStakePoolById}
        />
        {uiDialogs.isOpen(UndelegateConfirmationDialog) ? (
          <UndelegateDialogContainer
            onExternalLinkClick={app.openExternalLink}
          />
        ) : null}
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </div>
    );
  }
}
