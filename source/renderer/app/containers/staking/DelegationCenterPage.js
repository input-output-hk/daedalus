// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
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

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  render() {
    const { uiDialogs, staking, wallets, profile } = this.props.stores;

    if (!wallets.all.length) {
      return (
        <DelegationCenterNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
        />
      );
    }

    const { currentNumberFormatPretty } = profile;

    return (
      <div>
        <DelegationCenter
          adaValue={staking.adaValue}
          percentage={staking.percentage}
          wallets={wallets.all}
          onDelegate={this.handleDelegate}
          currentNumberFormatPretty={currentNumberFormatPretty}
        />
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </div>
    );
  }
}
