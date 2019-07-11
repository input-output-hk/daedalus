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

  handleDelegate = () => {
    const { actions } = this.props;
    actions.dialogs.open.trigger({ dialog: DelegationSetupWizardDialog });
  };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  render() {
    const { uiDialogs, staking, wallets } = this.props.stores;

    if (!wallets.all.length) {
      return (
        <DelegationCenterNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
        />
      );
    }

    return (
      <DelegationCenter
        adaValue={staking.adaValue}
        percentage={staking.percentage}
        wallets={wallets.all}
        onDelegate={this.handleDelegate}
      >
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </DelegationCenter>
    );
  }
}
