// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
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

  render() {
    const { uiDialogs, staking, wallets } = this.props.stores;

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
