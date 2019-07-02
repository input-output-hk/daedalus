// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores')
@observer
export default class DelegationCenterPage extends Component<Props> {
  static defaultProps = { stores: null };

  render() {
    const { uiDialogs } = this.props.stores;

    return (
      <DelegationCenter name="DelegationCenter">
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </DelegationCenter>
    );
  }
}
