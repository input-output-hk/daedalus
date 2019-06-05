// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationStepsNotAvailable from '../../components/staking/delegation-setup-wizard/DelegationStepsNotAvailable';
import DelegationStepsChooseWallet from '../../components/staking/delegation-setup-wizard/DelegationStepsChooseWallet';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

type State = {
  activeStep: boolean,
};

@inject('stores', 'actions')
@observer
export default class DelegationSetupWizardContainer extends Component<
  Props,
  State
> {
  state = {
    activeStep: 0,
  };

  onClose = () => {
    console.debug('close');
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  onContinue = () => {
    console.debug('onContinue');
  };

  render() {
    const { activeStep } = this.state;
    const {
      isDisabled,
      stores: { app, staking },
    } = this.props;

    let content;
    if (isDisabled) {
      content = <DelegationStepsNotAvailable onClose={this.onClose} />;
    } else if (activeStep === 0) {
      content = (
        <DelegationStepsChooseWallet
          onClose={this.onClose}
          onContinue={this.onContinue}
        />
      );
    }

    return content;
  }
}
