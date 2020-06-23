// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step1ConfigurationDialog from '../../../../components/staking/redeem-itn-rewards/Step1ConfigurationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';
// import { isValidMnemonic } from '../../../../../../common/config/crypto/decrypt';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class Step1ConfigurationContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onContinue, onClose, onBack, stores } = this.props;
    const { allWallets } = stores.wallets;
    const isWalletValid = true;
    const isSubmitting = false;
    return (
      <Step1ConfigurationDialog
        wallets={allWallets}
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
        isWalletValid={isWalletValid}
        isSubmitting={isSubmitting}
      />
    );
  }
}
