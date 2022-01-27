import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step2ConfirmationDialog from '../../../../components/staking/redeem-itn-rewards/Step2ConfirmationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class Step2ConfirmationContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onBack, stores, actions } = this.props;
    const {
      redeemWallet,
      transactionFees,
      isSubmittingReedem,
      confirmationStepError,
    } = stores.staking;
    const { onConfirmationContinue } = actions.staking;
    if (!redeemWallet) throw new Error('Redeem wallet required');
    return (
      <Step2ConfirmationDialog
        wallet={redeemWallet}
        transactionFees={transactionFees}
        isSubmitting={isSubmittingReedem}
        error={confirmationStepError}
        onClose={onClose}
        onContinue={onConfirmationContinue.trigger}
        onBack={onBack}
      />
    );
  }
}

export default Step2ConfirmationContainer;
