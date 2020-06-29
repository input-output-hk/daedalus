// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step2ConfirmationDialog from '../../../../components/staking/redeem-itn-rewards/Step2ConfirmationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class Step2ConfirmationContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onBack, stores, actions } = this.props;
    const {
      redeemWallet,
      rewardsTotal,
      transactionFees,
      finalTotal,
      isSubmittingReedem,
    } = stores.staking;
    const { onConfirmationContinue } = actions.staking;
    if (!redeemWallet) return null;
    return (
      <Step2ConfirmationDialog
        rewardsTotal={rewardsTotal}
        wallet={redeemWallet}
        transactionFees={transactionFees}
        finalTotal={finalTotal}
        isSubmitting={isSubmittingReedem}
        error1
        onClose={onClose}
        onContinue={onConfirmationContinue.trigger}
        onBack={onBack}
      />
    );
  }
}
