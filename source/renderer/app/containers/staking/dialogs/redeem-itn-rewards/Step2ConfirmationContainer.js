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
    const { onContinue, onClose, onBack, stores } = this.props;
    const {
      walletName,
      rewardsTotal,
      transactionFees,
      finalTotal,
      isSubmittingReedem,
    } = stores.staking;
    if (!walletName) return null;
    return (
      <Step2ConfirmationDialog
        walletName={walletName}
        rewardsTotal={rewardsTotal}
        transactionFees={transactionFees}
        finalTotal={finalTotal}
        isSubmitting={isSubmittingReedem}
        error1
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
      />
    );
  }
}
