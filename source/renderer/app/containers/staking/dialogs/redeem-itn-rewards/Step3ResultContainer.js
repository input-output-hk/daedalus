// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step3SuccessDialog from '../../../../components/staking/redeem-itn-rewards/Step3SuccessDialog';
import Step3FailureDialog from '../../../../components/staking/redeem-itn-rewards/Step3FailureDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class Step3ResultContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onBack, onContinue, onClose, stores } = this.props;
    const {
      walletName,
      rewardsTotal,
      transactionFees,
      finalTotal,
      isSubmitting,
      stakingSuccess,
    } = stores.staking;

    if (stakingSuccess) {
      return (
        <Step3SuccessDialog
          walletName={walletName}
          rewardsTotal={rewardsTotal}
          transactionFees={transactionFees}
          finalTotal={finalTotal}
          isSubmitting={isSubmitting}
          error1
          onClose={onClose}
          onContinue={onContinue}
          onBack={onBack}
        />
      );
    }
    return <Step3FailureDialog onClose={onClose} onBack={onBack} />;
  }
}
