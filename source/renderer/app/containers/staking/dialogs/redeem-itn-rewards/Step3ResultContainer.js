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
    const { onBack, onClose, stores, actions } = this.props;
    const {
      redeemWallet,
      rewardsTotal,
      transactionFees,
      finalTotal,
      stakingSuccess,
      stakingFailure,
    } = stores.staking;
    const { onResultContinue } = actions.staking;
    if (!redeemWallet) throw new Error('Redeem wallet required');
    if (stakingSuccess) {
      return (
        <Step3SuccessDialog
          wallet={redeemWallet}
          rewardsTotal={rewardsTotal}
          transactionFees={transactionFees}
          finalTotal={finalTotal}
          error1
          onClose={onClose}
          onContinue={onResultContinue.trigger}
          onBack={onBack}
        />
      );
    }
    return (
      <Step3FailureDialog
        onClose={onClose}
        onBack={onBack}
        stakingFailure={stakingFailure}
      />
    );
  }
}
