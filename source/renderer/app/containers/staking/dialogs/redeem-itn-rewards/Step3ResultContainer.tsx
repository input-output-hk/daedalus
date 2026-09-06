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
class Step3ResultContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onBack, onClose, stores, actions } = this.props;
    const {
      redeemWallet,
      transactionFees,
      redeemedRewards,
      redeemSuccess,
    } = stores.staking;
    const { onResultContinue } = actions.staking;
    if (!redeemWallet) throw new Error('Redeem wallet required');

    if (redeemSuccess) {
      return (
        <Step3SuccessDialog
          wallet={redeemWallet}
          transactionFees={transactionFees}
          redeemedRewards={redeemedRewards}
          onClose={onClose}
          onContinue={onResultContinue.trigger}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onBack={onBack}
        />
      );
    }

    return <Step3FailureDialog onClose={onClose} onBack={onBack} />;
  }
}

export default Step3ResultContainer;
