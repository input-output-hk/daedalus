// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step2ConfirmationDialog from '../../../../components/staking/redeem-itn-rewards/Step2ConfirmationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';
// import { isValidMnemonic } from '../../../../../../common/config/crypto/decrypt';
// import {
//   getScrambledInput,
//   unscramblePaperWalletMnemonic,
// } from '../../../../utils/crypto';

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
      isSubmitting,
    } = stores.staking;
    return (
      <Step2ConfirmationDialog
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
}
