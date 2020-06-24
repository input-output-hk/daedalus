// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Step1ConfigurationDialog from '../../../../components/staking/redeem-itn-rewards/Step1ConfigurationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';
import validWords from '../../../../../../common/config/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../../common/config/crypto/decrypt';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class Step1ConfigurationContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onContinue, onClose, onBack, stores, actions } = this.props;
    const { allWallets } = stores.wallets;
    const { redeemWallet, redeemError } = stores.staking;
    const { onSelectRedeemWallet } = actions.staking;
    const isWalletValid = true;
    const isSubmitting = false;
    return (
      <Step1ConfigurationDialog
        wallets={allWallets}
        redeemWallet={redeemWallet}
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
        isWalletValid={isWalletValid}
        isSubmitting={isSubmitting}
        mnemonicValidator={isValidMnemonic}
        error={redeemError}
        onSelectWallet={walletId => onSelectRedeemWallet.trigger({ walletId })}
        suggestedMnemonics={validWords}
      />
    );
  }
}
