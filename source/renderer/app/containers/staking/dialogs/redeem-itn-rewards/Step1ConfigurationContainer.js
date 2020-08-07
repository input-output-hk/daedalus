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
    const { onClose, onBack, stores, actions } = this.props;
    const { allWallets } = stores.wallets;
    const {
      redeemWallet,
      configurationStepError,
      isSubmittingReedem,
      redeemRecoveryPhrase,
    } = stores.staking;
    const { openExternalLink } = stores.app;
    const { onConfigurationContinue, onSelectRedeemWallet } = actions.staking;
    return (
      <Step1ConfigurationDialog
        error={configurationStepError}
        isSubmitting={isSubmittingReedem}
        mnemonicValidator={isValidMnemonic}
        onBack={onBack}
        onClose={onClose}
        onContinue={onConfigurationContinue.trigger}
        onSelectWallet={walletId => onSelectRedeemWallet.trigger({ walletId })}
        suggestedMnemonics={validWords}
        wallet={redeemWallet}
        wallets={allWallets}
        openExternalLink={openExternalLink}
        recoveryPhrase={redeemRecoveryPhrase}
      />
    );
  }
}
