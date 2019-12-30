// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationDialog from '../../../../components/wallet/wallet-restore/ConfigurationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class ConfigurationDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  handleContinue = (walletName: string, spendingPassword: string) => {
    const { actions } = this.props;
    const { restoreWalletSetConfig, restoreWallet } = actions.wallets;
    restoreWalletSetConfig.trigger({ walletName, spendingPassword });
    restoreWallet.trigger();
  };

  render() {
    const { onClose, onBack, stores } = this.props;
    const {
      restoreRequest,
      restoreLegacyRequest,
      getWalletRecoveryPhraseFromCertificateRequest,
    } = stores.wallets;
    const error =
      restoreRequest.error ||
      restoreLegacyRequest.error ||
      getWalletRecoveryPhraseFromCertificateRequest.error;
    const isExecuting =
      restoreRequest.isExecuting ||
      restoreLegacyRequest.isExecuting ||
      getWalletRecoveryPhraseFromCertificateRequest.isExecuting;
    return (
      <ConfigurationDialog
        isSubmitting={isExecuting}
        onClose={onClose}
        onContinue={this.handleContinue}
        onBack={onBack}
        error={error}
      />
    );
  }
}
