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

  handleContinue = () => this.props.actions.wallets.restoreWallet.trigger();

  handleChange = (param: string, field: Object) =>
    this.props.actions.wallets.restoreWalletSetConfig.trigger({
      param,
      value: field.value,
    });

  render() {
    const { onClose, onBack, stores } = this.props;
    const { wallets } = stores;
    const { error, isExecuting } = wallets.restoreRequest;
    const { walletName, spendingPassword, repeatPassword } = wallets;
    const {
      error: certificateError,
    } = stores.wallets.getWalletRecoveryPhraseFromCertificateRequest;
    return (
      <ConfigurationDialog
        isSubmitting={isExecuting}
        onClose={onClose}
        onContinue={this.handleContinue}
        onChange={this.handleChange}
        onBack={onBack}
        error={error || certificateError}
        walletName={walletName}
        spendingPassword={spendingPassword}
        repeatPassword={repeatPassword}
      />
    );
  }
}
