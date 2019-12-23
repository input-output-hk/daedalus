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
    const { /* onContinue, */ actions } = this.props;
    const { restoreWalletSetConfig } = actions.wallets;
    restoreWalletSetConfig.trigger({ walletName, spendingPassword });
    // onContinue();
  };

  render() {
    const { onClose, onContinue, onBack } = this.props;
    return (
      <ConfigurationDialog
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
      />
    );
  }
}
