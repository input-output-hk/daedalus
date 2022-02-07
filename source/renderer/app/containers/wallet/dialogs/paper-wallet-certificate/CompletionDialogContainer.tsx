import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import CompletionDialog from '../../../../components/wallet/paper-wallet-certificate/CompletionDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import { ADDRESS_COPY_NOTIFICATION_SMALL_DURATION } from '../../../../config/timingConfig';

type Props = InjectedDialogContainerProps;

@inject('stores')
@observer
class CompletionDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { app, wallets } = this.props.stores;
    const {
      environment: { network },
    } = app;
    const { walletCertificateAddress } = wallets;

    if (!walletCertificateAddress) {
      throw new Error(
        'Prop "walletCertificateAddress" is required but was null.'
      );
    }

    return (
      <CompletionDialog
        walletCertificateAddress={walletCertificateAddress}
        onClose={this.props.onClose}
        onOpenExternalLink={this.props.stores.app.openExternalLink}
        copyAddressNotificationDuration={
          ADDRESS_COPY_NOTIFICATION_SMALL_DURATION
        }
        network={network}
      />
    );
  }
}

export default CompletionDialogContainer;
