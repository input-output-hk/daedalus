// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import CompletionDialog from '../../../../components/wallet/paper-wallet-certificate/CompletionDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import { ADDRESS_COPY_NOTIFICATION_SMALL_DURATION } from '../../../../config/timingConfig';

type Props = InjectedDialogContainerProps;

@inject('stores') @observer
export default class CompletionDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  render() {
    const { walletCertificateAddress } = this.props.stores.wallets;

    return (
      <CompletionDialog
        walletCertificateAddress={walletCertificateAddress}
        onClose={this.props.onClose}
        onOpenExternalLink={this.props.stores.app.openExternalLink}
        copyAddressNotificationDuration={ADDRESS_COPY_NOTIFICATION_SMALL_DURATION}
      />
    );
  }
}
