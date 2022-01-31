// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletCreateDialog from '../../../components/wallet/WalletCreateDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class WalletCreateDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onSubmit = (values: { name: string; spendingPassword: string }) => {
    this.props.actions.wallets.createWallet.trigger(values);
  };

  render() {
    const { isShelleyActivated } = this.props.stores.networkStatus;
    const { currentLocale } = this.props.stores.profile;
    return (
      <WalletCreateDialog
        onSubmit={this.onSubmit}
        onCancel={this.props.onClose}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        isShelleyActivated={isShelleyActivated}
        currentLocale={currentLocale}
      />
    );
  }
}

export default WalletCreateDialogContainer;
