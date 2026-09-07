import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get } from 'lodash';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import UndelegateWalletSuccessDialog from '../../../../components/wallet/settings/UndelegateWalletSuccessDialog';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
class UndelegateWalletDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    this.submit();
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  get selectedWalletId() {
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }

  submit = async () => {
    const wallet = this.props.stores.wallets.getWalletById(
      this.selectedWalletId
    );
    if (!wallet) return;
    try {
      await this.props.stores.wallets._undelegateWallet({
        walletId: wallet.id,
        passphrase: '',
        isHardwareWallet: wallet.isHardwareWallet,
      });
    } catch {
      if (this._isMounted)
        this.props.actions.dialogs.closeActiveDialog.trigger();
    }
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets, staking, networkStatus, profile } = stores;
    const { futureEpoch } = networkStatus;
    const { currentLocale } = profile;
    const { quitStakePoolRequest } = staking;
    const { getWalletById, undelegateWalletSubmissionSuccess } = wallets;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);
    const walletToBeUndelegated = getWalletById(this.selectedWalletId);
    if (!walletToBeUndelegated) return null;
    const { name: walletName } = walletToBeUndelegated;

    if (undelegateWalletSubmissionSuccess && !quitStakePoolRequest.error) {
      return (
        <UndelegateWalletSuccessDialog
          walletName={walletName}
          futureEpochStartTime={futureEpochStartTime}
          currentLocale={currentLocale}
          onClose={() => {
            actions.dialogs.closeActiveDialog.trigger();
            quitStakePoolRequest.reset();
            actions.wallets.setUndelegateWalletSubmissionSuccess.trigger({
              result: false,
            });
          }}
        />
      );
    }

    return null;
  }
}

export default UndelegateWalletDialogContainer;
