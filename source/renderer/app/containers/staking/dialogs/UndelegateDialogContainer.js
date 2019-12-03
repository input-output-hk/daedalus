// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import UndelegateConfirmationDialog from '../../../components/staking/delegation-center/UndelegateConfirmationDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class UndelegateDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { uiDialogs, wallets } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { walletId } = dialogData;
    const { quitStakePoolRequest, getWalletById } = wallets;
    const walletToBeUndelegated = getWalletById(walletId);

    if (!walletToBeUndelegated) {
      throw new Error(
        'Delegated wallet should be selected for UndelegateDialogContainer.'
      );
    }

    const {
      name: walletName,
      reward: fees,
      delegatedStakePool,
    } = walletToBeUndelegated;

    if (!delegatedStakePool) {
      throw new Error(
        'Stakepool of delegated wallet is required for UndelegateDialogContainer.'
      );
    }

    const { id: stakePoolId, name: stakePoolName } = delegatedStakePool;

    return (
      <UndelegateConfirmationDialog
        walletName={walletName}
        stakePoolName={stakePoolName}
        onConfirm={passphrase => {
          actions.wallets.undelegateWallet.trigger({
            walletId,
            stakePoolId,
            passphrase,
          });
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          quitStakePoolRequest.reset();
        }}
        isSubmitting={quitStakePoolRequest.isExecuting}
        fees={fees}
      />
    );
  }
}
