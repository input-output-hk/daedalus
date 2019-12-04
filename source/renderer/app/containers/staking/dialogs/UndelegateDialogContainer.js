// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import UndelegateConfirmationDialog from '../../../components/staking/delegation-center/UndelegateConfirmationDialog';
import UndelegateConfirmationResultDialog from '../../../components/staking/delegation-center/UndelegateConfirmationResultDialog';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  onExternalLinkClick: Function,
};

@inject('actions', 'stores')
@observer
export default class UndelegateDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores, onExternalLinkClick } = this.props;
    const { uiDialogs, wallets } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { walletId } = dialogData;
    const {
      quitStakePoolRequest,
      getWalletById,
      undelegateWalletSubmissionSuccess,
    } = wallets;
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

    if (undelegateWalletSubmissionSuccess) {
      return (
        <UndelegateConfirmationResultDialog
          walletName={walletName}
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

    if (!delegatedStakePool) {
      throw new Error(
        'Stakepool of delegated wallet is required for UndelegateDialogContainer.'
      );
    }

    const {
      id: stakePoolId,
      name: stakePoolName,
      slug: stakePoolSlug,
    } = delegatedStakePool;

    return (
      <UndelegateConfirmationDialog
        walletName={walletName}
        stakePoolName={stakePoolName}
        stakePoolSlug={stakePoolSlug}
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
          actions.wallets.setUndelegateWalletSubmissionSuccess.trigger({
            result: false,
          });
        }}
        onExternalLinkClick={onExternalLinkClick}
        isSubmitting={quitStakePoolRequest.isExecuting}
        error={quitStakePoolRequest.error}
        fees={fees}
      />
    );
  }
}
