// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get } from 'lodash';
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
    const { uiDialogs, wallets, staking, networkStatus, profile } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { walletId, stakePoolQuitFee } = dialogData;
    const { futureEpoch } = networkStatus;
    const { currentLocale } = profile;
    const {
      getStakePoolById,
      quitStakePoolRequest,
      isDelegationTransactionPending,
    } = staking;
    const { getWalletById, undelegateWalletSubmissionSuccess } = wallets;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);

    const walletToBeUndelegated = getWalletById(walletId);
    if (!walletToBeUndelegated) return null;

    const { name: walletName } = walletToBeUndelegated;

    const {
      lastDelegationStakePoolId,
      delegatedStakePoolId,
    } = walletToBeUndelegated;

    const stakePoolId = lastDelegationStakePoolId || delegatedStakePoolId || '';

    if (
      (!stakePoolId || !isDelegationTransactionPending) &&
      undelegateWalletSubmissionSuccess &&
      !quitStakePoolRequest.error
    ) {
      return (
        <UndelegateConfirmationResultDialog
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

    const delegatedStakePool = getStakePoolById(stakePoolId);
    const stakePoolName = get(delegatedStakePool, 'name', '');
    const stakePoolTicker = get(delegatedStakePool, 'ticker');

    return (
      <UndelegateConfirmationDialog
        walletName={walletName}
        stakePoolName={stakePoolName}
        stakePoolTicker={stakePoolTicker}
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
        isSubmitting={
          quitStakePoolRequest.isExecuting || isDelegationTransactionPending
        }
        error={quitStakePoolRequest.error}
        fees={stakePoolQuitFee}
      />
    );
  }
}
