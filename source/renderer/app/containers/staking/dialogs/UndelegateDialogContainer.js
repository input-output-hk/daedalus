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
    const { nextEpoch } = networkStatus;
    const { currentLocale } = profile;
    const {
      quitStakePoolRequest,
      getWalletById,
      undelegateWalletSubmissionSuccess,
    } = wallets;
    const nextEpochStartTime = get(nextEpoch, 'epochStart', 0);

    const walletToBeUndelegated = getWalletById(walletId);
    if (!walletToBeUndelegated)
      return null;

    const { name: walletName, delegatedStakePoolId } = walletToBeUndelegated;
    const delegatedStakePool = staking.getStakePoolById(delegatedStakePoolId);

    if (undelegateWalletSubmissionSuccess) {
      return (
        <UndelegateConfirmationResultDialog
          walletName={walletName}
          nextEpochStartTime={nextEpochStartTime}
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

    if (!delegatedStakePool)
      return null;

    const {
      id: stakePoolId,
      name: stakePoolName,
      ticker: stakePoolTicker,
    } = delegatedStakePool;

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
        isSubmitting={quitStakePoolRequest.isExecuting}
        error={quitStakePoolRequest.error}
        fees={stakePoolQuitFee}
      />
    );
  }
}
