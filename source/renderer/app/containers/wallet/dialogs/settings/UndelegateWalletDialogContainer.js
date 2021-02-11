// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get, find } from 'lodash';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';
import type { DelegationCalculateFeeResponse } from '../../../../api/staking/types';
import UndelegateWalletConfirmationDialog from '../../../../components/wallet/settings/UndelegateWalletConfirmationDialog';
import UndelegateWalletConfirmationResultDialog from '../../../../components/wallet/settings/UndelegateWalletConfirmationResultDialog';
import { DELEGATION_ACTIONS } from '../../../../config/stakingConfig';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  onExternalLinkClick: Function,
};

type State = {
  stakePoolQuitFee: ?DelegationCalculateFeeResponse,
};

@inject('actions', 'stores')
@observer
export default class UndelegateWalletDialogContainer extends Component<
  Props,
  State
> {
  static defaultProps = { actions: null, stores: null };

  state = {
    stakePoolQuitFee: null,
  };

  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;

    this._handleCalculateTransactionFee();
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  get selectedWalletId() {
    return get(this.props, ['stores', 'wallets', 'active', 'id'], null);
  }

  async _handleCalculateTransactionFee() {
    const { staking, wallets, hardwareWallets } = this.props.stores;
    const { calculateDelegationFee } = staking;

    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === this.selectedWalletId
    );
    const { lastDelegationStakePoolId, delegatedStakePoolId } = selectedWallet;
    const poolId = lastDelegationStakePoolId || delegatedStakePoolId || '';

    let stakePoolQuitFee;
    if (selectedWallet.isHardwareWallet) {
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId: this.selectedWalletId,
        poolId,
        delegationAction: DELEGATION_ACTIONS.QUIT,
      });
      const { feeWithDeposits, fee } = coinsSelection;
      stakePoolQuitFee = {
        fee,
        deposit: feeWithDeposits.minus(fee),
      };
      hardwareWallets.initiateTransaction({ walletId: this.selectedWalletId });
    } else {
      stakePoolQuitFee = await calculateDelegationFee({
        walletId: this.selectedWalletId,
      });
    }

    if (this._isMounted && stakePoolQuitFee) {
      this.setState({ stakePoolQuitFee });
    }
  }

  render() {
    const { actions, stores, onExternalLinkClick } = this.props;
    const {
      wallets,
      staking,
      networkStatus,
      profile,
      hardwareWallets,
    } = stores;
    const { futureEpoch } = networkStatus;
    const { currentLocale } = profile;
    const {
      getStakePoolById,
      quitStakePoolRequest,
      isDelegationTransactionPending,
    } = staking;
    const { getWalletById, undelegateWalletSubmissionSuccess } = wallets;
    const {
      hwDeviceStatus,
      sendMoneyRequest,
      selectCoinsRequest,
    } = hardwareWallets;
    const { stakePoolQuitFee } = this.state;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);

    const walletToBeUndelegated = getWalletById(this.selectedWalletId);
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
        <UndelegateWalletConfirmationResultDialog
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
      <UndelegateWalletConfirmationDialog
        selectedWallet={walletToBeUndelegated}
        stakePoolName={stakePoolName}
        stakePoolTicker={stakePoolTicker}
        onConfirm={(passphrase: ?string, isHardwareWallet: boolean) => {
          actions.wallets.undelegateWallet.trigger({
            walletId: this.selectedWalletId,
            stakePoolId,
            passphrase,
            isHardwareWallet,
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
        submitting={
          quitStakePoolRequest.isExecuting ||
          sendMoneyRequest.isExecuting ||
          isDelegationTransactionPending
        }
        error={
          quitStakePoolRequest.error ||
          sendMoneyRequest.error ||
          selectCoinsRequest.error
        }
        fees={stakePoolQuitFee}
        hwDeviceStatus={hwDeviceStatus}
      />
    );
  }
}
