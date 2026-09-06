import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get, find } from 'lodash';
import BigNumber from 'bignumber.js';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import type { DelegationCalculateFeeResponse } from '../../../../api/staking/types';
import UndelegateWalletConfirmationDialog from '../../../../components/wallet/settings/UndelegateWalletConfirmationDialog';
import UndelegateWalletSuccessDialog from '../../../../components/wallet/settings/UndelegateWalletSuccessDialog';
import {
  DELEGATION_ACTIONS,
  DELEGATION_DEPOSIT,
} from '../../../../config/stakingConfig';

type Props = InjectedProps & {
  onExternalLinkClick: (...args: Array<any>) => any;
};
type State = {
  stakePoolQuitFee: DelegationCalculateFeeResponse | null | undefined;
};

@inject('actions', 'stores')
@observer
class UndelegateWalletDialogContainer extends Component<Props, State> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
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
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }

  async _handleCalculateTransactionFee() {
    const { staking, wallets, hardwareWallets } = this.props.stores;
    const { calculateDelegationFee } = staking;
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === this.selectedWalletId
    );
    const { lastDelegatedStakePoolId, delegatedStakePoolId } = selectedWallet;
    const poolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';
    let stakePoolQuitFee;

    if (selectedWallet.isHardwareWallet) {
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId: this.selectedWalletId,
        poolId,
        delegationAction: DELEGATION_ACTIONS.QUIT,
      });
      const { deposits, depositsReclaimed, fee } = coinsSelection;
      stakePoolQuitFee = {
        deposits,
        depositsReclaimed,
        fee,
      };
      hardwareWallets.initiateTransaction({
        walletId: this.selectedWalletId,
      });
    } else {
      stakePoolQuitFee = await calculateDelegationFee({
        walletId: this.selectedWalletId,
      });

      // @TODO Remove this when api returns depositsReclaimed value
      if (stakePoolQuitFee) {
        stakePoolQuitFee.depositsReclaimed = new BigNumber(DELEGATION_DEPOSIT);
      }
    }

    if (this._isMounted && stakePoolQuitFee) {
      this.setState({
        stakePoolQuitFee,
      });
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
      checkIsTrezorByWalletId,
    } = hardwareWallets;
    const { stakePoolQuitFee } = this.state;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);
    const walletToBeUndelegated = getWalletById(this.selectedWalletId);
    if (!walletToBeUndelegated) return null;
    const isTrezor = checkIsTrezorByWalletId(walletToBeUndelegated.id);
    const { name: walletName } = walletToBeUndelegated;
    const {
      lastDelegatedStakePoolId,
      delegatedStakePoolId,
    } = walletToBeUndelegated;
    const stakePoolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';

    if (
      (!stakePoolId || !isDelegationTransactionPending) &&
      undelegateWalletSubmissionSuccess &&
      !quitStakePoolRequest.error
    ) {
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

    const delegatedStakePool = getStakePoolById(stakePoolId);
    const stakePoolName = get(delegatedStakePool, 'name', '');
    const stakePoolTicker = get(delegatedStakePool, 'ticker');
    return (
      <UndelegateWalletConfirmationDialog
        selectedWallet={walletToBeUndelegated}
        stakePoolName={stakePoolName}
        stakePoolTicker={stakePoolTicker}
        onConfirm={(passphrase: string, isHardwareWallet: boolean) => {
          actions.wallets.undelegateWallet.trigger({
            walletId: this.selectedWalletId,
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
        isSubmitting={
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
        isTrezor={isTrezor}
      />
    );
  }
}

export default UndelegateWalletDialogContainer;
