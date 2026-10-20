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
  onExternalLinkClick: (url: string) => void;
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

    if (global.cip30Enabled) this.submit();
    else
      this._handleCalculateTransactionFee().catch(() => {
        // Request errors are displayed by the confirmation dialog.
      });
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

  async _handleCalculateTransactionFee() {
    const { staking, wallets, hardwareWallets } = this.props.stores;
    const { calculateDelegationFee } = staking;
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === this.selectedWalletId
    );
    if (!selectedWallet) return;
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
      if (!this._isMounted) return;
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
    const { wallets, staking, networkStatus, profile, hardwareWallets } =
      stores;
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
    const { lastDelegatedStakePoolId, delegatedStakePoolId } =
      walletToBeUndelegated;
    const stakePoolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';

    if (
      (global.cip30Enabled ||
        !stakePoolId ||
        !isDelegationTransactionPending) &&
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

    if (global.cip30Enabled) return null;

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
          hardwareWallets._resetTransaction({ cancelDeviceAction: true });
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
