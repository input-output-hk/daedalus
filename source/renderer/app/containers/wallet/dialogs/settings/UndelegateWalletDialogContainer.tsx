import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { get, find } from 'lodash';
import BigNumber from 'bignumber.js';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import type { DelegationCalculateFeeResponse } from '../../../../api/staking/types';
import UndelegateWalletConfirmationDialog from '../../../../components/wallet/settings/UndelegateWalletConfirmationDialog';
import UndelegateWalletVoteFirstDialog from '../../../../components/wallet/settings/UndelegateWalletVoteFirstDialog';
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
  _ownsHardwareTransaction = false;
  _undelegationStarted = false;

  cancelHardwareTransaction = () => {
    if (!this._ownsHardwareTransaction) return;
    this._ownsHardwareTransaction = false;
    this.props.stores.hardwareWallets._resetTransaction({
      cancelDeviceAction: true,
    });
  };

  componentDidMount() {
    this._isMounted = true;
    this._proceed();
  }

  componentDidUpdate() {
    this._proceed();
  }

  get needsVoteDelegationFirst() {
    const { wallets } = this.props.stores;
    const walletId = this.selectedWalletId;
    if (walletId == null || !wallets.getWalletById(walletId)) return false;
    return (
      wallets.withdrawalDecisionFor(walletId, 'undelegation').kind ===
      'delegate_vote_first'
    );
  }

  // Starts the vote delegation while the ledger holds none, and the
  // undelegation once it does, which is also when the wait for the ledger ends.
  _proceed() {
    if (this._undelegationStarted) return;
    const walletId = this.selectedWalletId;
    const { wallets } = this.props.stores;
    const wallet = walletId == null ? null : wallets.getWalletById(walletId);
    if (!wallet) return;
    if (this.needsVoteDelegationFirst) {
      const step = wallets.undelegationVoteStepFor(walletId);
      if (step == null && this._isMounted) this._prepareVoteStep(wallet);
      return;
    }
    this._undelegationStarted = true;
    // The device state left by the vote delegation must not enable the
    // undelegation's confirmation before its own transaction is prepared.
    this.cancelHardwareTransaction();
    if (global.cip30Enabled) this.submit();
    else
      this._handleCalculateTransactionFee().catch(() => {
        // Request errors are displayed by the confirmation dialog.
      });
  }

  _prepareVoteStep = (wallet: { id: string; isHardwareWallet: boolean }) => {
    if (wallet.isHardwareWallet) this._ownsHardwareTransaction = true;
    this.props.stores.wallets.prepareUndelegationVote(wallet.id).catch(() => {
      // The step records its own failure.
    });
  };

  _retryVoteStep = () => {
    const wallet = this.props.stores.wallets.getWalletById(
      this.selectedWalletId
    );
    if (wallet) this._prepareVoteStep(wallet);
  };

  _cancelVoteStep = () => {
    const { actions, stores } = this.props;
    stores.wallets.cancelUndelegationVote(this.selectedWalletId);
    this.cancelHardwareTransaction();
    actions.dialogs.closeActiveDialog.trigger();
  };

  componentWillUnmount() {
    this._isMounted = false;
    this.cancelHardwareTransaction();
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
    const walletId = this.selectedWalletId;
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === walletId
    );
    if (!selectedWallet) return;
    const { lastDelegatedStakePoolId, delegatedStakePoolId } = selectedWallet;
    const poolId = lastDelegatedStakePoolId || delegatedStakePoolId || '';
    let stakePoolQuitFee;

    if (selectedWallet.isHardwareWallet) {
      this._ownsHardwareTransaction = true;
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId,
        poolId,
        delegationAction: DELEGATION_ACTIONS.QUIT,
      });
      const { deposits, depositsReclaimed, fee } = coinsSelection;
      stakePoolQuitFee = {
        deposits,
        depositsReclaimed,
        fee,
      };
      if (!this._isMounted || !this._ownsHardwareTransaction) return;
      hardwareWallets.initiateTransaction({
        walletId,
      });
    } else {
      stakePoolQuitFee = await calculateDelegationFee({
        walletId,
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

    if (this.needsVoteDelegationFirst && !this._undelegationStarted) {
      return (
        <UndelegateWalletVoteFirstDialog
          walletName={walletName}
          heldReward={walletToBeUndelegated.reward}
          isHardwareWallet={walletToBeUndelegated.isHardwareWallet}
          step={wallets.undelegationVoteStepFor(walletToBeUndelegated.id)}
          hwDeviceStatus={hwDeviceStatus}
          isTrezor={isTrezor}
          onSubmit={(passphrase) => {
            wallets
              .submitUndelegationVote(walletToBeUndelegated.id, passphrase)
              .catch(() => {
                // The step records its own failure.
              });
          }}
          onRetry={this._retryVoteStep}
          onCancel={this._cancelVoteStep}
          onClose={() => actions.dialogs.closeActiveDialog.trigger()}
          onExternalLinkClick={onExternalLinkClick}
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
          this.cancelHardwareTransaction();
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
