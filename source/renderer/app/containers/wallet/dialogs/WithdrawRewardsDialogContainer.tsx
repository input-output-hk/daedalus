import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WithdrawRewardsDialog from '../../../components/wallet/withdraw-rewards/WithdrawRewardsDialog';
import type LocalizableError from '../../../i18n/LocalizableError';
import type { InjectedProps } from '../../../types/injectedPropsType';

type Props = InjectedProps & {
  walletId: string;
  onExternalLinkClick: (url: string) => void;
};

@inject('actions', 'stores')
@observer
class WithdrawRewardsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  _isMounted = false;
  _ownsHardwareTransaction = false;

  componentDidMount() {
    this._isMounted = true;
    this._prepare();
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.cancelHardwareTransaction();
    this.props.stores.wallets.cancelRewardWithdrawal(this.props.walletId);
  }

  get wallet() {
    return this.props.stores.wallets.getWalletById(this.props.walletId);
  }

  cancelHardwareTransaction = () => {
    if (!this._ownsHardwareTransaction) return;
    this._ownsHardwareTransaction = false;
    this.props.stores.hardwareWallets._resetTransaction({
      cancelDeviceAction: true,
    });
  };

  // The wallet builds the transaction; a hardware wallet then starts the
  // device flow for the selection that was built, as the send confirmation
  // does.
  _prepare = async () => {
    const { stores, walletId } = this.props;
    const { wallets, hardwareWallets } = stores;
    const wallet = this.wallet;
    if (!wallet) return;
    if (wallet.isHardwareWallet) this._ownsHardwareTransaction = true;
    await wallets.prepareRewardWithdrawal(walletId);
    if (
      this._isMounted &&
      this._ownsHardwareTransaction &&
      wallets.rewardWithdrawalFor(walletId)?.status === 'ready'
    ) {
      hardwareWallets
        .initiateTransaction({ walletId })
        .catch((error: LocalizableError) => this._fail(error));
    }
  };

  // The step draws an error that has no message id as an internal error.
  _fail = (error: LocalizableError) => {
    if (!this._isMounted) return;
    this.props.stores.wallets._setRewardWithdrawal(this.props.walletId, {
      status: 'failed',
      error,
    });
  };

  _submit = (passphrase: string | undefined) => {
    const { actions, stores, walletId } = this.props;
    if (this.wallet?.isHardwareWallet) {
      // The hardware store submits the signed transaction and closes the
      // dialog once the ledger holds it.
      actions.hardwareWallets.sendMoney.trigger().catch(this._fail);
      return;
    }
    stores.wallets.submitRewardWithdrawal(walletId, passphrase).catch(() => {
      // The step records its own failure.
    });
  };

  _cancel = () => {
    this.cancelHardwareTransaction();
    this.props.stores.wallets.cancelRewardWithdrawal(this.props.walletId);
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores, walletId, onExternalLinkClick } = this.props;
    const { wallets, hardwareWallets } = stores;
    const wallet = this.wallet;
    if (!wallet) return null;
    return (
      <WithdrawRewardsDialog
        walletName={wallet.name}
        isHardwareWallet={wallet.isHardwareWallet}
        step={wallets.rewardWithdrawalFor(walletId)}
        isSubmitting={
          wallet.isHardwareWallet &&
          (hardwareWallets.sendMoneyRequest.isExecuting ||
            hardwareWallets.isTransactionPending)
        }
        hwDeviceStatus={hardwareWallets.hwDeviceStatus}
        isTrezor={hardwareWallets.checkIsTrezorByWalletId(walletId)}
        onSubmit={this._submit}
        onCancel={this._cancel}
        onExternalLinkClick={onExternalLinkClick}
      />
    );
  }
}

export default WithdrawRewardsDialogContainer;
