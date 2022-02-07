import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TransferFundsStep1Dialog from '../../../../components/wallet/transfer-funds/TransferFundsStep1Dialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class TransferFundsStep1Container extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, actions, onClose, onContinue } = this.props;
    const {
      transferFundsCalculateFeeRequest,
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
      allLegacyWallets,
      allWallets,
    } = stores.wallets;
    const { stakePools, getStakePoolById } = stores.staking;
    const { transferFundsSetTargetWalletId } = actions.wallets;
    const sourceWallet = allLegacyWallets.find(
      ({ id }) => id === transferFundsSourceWalletId
    );
    if (!sourceWallet || !transferFundsTargetWalletId) return null;
    return (
      <TransferFundsStep1Dialog
        targetWalletId={transferFundsTargetWalletId}
        sourceWallet={sourceWallet}
        wallets={allWallets}
        onClose={onClose}
        onContinue={onContinue}
        onSetSourceWallet={(targetWalletId: string) =>
          transferFundsSetTargetWalletId.trigger({
            targetWalletId,
          })
        }
        numberOfStakePools={stakePools.length}
        getStakePoolById={getStakePoolById}
        isSubmitting={transferFundsCalculateFeeRequest.isExecuting}
        error={transferFundsCalculateFeeRequest.error}
      />
    );
  }
}

export default TransferFundsStep1Container;
