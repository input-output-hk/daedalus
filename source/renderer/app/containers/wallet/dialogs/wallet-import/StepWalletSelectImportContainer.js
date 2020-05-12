// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { map } from 'lodash';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import WalletSelectImportDialog from '../../../../components/wallet/wallet-import/WalletSelectImportDialog';
import { WalletImportStatuses } from '../../../../types/walletExportTypes';
import { MAX_ADA_WALLETS_COUNT } from '../../../../config/numbersConfig';
import { isValidWalletName } from '../../../../utils/validations';

type Props = InjectedProps;

type State = {
  existingWalletsCount: number,
};

@inject('stores', 'actions')
@observer
export default class StepWalletSelectImportContainer extends Component<
  Props,
  State
> {
  static defaultProps = { actions: null, stores: null };

  state = {
    existingWalletsCount: this.props.stores.wallets.all.length,
  };

  onWalletNameChange = (params: { index: number, name: string }) => {
    this.props.actions.walletMigration.updateWalletName.trigger(params);
  };

  onToggleWalletImportSelection = (params: { index: number }) => {
    this.props.actions.walletMigration.toggleWalletImportSelection.trigger(
      params
    );
  };

  render() {
    const { onClose, onContinue, stores } = this.props;
    const { walletMigration, app } = stores;

    const {
      exportedWallets,
      pendingImportWalletsCount,
      isRestorationRunning,
    } = walletMigration;

    const { openExternalLink } = app;

    let walletsCount =
      this.state.existingWalletsCount + pendingImportWalletsCount;
    map(exportedWallets, wallet => {
      if (
        wallet.import.status === WalletImportStatuses.COMPLETED ||
        wallet.import.status === WalletImportStatuses.RUNNING
      ) {
        walletsCount++;
      }
    });

    const isMaxNumberOfWalletsReached = walletsCount >= MAX_ADA_WALLETS_COUNT;

    return (
      <WalletSelectImportDialog
        onClose={onClose}
        onContinue={onContinue}
        isSubmitting={isRestorationRunning}
        nameValidator={name => isValidWalletName(name)}
        exportedWallets={exportedWallets}
        pendingImportWalletsCount={pendingImportWalletsCount}
        onOpenExternalLink={openExternalLink}
        onWalletNameChange={this.onWalletNameChange}
        onToggleWalletImportSelection={this.onToggleWalletImportSelection}
        isMaxNumberOfWalletsReached={isMaxNumberOfWalletsReached}
      />
    );
  }
}
