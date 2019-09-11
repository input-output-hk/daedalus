// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
import { getWalletLocalData } from '../../utils/walletLocalStorage.js';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import DeleteWalletDialogContainer from './dialogs/settings/DeleteWalletDialogContainer';
import ExportWalletToFileDialogContainer from './dialogs/settings/ExportWalletToFileDialogContainer';
import WalletRecoveryPhraseStep1Container from './dialogs/settings/WalletRecoveryPhraseStep1Container';
import WalletRecoveryPhraseStep2Container from './dialogs/settings/WalletRecoveryPhraseStep2Container';

type Props = InjectedProps;

type State = {
  mnemonicsConfirmationDate?: Date,
};

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null };

  state = {
    mnemonicsConfirmationDate: null,
  };

  componentDidMount() {
    this.getWalletLocalData();
  }

  getWalletLocalData = async () => {
    const { id } = this.activeWallet;
    const { mnemonicsConfirmationDate } = await getWalletLocalData(id);
    this.setState({ mnemonicsConfirmationDate });
  };

  get activeWallet() {
    const { active: activeWallet } = this.props.stores.wallets;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');

    return activeWallet;
  }

  render() {
    const { uiDialogs, walletSettings, app } = this.props.stores;
    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const { activeWallet } = this;
    const {
      WALLET_ASSURANCE_LEVEL_OPTIONS,
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
    } = actions.walletSettings;
    const { mnemonicsConfirmationDate } = this.state;

    return (
      <WalletSettings
        assuranceLevels={WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={activeWallet.assurance}
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        isSpendingPasswordSet={activeWallet.hasPassword}
        spendingPasswordUpdateDate={activeWallet.passwordUpdateDate}
        mnemonicsConfirmationDate={mnemonicsConfirmationDate}
        isDialogOpen={uiDialogs.isOpen}
        walletId={activeWallet.id}
        walletName={activeWallet.name}
        walletCreationDate={activeWallet.createdAt}
        isSubmitting={updateWalletRequest.isExecuting}
        isInvalid={
          updateWalletRequest.wasExecuted &&
          updateWalletRequest.result === false
        }
        showExportLink={!isProduction}
        lastUpdatedField={lastUpdatedWalletField}
        onFieldValueChange={(field, value) =>
          updateWalletField.trigger({ field, value })
        }
        onStartEditing={field => startEditingWalletField.trigger({ field })}
        onStopEditing={stopEditingWalletField.trigger}
        onCancelEditing={cancelEditingWalletField.trigger}
        activeField={walletFieldBeingEdited}
        nameValidator={name => isValidWalletName(name)}
        changeSpendingPasswordDialog={<ChangeSpendingPasswordDialogContainer />}
        deleteWalletDialogContainer={<DeleteWalletDialogContainer />}
        exportWalletDialogContainer={<ExportWalletToFileDialogContainer />}
        walletRecoveryPhraseStep1Container={
          <WalletRecoveryPhraseStep1Container />
        }
        walletRecoveryPhraseStep2Container={
          <WalletRecoveryPhraseStep2Container />
        }
      />
    );
  }
}
