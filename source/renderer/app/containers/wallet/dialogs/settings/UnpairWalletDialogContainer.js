// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import type { WalletSettingActionMessages } from '../../../../components/wallet/settings/WalletSettingsActionConfirmationDialog';
import WalletSettingsActionConfirmationDialog from '../../../../components/wallet/settings/WalletSettingsActionConfirmationDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;
const messages: WalletSettingActionMessages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.unpair.dialog.title',
    defaultMessage: '!!!Unpair Wallet',
    description: 'Title for the "Unpair wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.unpair.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description:
      'Label for the "Unpair (x)" button in the unpair wallet dialog.',
  },
  confirmationQuestion: {
    id: 'wallet.settings.unpair.dialog.confirmationQuestion',
    defaultMessage:
      '!!!Do you really want to unpair <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to unpair the wallet.',
  },
});

@inject('actions', 'stores')
@observer
export default class UnpairWalletDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { uiDialogs, wallets, app } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { deleteWalletRequest, isDeleting } = wallets;
    const {
      environment: { isTest },
    } = app;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error(
        'Active wallet required for DeleteWalletDialogContainer.'
      );

    return (
      <WalletSettingsActionConfirmationDialog
        isTest={isTest}
        isUnpair
        walletName={activeWallet.name}
        hasWalletFunds={activeWallet.hasFunds}
        countdownFn={uiDialogs.countdownSinceDialogOpened}
        isBackupNoticeAccepted={dialogData.isBackupNoticeAccepted}
        messages={Object.freeze(messages)}
        onAcceptBackupNotice={() =>
          updateDataForActiveDialog.trigger({
            data: { isBackupNoticeAccepted: true },
          })
        }
        onContinue={() => {
          actions.wallets.deleteWallet.trigger({
            walletId: activeWallet.id,
            isLegacy: activeWallet.isLegacy,
          });
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          deleteWalletRequest.reset();
        }}
        confirmationValue={dialogData.confirmationValue}
        onConfirmationValueChange={(confirmationValue) =>
          updateDataForActiveDialog.trigger({
            data: { confirmationValue },
          })
        }
        isSubmitting={isDeleting}
      />
    );
  }
}
