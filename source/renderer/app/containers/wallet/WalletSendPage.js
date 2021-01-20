// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../types/injectedPropsType';
import globalMessages from '../../i18n/global-messages';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import WalletTokenSendForm from '../../components/wallet/WalletTokenSendForm';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSendPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  calculateTransactionFee = async (params: {
    walletId: string,
    address: string,
    amount: number,
    isHardwareWallet: boolean,
  }) => {
    const { walletId, address, amount, isHardwareWallet } = params;
    let fee;
    if (isHardwareWallet) {
      const coinsSelection = await this.props.stores.hardwareWallets.selectCoins(
        {
          walletId,
          address,
          amount,
        }
      );
      fee = coinsSelection.fee;
    } else {
      fee = await this.props.stores.transactions.calculateTransactionFee({
        walletId,
        address,
        amount,
      });
    }
    return fee;
  };

  openDialog = (
    dialog: Function,
    isHardwareWallet: boolean,
    walletId: string
  ) => {
    const { isFlight } = global;
    this.props.actions.dialogs.open.trigger({
      dialog,
    });
    if (isHardwareWallet && !isFlight) {
      this.props.stores.hardwareWallets.initiateTransaction({ walletId });
    }
  };

  render() {
    const { intl } = this.context;
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
      hardwareWallets,
    } = this.props.stores;
    const { isValidAddress } = wallets;
    const { validateAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const activeWallet = wallets.active;
    let nativeTokens = wallets.all.filter(
      (wallet) => wallet.isNativeTokenWallet
    );

    // @todo - Remove hardcoded nativeTokens value after hooking up real data
    if (!nativeTokens.length) {
      nativeTokens = [wallets.all[0]];
    }

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');
    const { isHardwareWallet } = activeWallet;

    return (
      <>
        {nativeTokens && nativeTokens.length ? (
          <WalletTokenSendForm
            currencyUnit={intl.formatMessage(globalMessages.unitAda)}
            currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
            currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
            currentNumberFormat={profile.currentNumberFormat}
            validateAmount={validateAmount}
            calculateTransactionFee={(address: string, amount: number) =>
              this.calculateTransactionFee({
                walletId: activeWallet.id,
                address,
                amount,
                isHardwareWallet,
              })
            }
            nativeTokens={nativeTokens}
            walletAmount={activeWallet.amount}
            addressValidator={isValidAddress}
            isDialogOpen={uiDialogs.isOpen}
            openDialogAction={(params) =>
              this.openDialog(params.dialog, isHardwareWallet, activeWallet.id)
            }
            isRestoreActive={activeWallet.isRestoring}
            onExternalLinkClick={app.openExternalLink}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
          />
        ) : (
          <WalletSendForm
            currencyUnit={intl.formatMessage(globalMessages.unitAda)}
            currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
            currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
            currentNumberFormat={profile.currentNumberFormat}
            validateAmount={validateAmount}
            calculateTransactionFee={(address: string, amount: number) =>
              this.calculateTransactionFee({
                walletId: activeWallet.id,
                address,
                amount,
                isHardwareWallet,
              })
            }
            addressValidator={isValidAddress}
            isDialogOpen={uiDialogs.isOpen}
            openDialogAction={(params) =>
              this.openDialog(params.dialog, isHardwareWallet, activeWallet.id)
            }
            isRestoreActive={activeWallet.isRestoring}
            onExternalLinkClick={app.openExternalLink}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
          />
        )}
      </>
    );
  }
}
