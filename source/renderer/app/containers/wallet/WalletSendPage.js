// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import { get } from 'lodash';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../types/injectedPropsType';
import globalMessages from '../../i18n/global-messages';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import { WalletSyncStateStatuses } from '../../domains/Wallet';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSendPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { uiDialogs, wallets, transactions, app } = this.props.stores;
    const { actions } = this.props;
    const { isValidAddress } = wallets;
    const { calculateTransactionFee, validateAmount } = transactions;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    const isRestoreActive =
      get(activeWallet, ['syncState', 'status'], '') ===
      WalletSyncStateStatuses.RESTORING;

    return (
      <WalletSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitAda)}
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        validateAmount={validateAmount}
        calculateTransactionFee={(address: string, amount: number) =>
          calculateTransactionFee({
            walletId: activeWallet.id,
            address,
            amount,
          })
        }
        walletAmount={activeWallet.amount}
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={actions.dialogs.open.trigger}
        isRestoreActive={isRestoreActive}
        onExternalLinkClick={app.openExternalLink}
      />
    );
  }
}
