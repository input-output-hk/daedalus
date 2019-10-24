// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import { get } from 'lodash';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../types/injectedPropsType';
import globalMessages from '../../i18n/global-messages';
import { WalletSyncStateTags } from '../../domains/Wallet';

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
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
    } = this.props.stores;
    const { actions } = this.props;
    const { currentNumberFormatPretty } = profile;
    const { isValidAddress } = wallets;
    const { calculateTransactionFee, validateAmount } = transactions;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    const isRestoreActive =
      get(activeWallet, 'syncState.tag') === WalletSyncStateTags.RESTORING;

    return (
      <WalletSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitAda)}
        validateAmount={validateAmount}
        calculateTransactionFee={(address: string, amount: number) =>
          calculateTransactionFee({
            walletId: activeWallet.id,
            address,
            amount,
          })
        }
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={actions.dialogs.open.trigger}
        isRestoreActive={isRestoreActive}
        onExternalLinkClick={app.openExternalLink}
        currentNumberFormatPretty={currentNumberFormatPretty}
      />
    );
  }
}
