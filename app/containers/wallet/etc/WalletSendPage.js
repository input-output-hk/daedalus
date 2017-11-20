// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import WalletSendForm from '../../../components/wallet/etc/WalletSendForm';
import type { InjectedProps } from '../../../types/injectedPropsType';
import globalMessages from '../../../i18n/global-messages';
import { DECIMAL_PLACES_IN_ETC } from '../../../config/numbersConfig';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { uiDialogs } = this.props.stores;
    const { wallets } = this.props.stores.etc;
    const { calculateTransactionFee, isValidAmount, isValidAddress } = wallets;
    const { actions } = this.props;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitEtc)}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ETC}
        validateAmount={isValidAmount}
        calculateTransactionFee={(receiver, amount) => (
          calculateTransactionFee({ sender: activeWallet.id, receiver, amount })
        )}
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={actions.dialogs.open.trigger}
      />
    );
  }

}
