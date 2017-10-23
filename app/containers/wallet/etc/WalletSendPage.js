// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import BigNumber from 'bignumber.js';
import { intlShape } from 'react-intl';
import WalletSendForm from '../../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../../types/injectedPropsType';
import globalMessages from '../../../i18n/global-messages';
import { DECIMAL_PLACES_IN_ETC } from '../../../config/numbersConfig';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { uiDialogs } = this.props.stores;
    const { wallets } = this.props.stores.etc;
    const { actions } = this.props;
    const activeWallet = wallets.active;

    // TODO: replace with real endpoints!!
    const isValidAddress = () => Promise.resolve(true);
    const calculateTransactionFee = () => Promise.resolve(new BigNumber(0));
    const validateAmount = () => Promise.resolve(true);

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitEtc)}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ETC}
        validateAmount={validateAmount}
        calculateTransactionFee={(receiver, amount) => (
          calculateTransactionFee(activeWallet.id, receiver, amount)
        )}
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={actions.dialogs.open.trigger}
      />
    );
  }

}
