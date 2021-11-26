// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import currencySettingsIcon from '../../../assets/images/currency-settings-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletSummaryCurrency.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletCurrencyAmount } from '../../../utils/formatters';
import type { Currency } from '../../../types/currencyTypes';
import { DiscreetValue } from '../../../features/discreet-mode';

const messages = defineMessages({
  currencyTitle: {
    id: 'wallet.summary.currency.title',
    defaultMessage: '!!!Converts as',
    description: '"Currency - title" label on Wallet summary currency page',
  },
  currencyLastFetched: {
    id: 'wallet.summary.currency.lastFetched',
    defaultMessage: '!!!converted {fetchedTimeAgo}',
    description:
      '"Currency - last fetched" label on Wallet summary currency page',
  },
  currencyIsFetchingRate: {
    id: 'wallet.summary.currency.isFetchingRate',
    defaultMessage: '!!!fetching conversion rates',
    description: '"Currency - Fetching" label on Wallet summary currency page',
  },
});

type Props = {
  wallet: Wallet,
  currencyIsFetchingRate: boolean,
  currencyIsActive: boolean,
  currencySelected: ?Currency,
  currencyRate: ?number,
  currencyLastFetched: ?Date,
  onCurrencySettingClick: Function,
};

@observer
export default class WalletSummaryCurrency extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      currencyIsActive,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
    } = this.props;
    const { intl } = this.context;

    const isRestoreActive = wallet.isRestoring;
    const hasCurrency =
      currencyIsActive &&
      !!currencySelected &&
      (!!currencyRate || currencyIsFetchingRate);

    const { decimalDigits } = currencySelected || {};

    let currencyWalletAmount;
    if (isRestoreActive || !currencyRate) currencyWalletAmount = '–';
    else if (hasCurrency && currencyRate)
      currencyWalletAmount = formattedWalletCurrencyAmount(
        wallet.amount,
        currencyRate,
        decimalDigits
      );
    const currencyWalletAmountSymbol =
      currencySelected && currencySelected.code
        ? currencySelected.code.toUpperCase()
        : '';
    const fetchedTimeAgo = moment(currencyLastFetched)
      .locale(intl.locale)
      .fromNow();

    const buttonStyles = classnames([
      styles.currencyLastFetched,
      currencyIsFetchingRate ? styles.currencyIsFetchingRate : null,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.currencyTitle}>
          {intl.formatMessage(messages.currencyTitle)}
        </div>
        <div className={styles.currencyWalletAmount}>
          <DiscreetValue>{currencyWalletAmount}</DiscreetValue>
          <span className={styles.currencyCode}>
            {' '}
            {currencyWalletAmountSymbol}
          </span>
        </div>
        <div className={styles.currencyRate}>
          1 {intl.formatMessage(globalMessages.adaUnit)} = {currencyRate || '–'}{' '}
          {currencyWalletAmountSymbol}
        </div>
        <button className={buttonStyles} onClick={onCurrencySettingClick}>
          <em>
            {currencyIsFetchingRate
              ? intl.formatMessage(messages.currencyIsFetchingRate)
              : intl.formatMessage(messages.currencyLastFetched, {
                  fetchedTimeAgo,
                })}
          </em>
          <SVGInline
            svg={currencySettingsIcon}
            className={styles.currencySettingsIcon}
          />
        </button>
      </div>
    );
  }
}
