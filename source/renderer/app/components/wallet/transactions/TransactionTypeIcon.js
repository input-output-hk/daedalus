// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import moment from 'moment';
import { LoadingSpinner } from 'react-polymorph/lib/components/LoadingSpinner';
import { LoadingSpinnerSkin } from 'react-polymorph/lib/skins/simple/LoadingSpinnerSkin';
import styles from './TransactionTypeIcon.scss';
import spinnerOverrides from './SpinnerOverrides.scss';
import expendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import incomeIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import exchangeIcon from '../../../assets/images/exchange-ic.inline.svg';
import pendingIcon from '../../../assets/images/wallet-nav/pending.inline.svg';
import { PENDING_LIMIT } from '../../../config/txnsConfig';
import {
  TransactionTypes,
  TransactionStates,
} from '../../../domains/WalletTransaction';

type Props = {
  iconType: string,
  txnDate: Date,
};

export default class TransactionTypeIcon extends Component<Props> {
  getTimePending = (txnDate: Date): number => {
    // right now (milliseconds) minus txn created_at date (milliseconds)
    const NOW = moment().valueOf();
    const TXN_PENDING_SINCE = moment(txnDate).valueOf();
    return NOW - TXN_PENDING_SINCE;
  };

  applyIconStyles = (iconType: string): string => {
    if (iconType !== TransactionStates.PENDING) {
      return iconType;
    }
    const TIME_PENDING = this.getTimePending(this.props.txnDate);
    if (TIME_PENDING < PENDING_LIMIT) {
      return `${iconType}_regular`;
    }
    return `${iconType}_warning`;
  };

  renderPendingIcon = () => {
    const TIME_PENDING = this.getTimePending(this.props.txnDate);
    if (TIME_PENDING < PENDING_LIMIT) {
      return this.renderPendingRegularIcon();
    }
    return this.renderPendingWarningIcon();
  };

  renderPendingRegularIcon = () => (
    <div className={styles.pendingTxnIconWrapper}>
      <LoadingSpinner
        skin={LoadingSpinnerSkin}
        themeOverrides={spinnerOverrides}
      />
    </div>
  );

  renderPendingWarningIcon = () => (
    <div className={styles.pendingTxnIconWrapper}>
      <LoadingSpinner
        skin={LoadingSpinnerSkin}
        themeOverrides={spinnerOverrides}
      />
      <SVGInline svg={pendingIcon} className={styles.pendingTxnIcon} />
    </div>
  );

  renderIcon = (icon: string) => {
    if (this.props.iconType === TransactionStates.PENDING) {
      return this.renderPendingIcon();
    }
    return <SVGInline svg={icon} className={styles.transactionTypeIcon} />;
  };

  render() {
    const { iconType } = this.props;

    const transactionTypeIconClasses = classNames([
      styles.transactionTypeIconWrapper,
      styles[this.applyIconStyles(iconType)],
    ]);

    let icon;
    switch (iconType) {
      case TransactionTypes.EXPEND:
        icon = expendIcon;
        break;
      case TransactionTypes.INCOME:
        icon = incomeIcon;
        break;
      case TransactionTypes.EXCHANGE:
        icon = exchangeIcon;
        break;
      default:
        icon = '';
        break;
    }

    return (
      <div className={transactionTypeIconClasses}>{this.renderIcon(icon)}</div>
    );
  }
}
