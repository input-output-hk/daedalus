// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import expendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import incomeIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import exchangeIcon from '../../../assets/images/exchange-ic.inline.svg';
import styles from './TransactionTypeIcon.scss';
import { TransactionTypes } from '../../../domains/WalletTransaction';

type Props = {
  iconType: string,
};

export default class TransactionTypeIcon extends Component<Props> {
  render() {
    const { iconType } = this.props;

    const transactionTypeIconClasses = classNames([
      styles.transactionTypeIconWrapper,
      styles[iconType],
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
      <div className={transactionTypeIconClasses}>
        <SVGInline svg={icon} className={styles.transactionTypeIcon} />
      </div>
    );
  }
}
