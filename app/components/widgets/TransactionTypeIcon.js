// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import SvgInline from 'react-svg-inline';
import adaExpendIcon from '../../assets/images/wallet-nav/send-ic.inline.svg';
import adaIncomeIcon from '../../assets/images/wallet-nav/receive-ic.inline.svg';
import exchangeIcon from '../../assets/images/exchange-ic.inline.svg';
import styles from './TransactionTypeIcon.scss';

export default class TransactionTypeIcon extends Component {

  props: {
    iconType: string,
  };

  render() {
    const { iconType } = this.props;

    const transactionTypeIconClasses = classNames([
      styles.transactionTypeIconWrapper,
      styles[iconType],
    ]);

    let icon;
    switch (iconType) {
      case 'adaExpend':
        icon = adaExpendIcon;
        break;
      case 'adaIncome':
        icon = adaIncomeIcon;
        break;
      case 'exchange':
        icon = exchangeIcon;
        break;
      default:
        icon = '';
        break;
    }

    return (
      <div className={transactionTypeIconClasses}>
        <SvgInline svg={icon} className={styles.transactionTypeIcon} />
      </div>
    );
  }
}
