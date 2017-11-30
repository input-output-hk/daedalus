// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import SvgInline from 'react-svg-inline';
import expendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import incomeIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import exchangeIcon from '../../../assets/images/exchange-ic.inline.svg';
import failedIcon from '../../../assets/images/wallet-nav/deny-ic.inline.svg';
import styles from './TransactionTypeIcon.scss';

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
      case 'expend':
        icon = expendIcon;
        break;
      case 'income':
        icon = incomeIcon;
        break;
      case 'exchange':
        icon = exchangeIcon;
        break;
      case 'failed':
        icon = failedIcon;
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
