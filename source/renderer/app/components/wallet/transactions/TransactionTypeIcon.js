// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { LoadingSpinner } from 'react-polymorph/lib/components/LoadingSpinner';
import { LoadingSpinnerSkin } from 'react-polymorph/lib/skins/simple/LoadingSpinnerSkin';
import styles from './TransactionTypeIcon.scss';
import spinnerOverrides from './SpinnerOverrides.scss';
import expendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import incomeIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import exchangeIcon from '../../../assets/images/exchange-ic.inline.svg';
import pendingIcon from '../../../assets/images/wallet-nav/pending.inline.svg';
import {
  TransactionTypes,
  TransactionStates,
} from '../../../domains/WalletTransaction';

type Props = {
  iconType: string,
};

export default class TransactionTypeIcon extends Component<Props> {
  renderIcon = (icon: string) => {
    if (this.props.iconType === TransactionStates.PENDING) {
      return (
        <div className={styles.pendingTxnIconWrapper}>
          <LoadingSpinner
            skin={LoadingSpinnerSkin}
            themeOverrides={spinnerOverrides}
          />
          <SVGInline svg={pendingIcon} className={styles.pendingTxnIcon} />
        </div>
      );
    }
    return <SVGInline svg={icon} className={styles.transactionTypeIcon} />;
  };

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
      <div className={transactionTypeIconClasses}>{this.renderIcon(icon)}</div>
    );
  }
}
