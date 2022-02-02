import React, { Component } from 'react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { LoadingSpinner } from 'react-polymorph/lib/components/LoadingSpinner';
import { LoadingSpinnerSkin } from 'react-polymorph/lib/skins/simple/LoadingSpinnerSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TransactionTypeIcon.scss' or... Remove this comment to see the full error message
import styles from './TransactionTypeIcon.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SpinnerOverrides.scss' or it... Remove this comment to see the full error message
import spinnerOverrides from './SpinnerOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
import expendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
import incomeIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/exchang... Remove this comment to see the full error message
import exchangeIcon from '../../../assets/images/exchange-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/wallet-... Remove this comment to see the full error message
import pendingIcon from '../../../assets/images/wallet-nav/pending.inline.svg';
import {
  TransactionTypes,
  TransactionStates,
} from '../../../domains/WalletTransaction';

type Props = {
  exceedsPendingTimeLimit: boolean;
  iconType: string;
};
export default class TransactionTypeIcon extends Component<Props> {
  applyIconStyles = (iconType: string): string => {
    if (iconType !== TransactionStates.PENDING) {
      return iconType;
    }

    if (!this.props.exceedsPendingTimeLimit) {
      return `${iconType}_regular`;
    }

    return `${iconType}_warning`;
  };
  renderPendingIcon = () => {
    if (!this.props.exceedsPendingTimeLimit) {
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
  renderFailedIcon = () => {
    return (
      <SVGInline svg={pendingIcon} className={styles.transactionTypeIcon} />
    );
  };
  renderIcon = (icon: string) => {
    let iconType;

    if (this.props.iconType === TransactionStates.PENDING) {
      iconType = this.renderPendingIcon();
    } else if (this.props.iconType === TransactionStates.FAILED) {
      iconType = this.renderFailedIcon();
    } else {
      iconType = (
        <SVGInline svg={icon} className={styles.transactionTypeIcon} />
      );
    }

    return iconType;
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
