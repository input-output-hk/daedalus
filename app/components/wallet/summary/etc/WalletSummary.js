// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SvgInline from 'react-svg-inline';
import etcSymbol from '../../../../assets/images/etc-logo.inline.svg';
import BorderedBox from '../../../widgets/BorderedBox';
import styles from '../WalletSummary.scss';

@observer
export default class WalletSummary extends Component {

  props: {
    walletName: string,
    amount: string,
  };

  render() {
    const {
      walletName,
      amount,
    } = this.props;
    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletName}>{walletName}</div>
          <div className={styles.walletAmount}>
            {amount}
            <SvgInline svg={etcSymbol} className={styles.currencySymbolBig} />
          </div>
        </BorderedBox>
      </div>
    );
  }

}
