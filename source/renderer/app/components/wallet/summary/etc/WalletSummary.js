// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SvgInline from 'react-svg-inline';
import etcSymbol from '../../../../assets/images/etc-logo.inline.svg';
import BorderedBox from '../../../widgets/BorderedBox';
import styles from '../WalletSummary.scss';

type Props = {
  walletName: string,
  amountInteger: string,
  amountDecimal: string,
};

@observer
export default class WalletSummary extends Component<Props> {

  render() {
    const {
      walletName, amountInteger, amountDecimal,
    } = this.props;
    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletName}>{walletName}</div>
          <div className={styles.walletAmount}>
            {amountInteger}
            <span className={styles.decimal}>.{amountDecimal}</span>
            <SvgInline svg={etcSymbol} className={styles.currencySymbolBig} />
          </div>
        </BorderedBox>
      </div>
    );
  }

}
