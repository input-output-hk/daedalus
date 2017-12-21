// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SvgInline from 'react-svg-inline';
import classNames from 'classnames';
import adaSymbolSmallest from '../../../assets/images/ada-symbol-smallest-dark.inline.svg';
import arrow from '../../../assets/images/arrow-right.inline.svg';
import WalletAccount from '../../../domain/WalletAccount';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './Account.scss';

type Props = {
  data: WalletAccount,
  index: number,
};

@observer
export default class Account extends Component<Props> {

  render() {
    const { data, index } = this.props;

    // Dynamic Account colors handling
    // We have a set of 55 different colors to chose from
    // Selection is based on Account index which we do based on account index
    let colorIndex = index + 1; // Guard against zero index
    if (colorIndex > 55) {
      colorIndex -= 55; // Loop from 1st color for 55+ indexes
    }
    const dotStyles = classNames([
      styles.dot,
      styles[`color-${colorIndex}`],
    ]);

    return (
      <div className={styles.component}>
        <div className={dotStyles} />
        <div className={styles.name}>{data.name}</div>
        <div className={styles.amount}>
          {data.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
          <SvgInline svg={adaSymbolSmallest} className={styles.currencySymbolSmallest} />
        </div>
        <SvgInline svg={arrow} className={styles.arrow} />
      </div>
    );
  }

}
