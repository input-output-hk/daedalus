import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import InputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './AmountInputSkinEtc.scss';
import { formattedAmountWithoutTrailingZeros } from '../../../../utils/formatters';
import { messages } from '../AmountInputSkin';

type Props = {
  currency: string,
  fees: BigNumber,
  total: BigNumber,
  error: boolean,
};

export default class AmountInputSkin extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { error, fees, total, currency } = this.props;
    const { intl } = this.context;

    const formattedFees = formattedAmountWithoutTrailingZeros(fees);
    const formattedTotal = formattedAmountWithoutTrailingZeros(total);

    return (
      <div className={styles.root}>
        <InputSkin {...this.props} />
        {!error && (
          <span className={styles.fees}>
            {intl.formatMessage(messages.feesLabel, { amount: formattedFees })}
          </span>
        )}
        <span className={styles.total}>
          = {formattedTotal} {currency}
        </span>
      </div>
    );
  }

}
