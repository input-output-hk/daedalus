import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AmountInputSkin.scss' or its... Remove this comment to see the full error message
import styles from './AmountInputSkin.scss';

export const messages = defineMessages({
  feesLabel: {
    id: 'wallet.amountInput.feesLabel',
    defaultMessage: '!!!+ {amount} of fees',
    description:
      'Label for the "+ 12.042481 of fees" message above amount input field.',
  },
  calculatingFeesLabel: {
    id: 'wallet.amountInput.calculatingFeesLabel',
    defaultMessage: '!!!Calculating fees',
    description:
      'Label for the "Calculating fees" message above amount input field.',
  },
});
type Props = {
  currency: string;
  fees: BigNumber | null | undefined;
  total: BigNumber | null | undefined;
  error: boolean;
  isCalculatingFees: boolean;
};
export default class AmountInputSkin extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { error, fees, total, currency, isCalculatingFees } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.root}>
        <InputSkin {...this.props} />
        {isCalculatingFees && (
          <span className={styles.calculatingFees}>
            {intl.formatMessage(messages.calculatingFeesLabel)}
          </span>
        )}
        {fees && !error && !isCalculatingFees && (
          <span className={styles.fees}>
            {intl.formatMessage(messages.feesLabel, {
              amount: fees,
            })}
          </span>
        )}
        <span className={styles.total}>
          {total && !error && `= ${total} `}
          {currency}
        </span>
      </div>
    );
  }
}
