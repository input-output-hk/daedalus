import React from 'react';
import compose from 'lodash/fp/compose';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import globalMessages from '../../../../i18n/global-messages';
import { messages } from './messages';
import { DialogContentWithoutAssets as Props } from './types';
import styles from './DialogContentWithoutAssets.scss';

function Component({
  intl,
  amount,
  receiver,
  transactionFee,
  formattedTotalAmount,
}: Props) {
  return (
    <div className={styles.root}>
      <div className={styles.addressToLabelWrapper}>
        <div className={styles.addressToLabel}>
          {intl.formatMessage(messages.addressToLabel)}
        </div>
        <div className={styles.addressTo}>{receiver}</div>
      </div>

      <div className={styles.amountFeesWrapper}>
        <div className={styles.amountWrapper}>
          <div className={styles.amountLabel}>
            {intl.formatMessage(messages.amountLabel)}
          </div>
          <div className={styles.amount}>
            {amount}
            <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
          </div>
        </div>

        <div className={styles.feesWrapper}>
          <div className={styles.feesLabel}>
            {intl.formatMessage(messages.feesLabel)}
          </div>
          <div className={styles.fees}>
            +{transactionFee}
            <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
          </div>
        </div>
      </div>

      <div className={styles.totalAmountLabel}>
        {intl.formatMessage(messages.totalLabel)}
      </div>
      <div className={styles.totalAmount}>
        {formattedTotalAmount}
        <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
      </div>
    </div>
  );
}

export const DialogContentWithoutAssets = compose(
  injectIntl,
  observer
)(Component) as React.ComponentType<Omit<Props, 'intl'>>;
