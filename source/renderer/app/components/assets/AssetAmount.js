// @flow
import React, { Component } from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './AssetAmount.scss';
import { formattedTokenWalletAmount } from '../../utils/formatters';
import type { AssetMetadata } from '../../api/assets/types';

const messages = defineMessages({
  unformattedAmount: {
    id: 'assets.assetAmount.unformattedAmount',
    defaultMessage: '!!!Unformatted amount {amount}',
    description: 'Unformatted amount',
  },
});

type Props = {
  amount: BigNumber,
  metadata?: ?AssetMetadata,
  decimals: ?number,
  isLoading?: boolean,
  className?: string,
  isShort?: boolean,
};

@observer
export default class AssetAmount extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      amount,
      metadata,
      decimals,
      isLoading,
      className,
      isShort,
    } = this.props;
    if (isLoading) return '-';
    const componentStyles = classnames([styles.component, className]);
    const content = !isLoading
      ? formattedTokenWalletAmount(amount, metadata, decimals, isShort)
      : '-';
    return (
      <div className={componentStyles}>
        {decimals ? (
          <PopOver
            content={
              <FormattedHTMLMessage
                {...messages.unformattedAmount}
                values={{
                  amount: formattedTokenWalletAmount(amount, null, 0),
                }}
              />
            }
            visible={decimals ? undefined : false}
            className={styles.unformattedAmount}
          >
            {content}
          </PopOver>
        ) : (
          <span>{content}</span>
        )}
      </div>
    );
  }
}
