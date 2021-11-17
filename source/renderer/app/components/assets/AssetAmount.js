// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './AssetAmount.scss';
import type { AssetMetadata } from '../../api/assets/types';
import { useDiscreetModeFeature } from '../../features/discreet-mode';

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

function AssetAmount({
  amount,
  metadata,
  decimals,
  isLoading,
  className,
  isShort,
}: Props) {
  const discreetModeFeature = useDiscreetModeFeature();

  if (isLoading) return '-';
  const componentStyles = classnames([styles.component, className]);
  const content = !isLoading
    ? discreetModeFeature.hideOrShowTokenWalletAmount({
        amount,
        metadata,
        decimals,
        isShort,
      })
    : '-';

  return (
    <div className={componentStyles}>
      {decimals ? (
        <PopOver
          content={
            <FormattedHTMLMessage
              {...messages.unformattedAmount}
              values={{
                amount: discreetModeFeature.hideOrShowTokenWalletAmount({
                  amount,
                  metadata: null,
                  decimals: 0,
                }),
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

export default observer(AssetAmount);
