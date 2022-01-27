import React from 'react';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import { discreetWalletTokenAmount } from '../../features/discreet-mode/replacers/discreetWalletTokenAmount';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AssetAmount.scss' or its cor... Remove this comment to see the full error message
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
  amount: BigNumber;
  metadata?: AssetMetadata | null | undefined;
  decimals: number | null | undefined;
  isLoading?: boolean;
  className?: string;
  isShort?: boolean;
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
    ? discreetModeFeature.discreetValue({
        replacer: discreetWalletTokenAmount({
          amount,
          metadata,
          decimals,
          isShort,
        }),
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
                amount: discreetModeFeature.discreetValue({
                  replacer: discreetWalletTokenAmount({
                    amount,
                    metadata: null,
                    decimals: 0,
                  }),
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

// @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ amount, metadata, decimals, i... Remove this comment to see the full error message
export default observer(AssetAmount);
