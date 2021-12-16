// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './WalletTokenFooter.scss';
import AssetAmount from '../../../assets/AssetAmount';
import type { AssetToken } from '../../../../api/assets/types';
import warningIcon from '../../../../assets/images/asset-token-warning-ic.inline.svg';
import { messages } from './WalletToken.messages';
import { isRecommendedDecimal } from './helpers';

type Props = {
  asset: AssetToken,
  intl: intlShape.isRequired,
  isLoading: boolean,
  onAssetSettings?: Function,
  onOpenAssetSend?: Function,
};

const WalletTokenFooter = (props: Props) => {
  const { asset, intl, isLoading, onAssetSettings, onOpenAssetSend } = props;
  const { recommendedDecimals, decimals } = asset;
  const hasWarning = isRecommendedDecimal({ decimals, recommendedDecimals });
  const warningPopOverMessage =
    typeof decimals === 'number'
      ? messages.settingsWarningPopOverNotUsing
      : messages.settingsWarningPopOverAvailable;

  return (
    <div className={styles.footer}>
      <div className={styles.amount}>
        <span className={styles.amountLabel}>
          {intl.formatMessage(messages.amountLabel)}
        </span>
        <div className={styles.amountValue}>
          <AssetAmount
            amount={asset.quantity}
            metadata={asset.metadata}
            decimals={asset.decimals}
            isLoading={isLoading}
            className={styles.assetAmount}
          />
        </div>
      </div>
      <div className={styles.footerButtons}>
        {onAssetSettings && (
          <PopOver
            content={
              hasWarning &&
              intl.formatMessage(warningPopOverMessage, {
                recommendedDecimals,
              })
            }
          >
            <Button
              className={classNames([
                'flat',
                styles.button,
                styles.settingsButton,
              ])}
              label={
                <>
                  {intl.formatMessage(messages.settingsButtonLabel)}
                  {hasWarning && (
                    <SVGInline
                      className={styles.warningIcon}
                      svg={warningIcon}
                    />
                  )}
                </>
              }
              onClick={() => onAssetSettings({ asset })}
            />
          </PopOver>
        )}
        {onOpenAssetSend && (
          <Button
            className={classNames([
              'primary',
              styles.button,
              asset.quantity.isZero() && styles.disabled,
            ])}
            onClick={() => onOpenAssetSend(asset)}
            label={intl.formatMessage(messages.tokenSendButton)}
            disabled={asset.quantity.isZero()}
          />
        )}
      </div>
    </div>
  );
};

export default injectIntl(observer(WalletTokenFooter));
