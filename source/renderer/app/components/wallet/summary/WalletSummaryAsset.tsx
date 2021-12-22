import React, { useState } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSummaryAsset.scss' or ... Remove this comment to see the full error message
import styles from './WalletSummaryAsset.scss';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import AssetContent from '../../assets/AssetContent';
import type { AssetToken } from '../../../api/assets/types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/collaps... Remove this comment to see the full error message
import arrow from '../../../assets/images/collapse-arrow-small.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/asset-t... Remove this comment to see the full error message
import warningIcon from '../../../assets/images/asset-token-warning-ic.inline.svg';

const messages = defineMessages({
  tokenSendButton: {
    id: 'wallet.summary.asset.tokenSendButton',
    defaultMessage: '!!!Send',
    description: 'Send button on Wallet summary assets page',
  },
  amountLabel: {
    id: 'wallet.summary.asset.amountLabel',
    defaultMessage: '!!!Amount',
    description: 'Amount label on Wallet summary assets page',
  },
  settingsButtonLabel: {
    id: 'wallet.summary.asset.settings.button.label',
    defaultMessage: '!!!Settings',
    description: 'Settings label on Wallet summary assets page',
  },
  settingsWarningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  settingsWarningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
  },
});
type Props = {
  asset: AssetToken;
  onOpenAssetSend: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
  onAssetSettings: (...args: Array<any>) => any;
  anyAssetWasHovered: boolean;
  isLoading: boolean;
  assetSettingsDialogWasOpened: boolean;
  intl: intlShape.isRequired;
};
type IsExpanded = boolean;
const WalletSummaryAsset = observer((props: Props) => {
  const [isExpanded, setIsExpanded] = useState<IsExpanded>(false);

  const toggleIsExpanded = () => {
    setIsExpanded(!isExpanded);
  };

  const header = () => {
    const {
      asset,
      onCopyAssetParam,
      isLoading,
      anyAssetWasHovered,
      assetSettingsDialogWasOpened,
    } = props;
    const { decimals, recommendedDecimals } = asset;
    const arrowStyles = classNames(styles.arrow, {
      [styles.isExpanded]: isExpanded,
    });
    const hasWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
    return (
      <div className={styles.header} onClick={toggleIsExpanded}>
        <Asset
          asset={asset}
          onCopyAssetParam={onCopyAssetParam}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          metadataNameChars={get('name', asset.metadata, 0)}
          assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
          anyAssetWasHovered={anyAssetWasHovered}
          className={styles.asset}
          hidePopOver
          fullFingerprint
          hasWarning={hasWarning}
        />
        <AssetAmount
          amount={asset.quantity}
          metadata={asset.metadata}
          decimals={asset.decimals}
          isLoading={isLoading}
          className={styles.assetAmount}
          isShort
        />
        <SVGInline svg={arrow} className={arrowStyles} />
      </div>
    );
  };

  const footer = () => {
    const { asset, isLoading, intl } = props;
    return (
      <div className={styles.footer}>
        <dl>
          <dt>{intl.formatMessage(messages.amountLabel)}</dt>
          <dd>
            {' '}
            <AssetAmount
              amount={asset.quantity}
              metadata={asset.metadata}
              decimals={asset.decimals}
              isLoading={isLoading}
              className={styles.assetAmount}
            />
          </dd>
        </dl>
        {buttons()}
      </div>
    );
  };

  const buttons = () => {
    const { asset, onOpenAssetSend, onAssetSettings, intl } = props;
    const { recommendedDecimals, decimals } = asset;
    const hasWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
    let settingsButtonLabel = intl.formatMessage(messages.settingsButtonLabel);
    let warningPopOverMessage;

    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
      settingsButtonLabel = (
        <>
          {settingsButtonLabel}
          <SVGInline className={styles.warningIcon} svg={warningIcon} />
        </>
      );
    }

    const settingsButton = (
      <Button
        className={classNames(['flat', styles.button, styles.settingsButton])}
        label={settingsButtonLabel}
        onClick={() =>
          onAssetSettings({
            asset,
          })
        }
      />
    );
    return (
      <div className={styles.footerButtons}>
        {hasWarning ? (
          <PopOver
            content={intl.formatMessage(warningPopOverMessage, {
              recommendedDecimals,
            })}
            className={styles.warningIconWrapper}
          >
            {settingsButton}
          </PopOver>
        ) : (
          settingsButton
        )}
        <Button
          className={classNames([
            'primary',
            styles.button,
            asset.quantity.isZero() ? styles.disabled : null,
          ])}
          onClick={() => onOpenAssetSend(asset)}
          label={intl.formatMessage(messages.tokenSendButton)}
        />
      </div>
    );
  };

  const { asset, onCopyAssetParam } = props;
  const componentStyles = classNames(styles.component, {
    [styles.isExpanded]: isExpanded,
  });
  return (
    <div className={componentStyles}>
      {header()}
      <div className={styles.content}>
        <AssetContent
          asset={asset}
          onCopyAssetParam={onCopyAssetParam}
          highlightFingerprint={false}
        />
        {footer()}
      </div>
    </div>
  );
});
export default injectIntl(WalletSummaryAsset);
