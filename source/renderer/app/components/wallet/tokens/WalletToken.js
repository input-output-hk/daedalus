// @flow
import React, { useState } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletToken.scss';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import AssetContent from '../../assets/AssetContent';
import type { AssetToken } from '../../../api/assets/types';
import arrow from '../../../assets/images/collapse-arrow-small.inline.svg';
import warningIcon from '../../../assets/images/asset-token-warning-ic.inline.svg';
import starNotFilledIcon from '../../../assets/images/star-not-filled.inline.svg';
import starFilledIcon from '../../../assets/images/star-filled.inline.svg';

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
  asset: AssetToken,
  onOpenAssetSend: Function,
  onCopyAssetParam: Function,
  onAssetSettings: Function,
  anyAssetWasHovered: boolean,
  isLoading: boolean,
  assetSettingsDialogWasOpened: boolean,
  intl: intlShape.isRequired,
  isFavorite: boolean,
  onToggleFavorite: Function,
};

type IsExpanded = boolean;

const WalletToken = observer((props: Props) => {
  const [isExpanded, setIsExpanded] = useState<IsExpanded>(false);

  const toggleIsExpanded = () => {
    setIsExpanded(!isExpanded);
  };

  const favoriteIconStyles = classNames([
    styles.favoriteIcon,
    props.isFavorite ? styles.isFavorite : null,
  ]);

  const header = () => {
    const {
      anyAssetWasHovered,
      asset,
      assetSettingsDialogWasOpened,
      isFavorite,
      isLoading,
      onCopyAssetParam,
      onToggleFavorite,
    } = props;
    const { decimals, recommendedDecimals, uniqueId } = asset;
    const arrowStyles = classNames(styles.arrow, {
      [styles.isExpanded]: isExpanded,
    });
    const hasWarning =
      typeof recommendedDecimals === 'number' &&
      decimals !== recommendedDecimals;
    const starIcon = isFavorite ? starFilledIcon : starNotFilledIcon;
    return (
      <div className={styles.header} onClick={toggleIsExpanded}>
        <button
          className={favoriteIconStyles}
          onClick={(event) => {
            event.persist();
            event.stopPropagation();
            onToggleFavorite({ uniqueId });
          }}
        >
          <SVGInline className={styles.warningIcon} svg={starIcon} />
        </button>
        <Asset
          asset={asset}
          onCopyAssetParam={onCopyAssetParam}
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
        onClick={() => onAssetSettings({ asset })}
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

export default injectIntl(WalletToken);
