// @flow
import React from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletTokenHeader.scss';
import Asset from '../../../assets/Asset';
import AssetAmount from '../../../assets/AssetAmount';
import type { AssetToken } from '../../../../api/assets/types';
import arrow from '../../../../assets/images/collapse-arrow-small.inline.svg';
import starNotFilledIcon from '../../../../assets/images/star-not-filled.inline.svg';
import starFilledIcon from '../../../../assets/images/star-filled.inline.svg';
import { isRecommendedDecimal } from './helpers';

type Props = {
  anyAssetWasHovered: boolean,
  asset: AssetToken,
  assetSettingsDialogWasOpened: boolean,
  isExpanded: boolean,
  isFavorite: boolean,
  isLoading: boolean,
  onClick: Function,
  onCopyAssetParam: Function,
  onToggleFavorite: Function,
};

const WalletTokenHeader = (props: Props) => {
  const {
    anyAssetWasHovered,
    asset,
    assetSettingsDialogWasOpened,
    isExpanded,
    isFavorite,
    isLoading,
    onClick,
    onCopyAssetParam,
    onToggleFavorite,
  } = props;
  const { decimals, uniqueId, recommendedDecimals } = asset;
  const starIcon = isFavorite ? starFilledIcon : starNotFilledIcon;
  const hasWarning = isRecommendedDecimal({ decimals, recommendedDecimals });
  const rootStyles = classNames(styles.root, isExpanded && styles.isExpanded);
  const favoriteIconStyles = classNames(
    styles.favoriteIcon,
    props.isFavorite && styles.isFavorite
  );

  return (
    <div className={rootStyles} onClick={onClick}>
      <button
        className={favoriteIconStyles}
        onClick={(event) => {
          event.persist();
          event.stopPropagation();
          onToggleFavorite({ uniqueId, isFavorite });
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
      <SVGInline svg={arrow} className={styles.arrow} />
    </div>
  );
};

export default observer(WalletTokenHeader);
