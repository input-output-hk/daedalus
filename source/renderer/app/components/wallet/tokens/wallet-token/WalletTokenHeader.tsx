import React from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletTokenHeader.scss';
import Asset from '../../../assets/Asset';
import AssetAmount from '../../../assets/AssetAmount';
import type { AssetToken } from '../../../../api/assets/types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/coll... Remove this comment to see the full error message
import arrow from '../../../../assets/images/collapse-arrow-small.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/star... Remove this comment to see the full error message
import starNotFilledIcon from '../../../../assets/images/star-not-filled.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/star... Remove this comment to see the full error message
import starFilledIcon from '../../../../assets/images/star-filled.inline.svg';

type Props = {
  anyAssetWasHovered: boolean;
  asset: AssetToken;
  assetSettingsDialogWasOpened: boolean;
  className?: string;
  fullFingerprint?: boolean;
  isExpanded: boolean;
  isFavorite: boolean;
  isLoading: boolean;
  hasWarning: boolean;
  onClick: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
  onToggleFavorite?: (...args: Array<any>) => any;
};

function WalletTokenHeader(props: Props) {
  const {
    anyAssetWasHovered,
    asset,
    assetSettingsDialogWasOpened,
    className,
    fullFingerprint = true,
    isExpanded,
    isFavorite,
    isLoading,
    hasWarning,
    onClick,
    onCopyAssetParam,
    onToggleFavorite,
  } = props;
  const { uniqueId } = asset;
  const starIcon = isFavorite ? starFilledIcon : starNotFilledIcon;

  const rootStyles = classNames(
    styles.root,
    isExpanded && styles.isExpanded,
    className
  );
  const favoriteIconStyles = classNames(
    styles.favoriteIcon,
    isFavorite && styles.isFavorite
  );

  return (
    <div className={rootStyles} onClick={onClick}>
      {onToggleFavorite && (
        <button
          className={favoriteIconStyles}
          onClick={(event) => {
            event.persist();
            event.stopPropagation();
            onToggleFavorite({
              uniqueId,
              isFavorite,
            });
          }}
        >
          <SVGInline svg={starIcon} />
        </button>
      )}

      <Asset
        asset={asset}
        small={false}
        onCopyAssetParam={onCopyAssetParam}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        metadataNameChars={get('name', asset.metadata, 0)}
        assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
        anyAssetWasHovered={anyAssetWasHovered}
        className={styles.asset}
        hidePopOver
        fullFingerprint={fullFingerprint}
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
}

export default observer(WalletTokenHeader);
