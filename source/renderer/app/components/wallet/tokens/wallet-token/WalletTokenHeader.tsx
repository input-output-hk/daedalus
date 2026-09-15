import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import styles from './WalletTokenHeader.scss';
import Asset from '../../../assets/Asset';
import AssetAmount from '../../../assets/AssetAmount';
import type { AssetToken } from '../../../../api/assets/types';
import { requestAssetImageUrl } from '../../../../ipc/assetMetadataChannel';
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
  const { uniqueId, policyId, assetName, hasImage } = asset;
  const starIcon = isFavorite ? starFilledIcon : starNotFilledIcon;
  const [logoUrl, setLogoUrl] = useState<string | null>(null);

  // The cache keys a logo on the policy id followed by the asset name, which is
  // what `uniqueId` is for a token the wallet reports and only usually what it
  // is for one read off a transaction. Derived here so the two cannot disagree.
  const subject = `${policyId}${assetName}`;

  // Keyed on `hasImage` as well as on the subject: a row drawn before its
  // metadata arrives says it has no logo, and the row that replaces it a moment
  // later is the first one with a reason to ask.
  useEffect(() => {
    if (!hasImage) return undefined;
    let wanted = true;
    requestAssetImageUrl(subject).then((url) => {
      if (wanted) setLogoUrl(url);
    });
    // A list is scrolled, and an answer can outlive the row that asked for it.
    return () => {
      wanted = false;
    };
  }, [subject, hasImage]);

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

      {logoUrl && (
        <img className={styles.logo} src={logoUrl} alt="" data-testid="logo" />
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
