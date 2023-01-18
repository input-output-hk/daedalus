import React, { useState, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './WalletToken.scss';
import AssetContent from '../../../assets/AssetContent';
import type { AssetToken } from '../../../../api/assets/types';
import WalletTokenFooter from './WalletTokenFooter';
import WalletTokenHeader from './WalletTokenHeader';
import { isNonRecommendedDecimalSettingUsed } from './helpers';

type Props = {
  anyAssetWasHovered: boolean;
  asset: AssetToken;
  assetSettingsDialogWasOpened: boolean;
  className?: string;
  headerClassName?: string;
  footerClassName?: string;
  fullFingerprint?: boolean;
  isFavorite: boolean;
  isInsertingAsset: boolean;
  isLoading: boolean;
  isRemovingAsset: boolean;
  onAssetSettings?: (...args: Array<any>) => any;
  onCopyAssetParam?: (...args: Array<any>) => any;
  onOpenAssetSend?: (...args: Array<any>) => any;
  onToggleFavorite?: (...args: Array<any>) => any;
};
const WalletToken = observer((props: Props) => {
  const {
    anyAssetWasHovered,
    asset,
    assetSettingsDialogWasOpened,
    className,
    headerClassName,
    footerClassName,
    fullFingerprint = true,
    isFavorite,
    isLoading,
    onAssetSettings,
    onCopyAssetParam,
    onOpenAssetSend,
    onToggleFavorite,
    isInsertingAsset,
    isRemovingAsset,
  } = props;
  const [isExpanded, setIsExpanded] = useState<boolean>(false);

  const toggleIsExpanded = useCallback(() => {
    setIsExpanded(!isExpanded);
  }, [setIsExpanded, isExpanded]);

  const hasWarning = isNonRecommendedDecimalSettingUsed({
    decimals: asset.decimals,
    recommendedDecimals: asset.recommendedDecimals,
  });

  const componentStyles = useMemo(
    () =>
      classNames(
        styles.component,
        isExpanded && styles.isExpanded,
        isInsertingAsset && styles.inserting,
        isRemovingAsset && styles.removing,
        className
      ),
    [className, styles, isExpanded, isInsertingAsset, isRemovingAsset]
  );
  return (
    <div className={componentStyles}>
      <WalletTokenHeader
        asset={asset}
        className={headerClassName}
        isFavorite={isFavorite}
        isExpanded={isExpanded}
        isLoading={isLoading}
        fullFingerprint={fullFingerprint}
        anyAssetWasHovered={anyAssetWasHovered}
        onClick={toggleIsExpanded}
        onCopyAssetParam={onCopyAssetParam}
        onToggleFavorite={onToggleFavorite}
        assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
        hasWarning={hasWarning}
      />
      <div className={styles.content}>
        <AssetContent
          asset={asset}
          onCopyAssetParam={onCopyAssetParam}
          highlightFingerprint={false}
        />
        <WalletTokenFooter
          asset={asset}
          className={footerClassName}
          isLoading={isLoading}
          onAssetSettings={onAssetSettings}
          onOpenAssetSend={onOpenAssetSend}
          hasWarning={hasWarning}
        />
      </div>
    </div>
  );
});
export default WalletToken;
