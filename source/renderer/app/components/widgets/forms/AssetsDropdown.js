// @flow
import React from 'react';
import { omit, filter, escapeRegExp } from 'lodash';
import ItemsDropdown from './ItemsDropdown';
import type { AssetToken } from '../../../api/assets/types';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import Asset from '../../assets/Asset';
import styles from './AssetsDropdown.scss';

/**
 *
 * This component extends the ItemDropdownProps component
 * which is based on React Polymorph's Select
 * Any prop from it can be used
 * Reference:
 * https://github.com/input-output-hk/react-polymorph/blob/develop/source/components/Select.js
 *
 */
type Props = {
  assets?: Array<$Shape<AssetToken>>,
  onSearch?: Function,
};

export const onSearchAssetsDropdown = (
  searchValue: string,
  options: Array<any>
) => {
  return filter(options, ({ asset }) => {
    if (searchValue.length < 3) {
      return true;
    }
    const { policyId, assetName, fingerprint, metadata } = asset;
    const { name, ticker, description } = metadata || {};
    const checkList = [
      policyId,
      assetName,
      fingerprint,
      metadata,
      name,
      ticker,
      description,
    ];
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return checkList.some((item) => regex.test(item));
  });
};

export default function AssetsDropdown({
  assets = [],
  onSearch = onSearchAssetsDropdown,
  ...props
}: Props) {
  const discreetModeFeature = useDiscreetModeFeature();
  const ItemsDropdownProps = {
    ...omit(props, ['wallets', 'options']),
    onSearch,
  };
  const formattedOptions = assets.map((asset) => {
    const { uniqueId: value, metadata, quantity, decimals } = asset;
    const detail = discreetModeFeature.hideOrShowTokenWalletAmount({
      amount: quantity,
      metadata,
      decimals,
    });
    return {
      label: (
        <Asset asset={asset} className={styles.assetToken} hidePopOver small />
      ),
      detail,
      value,
      asset,
    };
  });
  return <ItemsDropdown options={formattedOptions} {...ItemsDropdownProps} />;
}
