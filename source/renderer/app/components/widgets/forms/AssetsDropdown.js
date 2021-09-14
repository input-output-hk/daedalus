// @flow
import React, { Component } from 'react';
import { omit } from 'lodash';
import ItemsDropdown from './ItemsDropdown';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import { searchAssets } from '../../../utils/assets';
import type { AssetToken } from '../../../api/assets/types';
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
};

export default class AssetsDropdown extends Component<Props> {
  static defaultProps = {
    onSearch: (searchValue: string, assets: Array<any>) =>
      searchAssets(
        searchValue,
        assets.map(({ asset }) => asset)
      ),
  };

  render() {
    const { assets = [] } = this.props;
    const props = omit(this.props, ['wallets', 'options']);
    const formattedOptions = assets.map((asset) => {
      const { uniqueId: value, metadata, quantity, decimals } = asset;
      const detail = formattedTokenWalletAmount(quantity, metadata, decimals);
      return {
        label: (
          <Asset
            asset={asset}
            className={styles.assetToken}
            hidePopOver
            small
          />
        ),
        detail,
        value,
        asset,
      };
    });
    return <ItemsDropdown options={formattedOptions} {...props} />;
  }
}
