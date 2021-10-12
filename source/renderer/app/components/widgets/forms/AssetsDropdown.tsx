import { $Shape } from "utility-types";
import React, { Component } from "react";
import { omit, filter, escapeRegExp } from "lodash";
import ItemsDropdown from "./ItemsDropdown";
import { formattedTokenWalletAmount } from "../../../utils/formatters";
import type { AssetToken } from "../../../api/assets/types";
import Asset from "../../assets/Asset";
import styles from "./AssetsDropdown.scss";

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
  assets?: Array<$Shape<AssetToken>>;
};
export const onSearchAssetsDropdown = (searchValue: string, options: Array<any>) => {
  return filter(options, ({
    asset
  }) => {
    if (searchValue.length < 3) {
      return true;
    }

    const {
      policyId,
      assetName,
      fingerprint,
      metadata
    } = asset;
    const {
      name,
      ticker,
      description
    } = metadata || {};
    const checkList = [policyId, assetName, fingerprint, metadata, name, ticker, description];
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return checkList.some(item => regex.test(item));
  });
};
export default class AssetsDropdown extends Component<Props> {
  static defaultProps = {
    onSearch: onSearchAssetsDropdown
  };

  render() {
    const {
      assets = []
    } = this.props;
    const props = omit(this.props, ['wallets', 'options']);
    const formattedOptions = assets.map(asset => {
      const {
        uniqueId: value,
        metadata,
        quantity,
        decimals
      } = asset;
      const detail = formattedTokenWalletAmount(quantity, metadata, decimals);
      return {
        label: <Asset asset={asset} className={styles.assetToken} hidePopOver small />,
        detail,
        value,
        asset
      };
    });
    return <ItemsDropdown options={formattedOptions} {...props} />;
  }

}