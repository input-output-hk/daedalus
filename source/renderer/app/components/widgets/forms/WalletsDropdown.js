// @flow
import React, { Component } from 'react';
import BigNumber from 'bignumber.js';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit, escapeRegExp, get } from 'lodash';
import WalletsDropdownOption from './WalletsDropdownOption';
import styles from './WalletsDropdown.scss';
import AssetToken from '../AssetToken';

import dummyAssetsList from '../../../config/assets.dummy.json';

import {
  formattedTokenWalletAmount,
  formattedWalletAmount,
} from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import type { WalletSummaryAsset } from '../../../api/assets/types';

type SelectProps = {
  allowBlank?: boolean,
  autoFocus?: boolean,
  className?: string,
  context?: any,
  disabled?: boolean,
  error?: string | Element<any>,
  errorPosition?: 'top' | 'bottom',
  label?: string | Element<any>,
  hasSearch?: boolean,
  onSearch?: boolean,
  isOpeningUpward?: boolean,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  optionRenderer?: Function,
  options?: Array<any>,
  placeholder?: string,
  selectionRenderer?: Function,
  skin?: Element<any>,
  theme?: ?Object, // will take precedence over theme in context if passed
  themeId?: string,
  themeOverrides?: Object,
  value: ?string,
};

type Props = {
  ...$Shape<SelectProps>,
  numberOfStakePools: number,
  wallets?: Array<$Shape<Wallet>>,
  assets?: Array<$Shape<WalletSummaryAsset>>,
  getStakePoolById: Function,
  syncingLabel?: string,
  hasAssetsEnabled?: string,
};

type WalletOption = {
  delegatedStakePool?: ?StakePool,
  label: string,
  numberOfStakePools: number,
  detail: string,
  value: string,
  syncing?: boolean,
  syncingLabel?: string,
  isHardwareWallet: boolean,
  hasAssetsEnabled?: boolean,
};

export default class WalletsDropdown extends Component<Props> {
  static defaultProps = {
    optionRenderer: ({
      label,
      detail,
      numberOfStakePools,
      delegatedStakePool,
      isHardwareWallet,
      syncing,
      syncingLabel,
    }: WalletOption) => (
      <WalletsDropdownOption
        isSyncing={syncing}
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        delegatedStakePool={delegatedStakePool}
        isHardwareWallet={isHardwareWallet}
        syncingLabel={syncingLabel}
      />
    ),
    selectionRenderer: ({
      label,
      detail,
      numberOfStakePools,
      delegatedStakePool,
      isHardwareWallet,
      syncing,
      syncingLabel,
    }: WalletOption) => (
      <WalletsDropdownOption
        selected
        isSyncing={syncing}
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        delegatedStakePool={delegatedStakePool}
        isHardwareWallet={isHardwareWallet}
        syncingLabel={syncingLabel}
      />
    ),
    skin: SelectSkin,
    errorPosition: 'top',
  };

  render() {
    const {
      wallets,
      // assets,
      numberOfStakePools,
      getStakePoolById,
      error,
      errorPosition,
      hasAssetsEnabled,
      ...props
    } = this.props;

    // TODO REMOVE
    const dassets = dummyAssetsList.map(({ quantity, ...asset }) => ({
      ...asset,
      quantity: new BigNumber(quantity),
    }));
    const assets = [...this.props.assets, ...dassets];

    const walletsData =
      wallets && wallets.length
        ? wallets.map(
            ({
              name: label,
              id: value,
              amount,
              delegatedStakePoolId,
              lastDelegatedStakePoolId,
              pendingDelegations,
              isRestoring,
              isHardwareWallet,
            }: Wallet) => {
              const hasPendingDelegations =
                pendingDelegations && pendingDelegations.length > 0;
              let currentStakePoolId = delegatedStakePoolId;
              if (hasPendingDelegations) {
                currentStakePoolId = lastDelegatedStakePoolId;
              }
              const delegatedStakePool = getStakePoolById(currentStakePoolId);
              const detail = !isRestoring
                ? formattedWalletAmount(amount)
                : null;
              return {
                detail,
                syncing: isRestoring,
                label,
                value,
                numberOfStakePools,
                delegatedStakePool,
                isHardwareWallet,
                syncingLabel: this.props.syncingLabel,
              };
            }
          )
        : null;
    const assetsData =
      assets && assets.length
        ? assets.map((asset: WalletSummaryAsset) => {
            const { metadata, quantity, fingerprint } = asset;
            const formattedAmount = formattedTokenWalletAmount(
              quantity,
              metadata
            );
            return {
              detail: formattedAmount,
              label: <AssetToken asset={asset} hideTooltip small />,
              value: fingerprint,
            };
          })
        : null;
    let topError;
    let bottomError;
    if (errorPosition === 'bottom') bottomError = error;
    else topError = error;
    const selectOptions = omit({ ...props, topError }, 'options');
    const options = hasAssetsEnabled && assetsData ? assetsData : walletsData;
    return (
      <>
        <Select
          options={options}
          {...selectOptions}
          hasSearch
          onSearch={(searchValue, list) => {
            const regex = new RegExp(escapeRegExp(searchValue), 'i');
            return list.filter((item) => {
              const {
                asset,
                policyId,
                assetName,
                quantity,
                fingerprint,
                metadata,
              } = get(item, 'label.props.asset', {});
              const { name } = metadata || {};
              return (
                regex.test(asset) ||
                regex.test(policyId) ||
                regex.test(assetName) ||
                regex.test(quantity) ||
                regex.test(fingerprint) ||
                regex.test(name)
              );
            });
          }}
          optionHeight={50}
        />
        {bottomError && <div className={styles.error}>{bottomError}</div>}
      </>
    );
  }
}
