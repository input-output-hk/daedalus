// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit } from 'lodash';
import { BigNumber } from 'bignumber.js';
import WalletsDropdownOption from './WalletsDropdownOption';
import styles from './WalletsDropdown.scss';

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
      assets,
      numberOfStakePools,
      getStakePoolById,
      error,
      errorPosition,
      hasAssetsEnabled,
      ...props
    } = this.props;
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
        ? assets.map(({ metadata, policyId, quantity }: WalletSummaryAsset) => {
            const formattedAmount = formattedTokenWalletAmount(
              new BigNumber(quantity),
              metadata && metadata.acronym ? metadata.acronym : ''
            );
            return {
              detail: formattedAmount,
              label: metadata && metadata.acronym ? metadata.acronym : '',
              value: policyId,
            };
          })
        : null;
    let topError;
    let bottomError;
    if (errorPosition === 'bottom') bottomError = error;
    else topError = error;
    const selectOptions = omit({ ...props, topError }, 'options');
    return (
      <>
        <Select
          options={hasAssetsEnabled && assetsData ? assetsData : walletsData}
          {...selectOptions}
          optionHeight={50}
        />
        {bottomError && <div className={styles.error}>{bottomError}</div>}
      </>
    );
  }
}
