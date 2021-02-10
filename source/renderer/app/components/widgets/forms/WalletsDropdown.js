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
  wallets: Array<$Shape<Wallet>>,
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
      numberOfStakePools,
      getStakePoolById,
      error,
      errorPosition,
      hasAssetsEnabled,
      ...props
    } = this.props;
    const walletsData = wallets.map(
      ({
        name: label,
        id: value,
        amount,
        delegatedStakePoolId,
        lastDelegationStakePoolId,
        pendingDelegations,
        isRestoring,
        isHardwareWallet,
        ticker,
      }: Wallet) => {
        const hasPendingDelegations =
          pendingDelegations && pendingDelegations.length > 0;
        let currentStakePoolId = delegatedStakePoolId;
        if (hasPendingDelegations) {
          currentStakePoolId = lastDelegationStakePoolId;
        }
        const delegatedStakePool = getStakePoolById(currentStakePoolId);
        const formattedAmount =
          hasAssetsEnabled && ticker
            ? formattedTokenWalletAmount(amount, ticker)
            : formattedWalletAmount(amount || new BigNumber(1));
        const detail = !isRestoring ? formattedAmount : null;
        return {
          detail,
          syncing: isRestoring,
          label: hasAssetsEnabled ? ticker : label,
          value,
          numberOfStakePools,
          delegatedStakePool,
          isHardwareWallet,
          syncingLabel: this.props.syncingLabel,
        };
      }
    );
    let topError;
    let bottomError;
    if (errorPosition === 'bottom') bottomError = error;
    else topError = error;
    const selectOptions = omit({ ...props, topError }, 'options');
    return (
      <>
        <Select options={walletsData} {...selectOptions} optionHeight={50} />
        {bottomError && <div className={styles.error}>{bottomError}</div>}
      </>
    );
  }
}
