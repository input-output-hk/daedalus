// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit } from 'lodash';
import WalletsDropdownOption from './WalletsDropdownOption';

import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';

type SelectProps = {
  allowBlank: boolean,
  autoFocus: boolean,
  className?: string,
  context: any,
  error?: string | Element<any>,
  label?: string | Element<any>,
  isOpeningUpward: boolean,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  optionRenderer?: Function,
  options: Array<any>,
  placeholder?: string,
  selectionRenderer?: Function,
  skin?: Element<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  value: ?string,
};

type Props = {
  ...$Shape<SelectProps>,
  numberOfStakePools: number,
  wallets: Array<$Shape<Wallet>>,
  getStakePoolById: Function,
};

type WalletOption = {
  delegatedStakePool?: ?StakePool,
  label: string,
  numberOfStakePools: number,
  detail: string,
  value: string,
};

export default class WalletsDropdown extends Component<Props> {
  static defaultProps = {
    optionRenderer: ({
      label,
      detail,
      numberOfStakePools,
      delegatedStakePool,
    }: WalletOption) => (
      <WalletsDropdownOption
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        delegatedStakePool={delegatedStakePool}
      />
    ),
    selectionRenderer: ({
      label,
      detail,
      numberOfStakePools,
      delegatedStakePool,
    }: WalletOption) => (
      <WalletsDropdownOption
        selected
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        delegatedStakePool={delegatedStakePool}
      />
    ),
    skin: SelectSkin,
  };

  render() {
    const {
      wallets,
      numberOfStakePools,
      getStakePoolById,
      ...props
    } = this.props;
    const walletsData = wallets.map(
      ({
        name: label,
        id: value,
        amount,
        delegatedStakePoolId,
        nextDelegationStakePoolId,
      }: Wallet) => {
        const currentStakePoolId =
          nextDelegationStakePoolId || delegatedStakePoolId;
        const delegatedStakePool = getStakePoolById(currentStakePoolId);
        const detail = formattedWalletAmount(amount);
        return {
          detail,
          label,
          value,
          numberOfStakePools,
          delegatedStakePool,
        };
      }
    );
    const selectOptions = omit(props, 'options');
    return <Select options={walletsData} {...selectOptions} />;
  }
}
