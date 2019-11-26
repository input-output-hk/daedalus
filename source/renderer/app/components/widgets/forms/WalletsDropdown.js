// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit } from 'lodash';
import WalletsDropdownOption from './WalletsDropdownOption';

import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import type { StakePool } from '../../../api/staking/types';

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
  stakePoolsDelegatingList: Array<StakePool>,
  wallets: Array<$Shape<Wallet>>,
};

type WalletOption = {
  activeDelegation: StakePool,
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
      activeDelegation,
    }: WalletOption) => (
      <WalletsDropdownOption
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        activeDelegation={activeDelegation}
      />
    ),
    selectionRenderer: ({
      label,
      detail,
      numberOfStakePools,
      activeDelegation,
    }: WalletOption) => (
      <WalletsDropdownOption
        selected
        label={label}
        numberOfStakePools={numberOfStakePools}
        detail={detail}
        activeDelegation={activeDelegation}
      />
    ),
    skin: SelectSkin,
  };

  render() {
    const {
      wallets,
      numberOfStakePools,
      stakePoolsDelegatingList,
      ...props
    } = this.props;
    const walletsData = wallets.map(
      ({ name: label, id: value, amount }: Wallet) => {
        const detail = formattedWalletAmount(amount);
        // TODO: use wallet ID or another method to determine if the wallet actually has an active delegation
        const activeDelegation = stakePoolsDelegatingList[0];
        return {
          detail,
          label,
          value,
          numberOfStakePools,
          activeDelegation,
        };
      }
    );
    const selectOptions = omit(props, 'options');
    return <Select options={walletsData} {...selectOptions} />;
  }
}
