// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit } from 'lodash';
import WalletsDropdownOption from './WalletsDropdownOption';

import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';

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
  wallets: Array<$Shape<Wallet>>,
};

type WalletOption = {
  label: string,
  detail: string,
  value: string,
};

export default class WalletsDropdown extends Component<Props> {
  static defaultProps = {
    optionRenderer: ({ label, detail }: WalletOption) => (
      <WalletsDropdownOption label={label} detail={detail} />
    ),
    selectionRenderer: ({ label, detail }: WalletOption) => (
      <WalletsDropdownOption label={label} detail={detail} selected />
    ),
    skin: SelectSkin,
  };

  render() {
    const { wallets, ...props } = this.props;
    const walletsData = wallets.map(
      ({ name: label, id: value, amount }: Wallet) => {
        const detail = formattedWalletAmount(amount, true, true);
        return {
          detail,
          label,
          value,
        };
      }
    );
    const selectOptions = omit(props, 'options');
    return <Select options={walletsData} {...selectOptions} />;
  }
}
