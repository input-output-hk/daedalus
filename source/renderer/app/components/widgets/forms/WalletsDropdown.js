// @flow
import React, { Component } from 'react';
import WalletsDropdownTopLabel from './WalletsDropdownTopLabel';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import ItemsDropdown from './ItemsDropdown';
import type { SelectProps } from './ItemsDropdown';

type Props = {
  ...$Shape<SelectProps>,
  getStakePoolById: Function,
  isSyncing: boolean,
  numberOfStakePools: number,
  syncingLabel?: string,
  wallets?: Array<$Shape<Wallet>>,
};

export default class WalletsDropdown extends Component<Props> {
  render() {
    const { wallets = [] } = this.props;
    const formattedOptions = wallets.map((wallet) => {
      const { id: value, amount, isRestoring } = wallet;
      const bottomLabel = !isRestoring ? formattedWalletAmount(amount) : null;
      return {
        topLabel: <WalletsDropdownTopLabel wallet={wallet} {...this.props} />,
        bottomLabel,
        value,
      };
    });
    return <ItemsDropdown options={formattedOptions} {...this.props} />;
  }
}
