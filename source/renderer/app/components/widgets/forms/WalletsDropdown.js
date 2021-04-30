// @flow
import React, { Component } from 'react';
import { omit, filter, escapeRegExp } from 'lodash';
import WalletsDropdownTopLabel from './WalletsDropdownTopLabel';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import ItemsDropdown from './ItemsDropdown';

type Props = {
  getStakePoolById: Function,
  isSyncing?: boolean,
  numberOfStakePools: number,
  syncingLabel?: string,
  wallets?: Array<$Shape<Wallet>>,
};

export const onSearchWalletsDropdown = (
  searchValue: string,
  options: Array<any>
) => {
  return filter(options, (option) => {
    const { walletName, bottomLabel, value } = option;
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return (
      regex.test(walletName) || regex.test(bottomLabel) || regex.test(value)
    );
  });
};

export default class WalletsDropdown extends Component<Props> {
  static defaultProps = {
    onSearch: onSearchWalletsDropdown,
  };
  render() {
    const { wallets = [] } = this.props;
    const props = omit(this.props, ['wallets', 'options']);
    const formattedOptions = wallets.map((wallet) => {
      const { id: value, amount, isRestoring } = wallet;
      const bottomLabel = !isRestoring ? formattedWalletAmount(amount) : null;
      return {
        walletName: wallet.name,
        topLabel: <WalletsDropdownTopLabel wallet={wallet} {...props} />,
        bottomLabel,
        value,
      };
    });
    return <ItemsDropdown options={formattedOptions} {...props} />;
  }
}
