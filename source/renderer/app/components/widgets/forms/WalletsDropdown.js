// @flow
import React, { Component } from 'react';
import { omit, filter, escapeRegExp } from 'lodash';
import WalletsDropdownLabel from './WalletsDropdownLabel';
import { formattedWalletAmount } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
import ItemsDropdown from './ItemsDropdown';

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
  getStakePoolById: Function,
  numberOfStakePools: number,
  syncingLabel?: string,
  wallets?: Array<$Shape<Wallet>>,
  className?: string,
};

export const onSearchWalletsDropdown = (
  searchValue: string,
  options: Array<any>
) => {
  return filter(options, (option) => {
    const { walletName, detail } = option;
    const regex = new RegExp(escapeRegExp(searchValue), 'i');
    return [walletName, detail].some((item) => regex.test(item));
  });
};

export default class WalletsDropdown extends Component<Props> {
  static defaultProps = {
    onSearch: onSearchWalletsDropdown,
  };
  render() {
    const { wallets = [], className } = this.props;
    const props = omit(this.props, ['wallets', 'options']);
    const formattedOptions = wallets.map((wallet) => {
      const {
        id: value,
        amount,
        isRestoring,
        isSyncing,
        restorationProgress: syncingProgress,
      } = wallet;
      const detail = !isRestoring ? formattedWalletAmount(amount) : null;
      return {
        label: <WalletsDropdownLabel wallet={wallet} {...props} />,
        detail,
        value,
        walletName: wallet.name,
        isSyncing,
        syncingProgress,
      };
    });
    return (
      <ItemsDropdown
        className={className}
        options={formattedOptions}
        {...props}
      />
    );
  }
}
