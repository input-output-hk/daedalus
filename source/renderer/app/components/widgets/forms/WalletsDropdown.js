// @flow
import React, { Component } from 'react';
import { omit } from 'lodash';
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

export default class WalletsDropdown extends Component<Props> {
  render() {
    const { wallets = [] } = this.props;
    const props = omit(this.props, ['wallets', 'options']);
    const formattedOptions = wallets.map((wallet) => {
      const { id: value, amount, isRestoring } = wallet;
      const bottomLabel = !isRestoring ? formattedWalletAmount(amount) : null;
      return {
        topLabel: <WalletsDropdownTopLabel wallet={wallet} {...props} />,
        bottomLabel,
        value,
      };
    });
    return <ItemsDropdown options={formattedOptions} {...props} />;
  }
}
