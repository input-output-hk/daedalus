// @flow
import React from 'react';
import { omit, filter, escapeRegExp } from 'lodash';
import WalletsDropdownLabel from './WalletsDropdownLabel';
import { formattedWalletAmount } from '../../../utils/formatters';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
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
  className?: string,
  getStakePoolById: Function,
  numberOfStakePools: number,
  onSearch?: Function,
  wallets?: Array<$Shape<Wallet>>,
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

export default function WalletsDropdown({
  className,
  getStakePoolById,
  numberOfStakePools,
  onSearch = onSearchWalletsDropdown,
  wallets = [],
  ...props
}: Props) {
  const discreetModeFeature = useDiscreetModeFeature();
  const itemsDropdownProps = {
    ...omit(props, ['wallets', 'options']),
    onSearch,
  };
  const formattedOptions = wallets.map((wallet) => {
    const {
      id: value,
      amount,
      isRestoring,
      isSyncing,
      restorationProgress: syncingProgress,
    } = wallet;
    const detail = !isRestoring
      ? discreetModeFeature.hideSensitiveData(formattedWalletAmount(amount))
      : null;
    return {
      label: (
        <WalletsDropdownLabel
          wallet={wallet}
          getStakePoolById={getStakePoolById}
          numberOfStakePools={numberOfStakePools}
        />
      ),
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
      {...itemsDropdownProps}
    />
  );
}
