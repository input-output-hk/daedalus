// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import type { AssetMetadata } from '../../../api/assets/types';
import { useDiscreetModeFeature } from '../context';

type Props = {
  amount: BigNumber,
  metadata?: ?AssetMetadata,
  decimals: ?number,
  isShort?: boolean,
};

function DiscreetTokenWalletAmount(props: Props) {
  const feature = useDiscreetModeFeature();

  return <>{feature.hideOrShowTokenWalletAmount(props)}</>;
}

export default observer(DiscreetTokenWalletAmount);
