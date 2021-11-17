// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../context';

type Props = {
  amount: BigNumber,
  withCurrency?: boolean,
  long?: boolean,
};

function DiscreetWalletAmount(props: Props) {
  const feature = useDiscreetModeFeature();

  return <>{feature.hideOrShowWalletAmount(props)}</>;
}

export default observer(DiscreetWalletAmount);
