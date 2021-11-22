// @flow
import React from 'react';
import { observer } from 'mobx-react';
import {
  discreetWalletTokenAmount,
  DiscreetWalletTokenAmountProps,
} from '../replacers/discreetWalletTokenAmount';
import DiscreetValue from './DiscreetValue';

function DiscreetTokenWalletAmount(props: DiscreetWalletTokenAmountProps) {
  return <DiscreetValue replacer={discreetWalletTokenAmount(props)} />;
}

export default observer(DiscreetTokenWalletAmount);
