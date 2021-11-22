// @flow
import React from 'react';
import {
  discreetWalletAmount,
  DiscreetWalletAmountProps,
} from '../replacers/discreetWalletAmount';
import DiscreetValue from './DiscreetValue';

export default function DiscreetWalletAmount(props: DiscreetWalletAmountProps) {
  return <DiscreetValue replacer={discreetWalletAmount(props)} />;
}
