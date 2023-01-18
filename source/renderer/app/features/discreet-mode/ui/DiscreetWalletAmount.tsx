import React from 'react';
import { discreetWalletAmount } from '../replacers/discreetWalletAmount';
import type { DiscreetWalletAmountProps } from '../replacers/discreetWalletAmount';
import DiscreetValue from './DiscreetValue';

export default function DiscreetWalletAmount(props: DiscreetWalletAmountProps) {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'children' is missing in type '{ replacer... Remove this comment to see the full error message
  return <DiscreetValue replacer={discreetWalletAmount(props)} />;
}
