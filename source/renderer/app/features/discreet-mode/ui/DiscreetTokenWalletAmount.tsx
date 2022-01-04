import React from 'react';
import { observer } from 'mobx-react';
import { discreetWalletTokenAmount } from '../replacers/discreetWalletTokenAmount';
import type { DiscreetWalletTokenAmountProps } from '../replacers/discreetWalletTokenAmount';
import DiscreetValue from './DiscreetValue';

function DiscreetTokenWalletAmount(props: DiscreetWalletTokenAmountProps) {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'children' is missing in type '{ replacer... Remove this comment to see the full error message
  return <DiscreetValue replacer={discreetWalletTokenAmount(props)} />;
}

export default observer(DiscreetTokenWalletAmount);
