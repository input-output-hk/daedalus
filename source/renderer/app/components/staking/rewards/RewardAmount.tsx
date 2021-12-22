import React from 'react';
import { DiscreetValue } from '../../../features/discreet-mode';

type Props = {
  amount: number;
};
export function RewardAmount({ amount }: Props) {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'replacer' is missing in type '{ children... Remove this comment to see the full error message
  return <DiscreetValue>{amount}</DiscreetValue>;
}
