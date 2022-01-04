// @flow
import React from 'react';
import { DiscreetValue } from '../../../features/discreet-mode';

type Props = {
  amount: number,
};

export function RewardAmount({ amount }: Props) {
  return <DiscreetValue>{amount}</DiscreetValue>;
}
