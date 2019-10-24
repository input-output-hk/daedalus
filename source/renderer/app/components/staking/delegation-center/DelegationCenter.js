// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';
import type { NumberFormat } from '../../../../../common/types/number.types';

type Props = {
  adaValue: BigNumber,
  percentage: number,
  wallets: Array<Wallet>,
  onDelegate: Function,
  currentNumberFormatPretty: NumberFormat,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      adaValue,
      percentage,
      wallets,
      onDelegate,
      currentNumberFormatPretty,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader
          adaValue={adaValue}
          percentage={percentage}
          currentNumberFormatPretty={currentNumberFormatPretty}
        />
        <DelegationCenterBody
          wallets={wallets}
          onDelegate={onDelegate}
          currentNumberFormatPretty={currentNumberFormatPretty}
        />
      </Fragment>
    );
  }
}
