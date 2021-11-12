// @flow
import React from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';

import { useDiscreetModeFeature } from '../context';
import { SENSITIVE_DATA_SYMBOL } from '../config';

type Props = {
  sensitiveData?: {
    className: string,
    character: string,
  },
  ticker?: {
    symbol: string,
    className: string,
    show: boolean,
  },
  children: Node,
};

const defaultValues = {
  sensitiveData: {
    className: '',
    character: SENSITIVE_DATA_SYMBOL,
  },
  ticker: {
    symbol: 'ADA',
    className: '',
    show: false,
  },
};

function DiscreetValue({ children, sensitiveData = {}, ticker = {} }: Props) {
  const feature = useDiscreetModeFeature();

  if (feature.isDiscreetMode) {
    const sensitiveDataValues = {
      ...defaultValues.sensitiveData,
      ...sensitiveData,
    };

    const tickerValues = {
      ...defaultValues.ticker,
      ...ticker,
    };

    return (
      <>
        <span className={sensitiveDataValues.className}>
          {sensitiveDataValues.character}
        </span>
        {tickerValues.show ? (
          <>
            {' '}
            <span className={tickerValues.className}>
              {tickerValues.symbol}
            </span>
          </>
        ) : null}
      </>
    );
  }

  return <>{children}</>;
}

export default observer(DiscreetValue);
