// @flow
import React from 'react';
import type { Node } from 'react';
import { IntlProvider } from 'react-intl';
import BigNumber from 'bignumber.js';
import { render, fireEvent, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import { BrowserLocalStorageBridge } from '../local-storage';
import { DiscreetModeFeatureProvider } from './context';
import {
  DiscreetWalletAmount,
  DiscreetModeToggle,
  DiscreetTokenWalletAmount,
} from './ui';

describe('Discreet Mode feature', () => {
  function DiscreetModeFeatureDecorator({ children }: { children: Node }) {
    return (
      <IntlProvider locale="en-US">
        <BrowserLocalStorageBridge>{children}</BrowserLocalStorageBridge>
      </IntlProvider>
    );
  }

  test('should replace wallet amount by sensitive data symbol', () => {
    expect.assertions(2);

    const amount = new BigNumber(1);
    const currency = 'TEST';

    render(
      <DiscreetModeFeatureDecorator>
        <DiscreetModeFeatureProvider>
          <>
            <DiscreetModeToggle />

            <div data-testid="walletAmount">
              <DiscreetWalletAmount amount={amount} currency={currency} />
            </div>
          </>
        </DiscreetModeFeatureProvider>
      </DiscreetModeFeatureDecorator>
    );

    expect(screen.getByTestId('walletAmount')).toHaveTextContent(
      `1.000000 ${currency}`
    );

    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );

    expect(screen.getByTestId('walletAmount')).toHaveTextContent('***');
  });

  test('should replace token wallet amount by sensitive data symbol', () => {
    expect.assertions(2);

    const amount = new BigNumber(1);
    const metadata = {
      ticker: 'TEST',
    };
    const decimals = 1;

    render(
      <DiscreetModeFeatureDecorator>
        <DiscreetModeFeatureProvider>
          <>
            <DiscreetModeToggle />

            <div data-testid="walletAmount">
              <DiscreetTokenWalletAmount
                amount={amount}
                metadata={metadata}
                decimals={decimals}
              />
            </div>
          </>
        </DiscreetModeFeatureProvider>
      </DiscreetModeFeatureDecorator>
    );

    expect(screen.getByTestId('walletAmount')).toHaveTextContent(
      `0.1 ${metadata.ticker}`
    );

    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );

    expect(screen.getByTestId('walletAmount')).toHaveTextContent('***');
  });
});
