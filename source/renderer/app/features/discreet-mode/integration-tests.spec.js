// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { IntlProvider } from 'react-intl';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { render, fireEvent, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';

import { BrowserLocalStorageBridge } from '../local-storage';
import { DiscreetModeFeatureProvider } from './context';
import {
  DiscreetWalletAmount,
  DiscreetModeToggle,
  DiscreetTokenWalletAmount,
  DiscreetValue,
  withDiscreetMode,
} from './ui';
import { DiscreetMode } from './feature';

describe('Discreet Mode feature', () => {
  afterEach(cleanup);

  function TestDecorator({
    children,
    testId,
  }: {
    children: Node,
    testId: string,
  }) {
    return (
      <IntlProvider locale="en-US">
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <>
              <DiscreetModeToggle />

              <div data-testid={testId}>{children}</div>
            </>
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    );
  }

  const discreetWalletAmountCases = [
    // testId, [amount, currency, withCurrency, long], [expected]
    ['custom ticker', [new BigNumber(1), 'TEST', true, true], '1.000000 TEST'],
    ['ada as ticker', [new BigNumber(1), undefined, true, false], '1 ADA'],
    ['no currency', [new BigNumber(1), 'TEST', false, false], '1'],
  ];

  test.each(discreetWalletAmountCases)(
    '<DiscreetWalletAmount /> should replace wallet amount with %s by sensitive data symbol',
    async (testId, [amount, currency, withCurrency, long], expected) => {
      expect.assertions(2);

      render(
        <TestDecorator testId={testId}>
          <DiscreetWalletAmount
            amount={amount}
            currency={currency}
            withCurrency={withCurrency}
            long={long}
          />
        </TestDecorator>
      );

      expect(screen.getByTestId(testId)).toHaveTextContent(expected);

      fireEvent.click(
        screen.getByRole('button', { name: /discreetModeToggle/i })
      );

      expect(screen.getByTestId(testId)).toHaveTextContent('***');
    }
  );

  const discreetTokenWalletAmountCases = [
    // testId, [amount, metada, decimals], [expected]
    ['without ticker', [new BigNumber(1), null, 0], '1'],
    ['with ticker', [new BigNumber(1), { ticker: 'TEST' }, 5], '0.00001 TEST'],
  ];

  test.each(discreetTokenWalletAmountCases)(
    '<DiscreetTokenWalletAmount /> should replace token wallet amount with %s by sensitive data symbol',
    (testId, [amount, metadata, decimals], expected) => {
      expect.assertions(2);

      render(
        <TestDecorator testId={testId}>
          <DiscreetTokenWalletAmount
            amount={amount}
            metadata={metadata}
            decimals={decimals}
          />
        </TestDecorator>
      );

      expect(screen.getByTestId(testId)).toHaveTextContent(expected);

      fireEvent.click(
        screen.getByRole('button', { name: /discreetModeToggle/i })
      );

      expect(screen.getByTestId(testId)).toHaveTextContent('***');
    }
  );

  test('<DiscreetValue /> should replace value by sensitive data symbol', () => {
    expect.assertions(2);

    const value = 'test';
    const testId = 'discreet-value';

    render(
      <TestDecorator testId={testId}>
        <DiscreetValue>{value}</DiscreetValue>
      </TestDecorator>
    );

    expect(screen.getByTestId(testId)).toHaveTextContent(value);

    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );

    expect(screen.getByTestId(testId)).toHaveTextContent('***');
  });

  test('<withDiscreetMode /> high order component should replace value by sensitive data symbol', () => {
    expect.assertions(2);

    const value = 'test';
    const testId = 'discreet-value';
    const HighOrderComponentApi = withDiscreetMode(
      observer(
        class View extends Component<{ discreetModeFeature: DiscreetMode }> {
          render() {
            return this.props.discreetModeFeature.discreetValue({
              value,
            });
          }
        }
      )
    );

    render(
      <TestDecorator testId={testId}>
        <HighOrderComponentApi />
      </TestDecorator>
    );

    expect(screen.getByTestId(testId)).toHaveTextContent(value);

    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );

    expect(screen.getByTestId(testId)).toHaveTextContent('***');
  });
});
