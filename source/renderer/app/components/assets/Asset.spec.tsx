import React from 'react';
import BigNumber from 'bignumber.js';
import { render, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import Asset from './Asset';
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';

const assets = [
  {
    policyId: 'policyId',
    assetName: '54657374636f696e',
    quantity: new BigNumber(1),
    fingerprint: 'fingerprint',
    metadata: {
      name: 'Testcoin',
      description: 'Test coin',
    },
    uniqueId: 'uniqueId',
    decimals: 1,
    recommendedDecimals: null,
  },
  {
    policyId: 'policyId',
    assetName: '436f696e74657374',
    quantity: new BigNumber(1),
    fingerprint: 'fingerprint',
    uniqueId: 'uniqueId',
    decimals: 1,
    recommendedDecimals: null,
  },
  {
    policyId: 'policyId',
    assetName: '',
    quantity: new BigNumber(1),
    fingerprint: 'fingerprint',
    uniqueId: 'uniqueId',
    decimals: 1,
    recommendedDecimals: null,
  },
];

describe('Asset', () => {
  afterEach(cleanup);

  test('Should display asset metadata name', () => {
    render(
      <TestDecorator>
        <Asset asset={assets[0]} />
      </TestDecorator>
    );
    expect(screen.queryByTestId('assetName')).toHaveTextContent('Testcoin');
  });

  test('Should display asset ASCII name', () => {
    render(
      <TestDecorator>
        <Asset asset={assets[1]} />
      </TestDecorator>
    );
    expect(screen.queryByTestId('assetName')).toHaveTextContent(
      'ASCII: Cointest'
    );
  });

  test('Should display empty name', () => {
    render(
      <TestDecorator>
        <Asset asset={assets[2]} />
      </TestDecorator>
    );
    expect(screen.queryByTestId('assetName')).toBeNull();
  });
});
