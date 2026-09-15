import React from 'react';
import BigNumber from 'bignumber.js';
import { render, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import Asset from './Asset';
import styles from './Asset.scss';
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';

const baseAsset = {
  policyId: 'policyId',
  quantity: new BigNumber(1),
  fingerprint: 'asset1t4gm2ptzxj22sqxd7jtx7dwjqy2u3h56j6vrjr',
  uniqueId: 'uniqueId',
  decimals: 1,
  recommendedDecimals: null,
};
const assetWithRegistryTicker = {
  ...baseAsset,
  assetName: '54657374636f696e',
  metadata: {
    name: 'Test Coin',
    description: 'Test coin',
    ticker: 'TEST',
  },
};
const assetWithRegistryName = {
  ...baseAsset,
  assetName: '54657374636f696e',
  metadata: {
    name: 'Testcoin',
    description: 'Test coin',
  },
};
// 'Cointest'
const assetWithPrintableName = {
  ...baseAsset,
  assetName: '436f696e74657374',
};
// 32 random bytes, the shape an NFT asset name usually takes
const assetWithNonPrintableName = {
  ...baseAsset,
  assetName: '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf',
};
const assetWithoutName = {
  ...baseAsset,
  assetName: '',
};
// 'é', which is valid UTF-8 and outside ASCII
const assetWithNonAsciiName = {
  ...baseAsset,
  assetName: 'c3a9',
};
// The impersonation pair. Both display the text 'USDC'.
const assetImpersonatingTicker = {
  ...baseAsset,
  assetName: '55534443',
};
const assetWithImpersonatedTicker = {
  ...baseAsset,
  assetName: '',
  metadata: {
    name: 'USD Coin',
    description: 'A stablecoin',
    ticker: 'USDC',
  },
};

// An NFT the registry has never heard of, named by its CIP-25 record.
const assetWithChainName = {
  ...baseAsset,
  assetName: '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf',
  metadata: {
    name: 'Northwind Demo',
    description: '',
  },
  source: 'chain' as const,
};

const renderAsset = (props) =>
  render(
    <TestDecorator>
      <Asset {...props} />
    </TestDecorator>
  );

describe('Asset', () => {
  afterEach(cleanup);

  it('displays the registry ticker ahead of every other name', () => {
    renderAsset({ asset: assetWithRegistryTicker });
    expect(screen.queryByTestId('assetName')).toHaveTextContent('TEST');
  });

  it('displays the registry name when no ticker is published', () => {
    renderAsset({ asset: assetWithRegistryName });
    expect(screen.queryByTestId('assetName')).toHaveTextContent('Testcoin');
  });

  it('displays a printable asset name decoded and without a prefix', () => {
    const { container } = renderAsset({ asset: assetWithPrintableName });
    expect(screen.queryByTestId('assetNameMinterChosen')).toHaveTextContent(
      'Cointest'
    );
    expect(container.textContent).not.toContain('ASCII');
  });

  it('displays no name for an asset name of 32 random bytes', () => {
    const { container } = renderAsset({ asset: assetWithNonPrintableName });
    expect(screen.queryByTestId('assetName')).toBeNull();
    expect(screen.queryByTestId('assetNameMinterChosen')).toBeNull();
    expect(container.textContent).not.toContain('�');
  });

  it('displays no name for an empty asset name', () => {
    renderAsset({ asset: assetWithoutName });
    expect(screen.queryByTestId('assetName')).toBeNull();
    expect(screen.queryByTestId('assetNameMinterChosen')).toBeNull();
  });

  it('displays no name for an asset name that is valid UTF-8 outside ASCII', () => {
    const { container } = renderAsset({ asset: assetWithNonAsciiName });
    expect(screen.queryByTestId('assetName')).toBeNull();
    expect(screen.queryByTestId('assetNameMinterChosen')).toBeNull();
    expect(container.textContent).not.toContain('é');
  });

  it('still displays the fingerprint when no name resolves', () => {
    const { container } = renderAsset({
      asset: assetWithNonPrintableName,
      fullFingerprint: true,
    });
    expect(container.textContent).toContain(baseAsset.fingerprint);
  });

  it('distinguishes a minter-chosen name from the registry ticker it spells', () => {
    const { unmount } = renderAsset({ asset: assetImpersonatingTicker });
    const minterChosen = screen.getByTestId('assetNameMinterChosen');
    expect(minterChosen).toHaveTextContent('USDC');
    expect(minterChosen).toHaveClass(styles.minterChosenName);
    expect(minterChosen).toHaveAttribute('title');
    expect(minterChosen.getAttribute('title')).not.toHaveLength(0);
    unmount();

    renderAsset({ asset: assetWithImpersonatedTicker });
    const published = screen.getByTestId('assetName');
    expect(published).toHaveTextContent('USDC');
    expect(published).not.toHaveClass(styles.minterChosenName);
    expect(published).not.toHaveAttribute('title');
  });

  // The token list, the send form and the transaction list pass three
  // different prop shapes to the same pill. The marking holds under all three.
  it.each([
    [
      'the token list',
      { small: false, metadataNameChars: 0, hidePopOver: true },
    ],
    ['the send form', { small: true, hidePopOver: true }],
    ['the transaction list', {}],
  ])('marks a minter-chosen name as rendered by %s', (_surface, props) => {
    renderAsset({ asset: assetImpersonatingTicker, ...props });
    expect(screen.getByTestId('assetNameMinterChosen')).toHaveClass(
      styles.minterChosenName
    );
  });

  // The chain channel exists to name an NFT the registry has never heard of.
  it('displays a CIP-25 name where the asset name alone would show nothing', () => {
    renderAsset({ asset: assetWithChainName });
    expect(screen.queryByTestId('assetName')).toHaveTextContent(
      'Northwind Demo'
    );
  });

  it('does not mark a CIP-25 name as minter-chosen', () => {
    renderAsset({ asset: assetWithChainName });
    expect(screen.queryByTestId('assetNameMinterChosen')).toBeNull();
    expect(screen.queryByTestId('assetName')).not.toHaveClass(
      styles.minterChosenName
    );
  });

  it('shows the fingerprint for that asset before its row arrives', () => {
    const { container } = renderAsset({
      asset: { ...baseAsset, assetName: assetWithChainName.assetName },
    });
    expect(screen.queryByTestId('assetName')).toBeNull();
    expect(screen.queryByTestId('assetNameMinterChosen')).toBeNull();
    expect(container.textContent).toContain('asset1t4g');
  });
});
