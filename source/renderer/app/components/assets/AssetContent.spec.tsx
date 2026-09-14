import React from 'react';
import { render, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import AssetContent from './AssetContent';
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';

const baseAsset = {
  policyId: 'policyId',
  fingerprint: 'asset1t4gm2ptzxj22sqxd7jtx7dwjqy2u3h56j6vrjr',
  uniqueId: 'uniqueId',
  decimals: 1,
  recommendedDecimals: null,
};
// 'Cointest'
const printableAssetName = '436f696e74657374';
// 32 random bytes
const nonPrintableAssetName =
  '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf';

const renderAssetContent = (asset) =>
  render(
    <TestDecorator>
      <AssetContent asset={asset} />
    </TestDecorator>
  );

describe('AssetContent', () => {
  afterEach(cleanup);

  it('annotates a printable asset name as chosen by the minter', () => {
    const { container } = renderAssetContent({
      ...baseAsset,
      assetName: printableAssetName,
    });
    const annotation = screen.getByTestId('assetNameMinterChosenParam');
    expect(annotation).toHaveTextContent('Cointest');
    expect(container.textContent).toContain(printableAssetName);
    expect(container.textContent).not.toContain('ASCII');
  });

  it('omits the annotation for an asset name that is not text', () => {
    const { container } = renderAssetContent({
      ...baseAsset,
      assetName: nonPrintableAssetName,
    });
    expect(screen.queryByTestId('assetNameMinterChosenParam')).toBeNull();
    expect(container.textContent).toContain(nonPrintableAssetName);
    expect(container.textContent).not.toContain('�');
  });

  it('omits the annotation for an empty asset name', () => {
    renderAssetContent({
      ...baseAsset,
      assetName: '',
    });
    expect(screen.queryByTestId('assetNameMinterChosenParam')).toBeNull();
  });
});
