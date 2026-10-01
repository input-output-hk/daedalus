import React from 'react';
import { render, fireEvent, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';
import WalletAdd from './WalletAdd';

type NetworkFlags = {
  isMainnet?: boolean;
  isTestnet?: boolean;
  isPreprod?: boolean;
  isPreview?: boolean;
};

const renderWalletAdd = (flags: NetworkFlags, isProduction = true) => {
  const onImport = jest.fn();
  const { container } = render(
    <TestDecorator>
      <WalletAdd
        onCreate={jest.fn()}
        onRestore={jest.fn()}
        onImport={onImport}
        onConnect={jest.fn()}
        isMaxNumberOfWalletsReached={false}
        isMainnet={false}
        isTestnet={false}
        isPreprod={false}
        isPreview={false}
        isProduction={isProduction}
        {...flags}
      />
    </TestDecorator>
  );
  const button = container.querySelector('.importWalletButton');
  return { button, onImport };
};

describe('WalletAdd', () => {
  afterEach(cleanup);

  it.each([
    ['mainnet', { isMainnet: true }],
    ['the legacy testnet', { isTestnet: true }],
    ['preprod', { isPreprod: true }],
    ['preview', { isPreview: true }],
  ])('enables wallet import on %s', (_name, flags) => {
    const { button, onImport } = renderWalletAdd(flags);
    fireEvent.click(button);
    expect(onImport).toHaveBeenCalledTimes(1);
  });

  it('disables wallet import on a network that is not supported', () => {
    const { button, onImport } = renderWalletAdd({});
    fireEvent.click(button);
    expect(onImport).not.toHaveBeenCalled();
  });

  it('enables wallet import outside production', () => {
    const { button, onImport } = renderWalletAdd({}, false);
    fireEvent.click(button);
    expect(onImport).toHaveBeenCalledTimes(1);
  });
});
