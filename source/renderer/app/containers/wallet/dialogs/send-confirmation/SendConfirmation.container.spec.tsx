import React from 'react';
import { render } from '@testing-library/react';
import { Containter } from './SendConfirmation.container';

jest.mock('../../../../utils/assets', () => ({
  getNonZeroAssetTokens: () => [],
}));
jest.mock('./SendConfirmation.view', () => ({
  WalletSendConfirmationDialogView: () => null,
}));

describe('SendConfirmation container', () => {
  it('starts hardware signing when production confirmation opens', () => {
    global.isFlight = false;
    const initiateTransaction = jest.fn();
    const request = { isExecuting: false, error: null, reset: jest.fn() };
    const trigger = jest.fn();

    render(
      <Containter
        {...({
          actions: {
            assets: { onCopyAssetParam: { trigger } },
            dialogs: { closeActiveDialog: { trigger } },
            hardwareWallets: { sendMoney: { trigger } },
            wallets: { sendMoney: { trigger } },
          },
          stores: {
            assets: { getAsset: jest.fn() },
            wallets: {
              sendMoneyRequest: request,
              active: { id: 'wallet-id', assets: { total: {} } },
            },
            hardwareWallets: {
              _resetTransaction: jest.fn(),
              sendMoneyRequest: request,
              isTransactionPending: false,
              checkIsTrezorByWalletId: () => false,
              initiateTransaction,
            },
            collateral: { cancelPreparation: jest.fn() },
          },
          isHardwareWallet: true,
          selectedAssets: [],
          assetsAmounts: {},
        } as any)}
      />
    );

    expect(initiateTransaction).toHaveBeenCalledWith({ walletId: 'wallet-id' });
  });
});
