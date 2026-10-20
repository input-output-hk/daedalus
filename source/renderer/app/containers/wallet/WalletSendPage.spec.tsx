import React from 'react';
import BigNumber from 'bignumber.js';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import WalletSendPage from './WalletSendPage';
import type { FormData } from '../../components/wallet/WalletSendForm';

jest.mock('../../components/analytics/withAnalytics', () => ({
  withAnalytics: (component: React.ComponentType) => component,
}));
jest.mock('../../components/wallet/WalletSendForm', () => ({
  __esModule: true,
  default: ({ onSubmit }: { onSubmit: (data: FormData) => void }) => (
    <button
      onClick={() =>
        onSubmit({
          receiver: 'address',
          amount: new BigNumber(3),
          totalAmount: new BigNumber('3.17'),
          transactionFee: new BigNumber('0.17'),
          selectedAssets: [],
          assetsAmounts: [],
          coinSelection: { fee: new BigNumber('0.17') },
        } as unknown as FormData)
      }
    >
      Send
    </button>
  ),
}));

const mountSend = (isHardwareWallet: boolean, isLegacy = false) => {
  const wallet = {
    id: 'wallet',
    name: 'Wallet',
    isHardwareWallet,
    isLegacy,
    amount: new BigNumber(10),
    assets: { total: [] },
  };
  const updateTxSignRequest = jest.fn();
  const open = jest.fn();
  const softwareSend = jest.fn();
  const hardwareSend = jest.fn();
  const initiateTransaction = jest.fn();
  const props = {
    analyticsTracker: jest.fn(),
    stores: {
      wallets: { active: wallet, getWalletById: () => wallet },
      hardwareWallets: { updateTxSignRequest, initiateTransaction },
      transactions: {},
      app: {},
      profile: {},
      assets: { all: [], getAsset: jest.fn(), favorites: [] },
      addresses: { all: [] },
      collateral: {
        get preparationFormActive(): boolean {
          throw new Error('Nonpilot collateral access');
        },
      },
      uiDialogs: { isOpen: () => false },
    },
    actions: {
      dialogs: { open: { trigger: open } },
      wallets: {
        unsetActiveAsset: { trigger: jest.fn() },
        sendMoney: { trigger: softwareSend },
      },
      hardwareWallets: { sendMoney: { trigger: hardwareSend } },
    },
  };
  render(
    <WalletSendPage
      {...(props as unknown as React.ComponentProps<typeof WalletSendPage>)}
    />
  );
  return {
    open,
    updateTxSignRequest,
    initiateTransaction,
    softwareSend,
    hardwareSend,
  };
};

describe('upstream Send approval entry', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(cleanup);

  it.each([false, true])(
    'opens explicit confirmation for a software wallet (legacy=%s)',
    (isLegacy) => {
      const fixture = mountSend(false, isLegacy);
      fireEvent.click(screen.getByRole('button', { name: 'Send' }));
      expect(fixture.open).toHaveBeenCalledWith(
        expect.objectContaining({
          props: expect.objectContaining({
            amount: '3',
            transactionFee: '0.17',
            isHardwareWallet: false,
          }),
        })
      );
      expect(fixture.softwareSend).not.toHaveBeenCalled();
      expect(fixture.hardwareSend).not.toHaveBeenCalled();
    }
  );

  it('hands the hardware selection to the draft before opening confirmation, without starting signing twice', () => {
    const fixture = mountSend(true);
    fireEvent.click(screen.getByRole('button', { name: 'Send' }));
    expect(fixture.updateTxSignRequest).toHaveBeenCalledTimes(1);
    expect(
      fixture.updateTxSignRequest.mock.invocationCallOrder[0]
    ).toBeLessThan(fixture.open.mock.invocationCallOrder[0]);
    expect(fixture.initiateTransaction).not.toHaveBeenCalled();
    expect(fixture.hardwareSend).not.toHaveBeenCalled();
  });
});
