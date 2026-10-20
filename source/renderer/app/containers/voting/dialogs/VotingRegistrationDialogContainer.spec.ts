import BigNumber from 'bignumber.js';
import VotingRegistrationDialogContainer from './VotingRegistrationDialogContainer';

jest.mock('mobx-react', () => ({
  inject: () => (component: unknown) => component,
  observer: (component: unknown) => component,
}));

// The injection decorator obscures the instance method type in this isolated test.
const Container = VotingRegistrationDialogContainer as unknown as {
  prototype: { _handleCalculateTransactionFee: () => Promise<void> };
};
const calculate = Container.prototype._handleCalculateTransactionFee;

describe('Catalyst registration receiving compatibility', () => {
  it.each([
    [false, false],
    [false, true],
    [true, false],
    [true, true],
  ])(
    'preserves registration preparation for pilot=%s hardware=%s',
    async (pilot, hardware) => {
      global.cip30Enabled = pilot;
      const getAutomaticReceivingAddress = jest
        .fn()
        .mockResolvedValue({ id: 'pilot-address' });
      const getAddressesByWalletId = jest
        .fn()
        .mockResolvedValue([{ id: 'upstream-address' }]);
      const metadata = { 61284: { map: [{ k: { int: 4 }, v: { int: 123 } }] } };
      const votingData = {
        metadata,
        addressHex: 'abcd',
        absoluteSlotNumber: 123,
      };
      const selection = { fee: new BigNumber(0.2), metadata };
      const calculateTransactionFee = jest.fn().mockResolvedValue(selection);
      const selectCoins = jest.fn().mockResolvedValue(selection);
      const updateTxSignRequest = jest.fn();
      const initiateTransaction = jest.fn().mockResolvedValue(undefined);
      const instance = {
        selectedWalletId: 'wallet',
        _isMounted: true,
        setState: jest.fn(),
        props: {
          stores: {
            transactions: { calculateTransactionFee },
            addresses: { getAutomaticReceivingAddress, getAddressesByWalletId },
            wallets: { getWalletById: () => ({ isHardwareWallet: hardware }) },
            hardwareWallets: {
              selectCoins,
              updateTxSignRequest,
              initiateTransaction,
            },
            voting: {
              prepareVotingData: jest.fn().mockResolvedValue(votingData),
            },
            app: {},
          },
        },
      };
      await calculate.call(instance);
      const selectedAddress = pilot ? 'pilot-address' : 'upstream-address';
      expect(
        hardware ? selectCoins : calculateTransactionFee
      ).toHaveBeenCalledWith(
        expect.objectContaining({
          walletId: 'wallet',
          address: selectedAddress,
          ...(hardware ? { metadata } : {}),
        })
      );
      expect(updateTxSignRequest.mock.calls).toEqual(
        hardware ? [[selection]] : []
      );
      expect(initiateTransaction.mock.calls).toEqual(
        hardware ? [[{ walletId: 'wallet', votingData }]] : []
      );
      expect(
        hardware ? calculateTransactionFee : selectCoins
      ).not.toHaveBeenCalled();
      expect(instance.setState).toHaveBeenLastCalledWith({
        transactionFee: selection.fee,
        transactionFeeError: null,
      });
      expect(
        pilot ? getAddressesByWalletId : getAutomaticReceivingAddress
      ).not.toHaveBeenCalled();
    }
  );
});
