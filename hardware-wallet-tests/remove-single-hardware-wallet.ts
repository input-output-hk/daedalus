import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  initLedgerChannel,
  createTestInstructions,
  createSequentialResult,
} from './utils';

export const run = () => {
  expect.assertions(3);

  createTestInstructions([
    'Plug Ledger Nano S to your computer',
    'Disconnect Nano S',
  ]);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  const getNextExpectedSequence = createSequentialResult([
    {
      disconnected: false,
    },
    {
      disconnected: true,
    },
  ]);

  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (params: { path: string }) => {
        const [expectedValue, isOver] = getNextExpectedSequence();
        expect(params).toEqual(expectedValue);

        if (isOver) return resolve(null);
      }
    );

    initLedgerChannel();
  });
};
