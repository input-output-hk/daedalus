import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  createSequentialResult,
  initLedgerChannel,
  createTestInstructions,
} from './utils';

export const run = () => {
  expect.assertions(2);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  const promptMessages = createTestInstructions([
    'Start test runner',
    'Plug Ledger Nano S to your computer',
    'Plug Ledger Nano X to your computer',
  ]);

  promptMessages();

  const expectedSequence = createSequentialResult([
    {
      disconnected: false,
      deviceModel: 'nanoS',
    },
    {
      disconnected: false,
      deviceModel: 'nanoX',
    },
  ]);

  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (message: { path: string; deviceModel: string }) => {
        const [expectedValue, isOver] = expectedSequence();
        expect(message).toEqual(expectedValue);

        if (isOver) {
          resolve(null);
        }
      }
    );

    initLedgerChannel();
  });
};
