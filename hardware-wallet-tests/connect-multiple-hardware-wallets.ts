import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  createSequentialResult,
  initLedgerChannel,
  createTestInstructions,
  waitForZombieMessages,
} from './utils';

export const run = () => {
  expect.assertions(2);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  createTestInstructions([
    'Start test runner',
    'Plug Ledger Nano S to your computer',
    'Plug Ledger Nano S Plus to your computer',
    'Plug Ledger Nano X to your computer',
  ]);

  const getNextExpectedSequence = createSequentialResult([
    {
      disconnected: false,
      deviceModel: 'nanoS',
    },
    {
      disconnected: false,
      deviceModel: 'nanoSP',
    },
    {
      disconnected: false,
      deviceModel: 'nanoX',
    },
  ]);

  return new Promise<void>((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (message: { path: string; deviceModel: string }) => {
        const [expectedValue, isOver] = getNextExpectedSequence();
        expect(message).toEqual(expectedValue);

        if (isOver) {
          await waitForZombieMessages();
          resolve();
        }
      }
    );

    initLedgerChannel();
  });
};
