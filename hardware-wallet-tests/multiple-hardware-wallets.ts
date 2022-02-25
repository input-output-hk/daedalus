/* eslint-disable jest/no-standalone-expect */
import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  createSequentialResult,
  initLedgerChannel,
  createSequentialPromptMessages,
} from './utils';

export const run = () => {
  expect.assertions(2);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  const promptMessages = createSequentialPromptMessages([
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
      async (params: { path: string; deviceModel: string }) => {
        const [expectedValue, isOver] = expectedSequence();
        expect(params).toEqual(expectedValue);

        if (isOver) {
          resolve(null);
        }
      }
    );

    initLedgerChannel();
  });
};
