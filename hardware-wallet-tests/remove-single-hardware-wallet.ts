/* eslint-disable jest/no-standalone-expect */
import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  initLedgerChannel,
  createSequentialPromptMessages,
  createSequentialResult,
} from './utils';

export const run = () => {
  expect.assertions(3);

  const promptMessages = createSequentialPromptMessages([
    'Plug Ledger Nano S to your computer',
    'Disconnect Nano S',
  ]);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  const expectedSequence = createSequentialResult([
    {
      disconnected: false,
    },
    {
      disconnected: true,
    },
  ]);

  promptMessages();

  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (params: { path: string }) => {
        const [expectedValue, isOver] = expectedSequence();
        expect(params).toEqual(expectedValue);

        if (isOver) return resolve(null);
        promptMessages();
      }
    );

    initLedgerChannel();
  });
};
