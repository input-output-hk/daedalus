import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  createSequentialResult,
  initLedgerChannel,
  createTestInstructions,
  waitForZombieMessages,
} from './utils';

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
  {
    disconnected: true,
    deviceModel: 'nanoS',
  },
  {
    disconnected: true,
    deviceModel: 'nanoSP',
  },
  {
    disconnected: true,
    deviceModel: 'nanoX',
  },
]);

export const run = () => {
  expect.assertions(4);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  createTestInstructions([
    'Connect Nano S',
    'Connect Nano S Plus',
    'Connect Nano X',
    'Disconnect Nano S',
    'Disconnect Nano S Plus',
    'Disconnect Nano X',
  ]);

  return new Promise<void>((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (params: { path: string; deviceModel: string }) => {
        const [expectedValue, isOver] = getNextExpectedSequence();

        expect(params).toEqual(expectedValue);

        if (isOver) {
          await waitForZombieMessages();
          return resolve();
        }
      }
    );

    initLedgerChannel();
  });
};
