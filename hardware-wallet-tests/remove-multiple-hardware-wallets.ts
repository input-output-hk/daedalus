import expect from 'expect';

import {
  createAndRegisterHardwareWalletChannels,
  createHardwareWalletConnectionChannel,
  createSequentialResult,
  initLedgerChannel,
  createTestInstructions,
} from './utils';

const getNextExpectedSequence = createSequentialResult([
  {
    disconnected: false,
    deviceModel: 'nanoS',
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
    deviceModel: 'nanoX',
  },
]);

export const run = () => {
  expect.assertions(4);

  createAndRegisterHardwareWalletChannels();

  const hardwareWalletConnectionChannel = createHardwareWalletConnectionChannel();

  createTestInstructions([
    'Connect Nano S',
    'Connect Nano X',
    'Disconnect Nano S',
    'Disconnect Nano X',
  ]);

  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(
      async (params: { path: string; deviceModel: string }) => {
        const [expectedValue, isOver] = getNextExpectedSequence();

        expect(params).toEqual(expectedValue);

        if (isOver) {
          return resolve(null);
        }
      }
    );

    initLedgerChannel();
  });
};
