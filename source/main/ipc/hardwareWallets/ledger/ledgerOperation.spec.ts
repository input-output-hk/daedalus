import AppAda from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { DeviceModelId, getDeviceModel } from '@ledgerhq/devices';
import { IpcChannel, IpcReceiver } from '../../../../common/ipc/lib/IpcChannel';
import { DeviceModels } from '../../../../common/types/hardware-wallets.types';
import { isLedgerUserRejection } from '../../../../common/utils/ledgerErrors';
import { logger } from '../../../utils/logging';
import { runLedgerOperation } from './ledgerOperation';

jest.mock('../../../utils/logging', () => ({
  logger: { error: jest.fn(), info: jest.fn() },
}));

const accountPath = [0x8000073c, 0x80000717, 0x80000000];
const key = Buffer.from('11'.repeat(32) + '22'.repeat(32), 'hex');
const serial = Buffer.from('01020304050607', 'hex');
const success = (data: Buffer) =>
  Buffer.concat([data, Buffer.from('9000', 'hex')]);

const createDevice = (major: number) => {
  let rejectNextExport = false;
  const send = jest.fn(
    async (...command: [number, number, number, number, Buffer]) => {
      const [, ins] = command;
      if (ins === 0)
        return success(Buffer.from([major, major === 7 ? 3 : 0, 0, 0]));
      if (ins === 1) return success(serial);
      if (ins === 0x10) {
        if (rejectNextExport) {
          rejectNextExport = false;
          throw Object.assign(new Error('Rejected'), {
            statusCode: major === 8 ? 0x6985 : 0x6e09,
          });
        }
        return success(key);
      }
      throw new Error(`Unexpected instruction ${ins}`);
    }
  );
  const transport = {
    send,
    decorateAppAPIMethods: jest.fn(),
  } as unknown as ConstructorParameters<typeof AppAda>[0];
  return {
    app: new AppAda(transport),
    send,
    rejectNextExport: () => {
      rejectNextExport = true;
    },
  };
};

describe.each([7, 8])('Ledger app v%i pairing protocol', (major) => {
  it('exports the Shelley account and retrieves serial with the actual SDK', async () => {
    const { app, send } = createDevice(major);
    const { compatibility } = await app.getVersion();
    expect(compatibility.isCompatible).toBe(true);
    await expect(
      runLedgerOperation('getExtendedPublicKey', () =>
        app.getExtendedPublicKey({ path: accountPath })
      )
    ).resolves.toEqual({
      publicKeyHex: '11'.repeat(32),
      chainCodeHex: '22'.repeat(32),
    });
    await expect(
      runLedgerOperation('getSerial', () => app.getSerial())
    ).resolves.toEqual({ serialHex: serial.toString('hex') });
    expect(send).toHaveBeenCalledWith(
      0xd7,
      0x10,
      0,
      0,
      Buffer.from('038000073c8000071780000000', 'hex')
    );
    expect(send).toHaveBeenCalledWith(0xd7, 1, 0, 0, Buffer.alloc(0));
  });

  it('preserves rejection status and allows the next export to succeed', async () => {
    const { app, rejectNextExport } = createDevice(major);
    rejectNextExport();
    const exportKey = () =>
      runLedgerOperation('getExtendedPublicKey', () =>
        app.getExtendedPublicKey({ path: accountPath })
      );
    await expect(exportKey()).rejects.toMatchObject({
      operation: 'getExtendedPublicKey',
      statusCode: major === 8 ? 0x6985 : 0x6e09,
    });
    expect(
      isLedgerUserRejection({ statusCode: major === 8 ? 0x6985 : 0x6e09 })
    ).toBe(true);
    await expect(exportKey()).resolves.toHaveProperty('publicKeyHex');
  });
});

describe('Ledger operation diagnostics', () => {
  it('uses the transport model identifier for Flex', () => {
    expect(DeviceModels.LEDGER_FLEX).toBe(
      getDeviceModel(DeviceModelId.europa).id
    );
  });

  it.each([0x6e09, 0x6985])(
    'recognizes SDK numeric rejection code %i',
    (code) => {
      expect(isLedgerUserRejection({ code })).toBe(true);
    }
  );

  it('preserves error details across the IPC response and JSON logging boundaries', async () => {
    const error = Object.assign(new Error('Device disconnected'), {
      name: 'DisconnectedDevice',
      code: 'DISCONNECTED',
      statusCode: 0x6e11,
      response: key,
      transport: { privateData: 'not-for-logs' },
    });
    const channel = new IpcChannel('ledger-operation-test');
    const receiver = { on: jest.fn() };
    channel.onRequest(
      () =>
        runLedgerOperation('getSerial', async () => {
          throw error;
        }),
      receiver as unknown as IpcReceiver
    );
    const send = jest.fn();
    await receiver.on.mock.calls[0][1](
      { sender: { send } },
      {
        requestId: 'ledger-operation-request',
        message: {},
      }
    );
    const expected = {
      operation: 'getSerial',
      name: 'DisconnectedDevice',
      message: 'Device disconnected',
      code: 'DISCONNECTED',
      statusCode: 0x6e11,
    };
    expect(send).toHaveBeenCalledWith('ledger-operation-test-response', {
      requestId: 'ledger-operation-request',
      isOk: false,
      response: expected,
    });
    expect(JSON.parse(JSON.stringify(send.mock.calls[0][1].response))).toEqual(
      expected
    );
    expect(logger.error).toHaveBeenLastCalledWith(
      '[HW-DEBUG] Ledger operation failed',
      expected
    );
    expect(isLedgerUserRejection(expected)).toBe(false);
  });

  it.each([null, undefined, {}, { message: 42 }])(
    'handles missing error details (%p)',
    async (error) => {
      await expect(
        runLedgerOperation('getVersion', async () => {
          throw error;
        })
      ).rejects.toEqual({
        operation: 'getVersion',
        name: 'Error',
        message: 'Ledger operation failed',
      });
    }
  );
});
