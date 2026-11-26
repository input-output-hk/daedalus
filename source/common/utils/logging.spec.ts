import {
  formatLogMessage,
  formatMessage,
  formatRendererConsoleMessage,
} from './logging';
import type { ElectronLoggerMessage } from '../types/logging.types';

const address = `addr1${'q'.repeat(98)}`;

describe('formatLogMessage', () => {
  it('redacts addresses in a text message', () => {
    expect(formatLogMessage(`Invalid receiver ${address}`)).toBe(
      'Invalid receiver [redacted-address]'
    );
  });

  it('reduces an error to its name and code', () => {
    const error = Object.assign(
      new Error(`spawnSync cardano-address ENOENT ${address}`),
      { code: 'ENOENT' }
    );

    expect(formatLogMessage(error)).toBe('Error ENOENT');
  });

  it('reduces an error without a code to its name', () => {
    expect(
      formatLogMessage(
        new TypeError("Cannot read properties of null (reading 'toString')")
      )
    ).toBe('TypeError');
  });

  it('converts other values to text', () => {
    expect(formatLogMessage(undefined)).toBe('undefined');
    expect(formatLogMessage(42)).toBe('42');
  });
});

describe('formatMessage', () => {
  const date = new Date('2026-11-19T05:42:47.000Z');
  const environmentData = {
    network: 'mainnet',
    os: 'Windows',
    platformVersion: '10.0.26100',
    version: '11.6.3',
  };
  const libraryText = 'electron-log.transports.file: Could not rotate log';
  const toLoggerMessage = (data: Array<unknown>) =>
    ({ date, level: 'warn', data }) as unknown as ElectronLoggerMessage;
  const format = (data: Array<unknown>) =>
    formatMessage(toLoggerMessage(data), environmentData);

  it('formats a message in the Daedalus shape with its own environment', () => {
    const result = format([
      '[daedalus.*mainnet*:info:ipcMain]',
      {
        message: 'Updating System-info.json file',
        data: { key: 'value' },
        environmentData: { ...environmentData, version: '1.0.0' },
      },
    ]);

    expect(result).toContain('[daedalus.*mainnet*:info:ipcMain]');
    expect(result).toContain('"msg": "Updating System-info.json file"');
    expect(result).toContain('"key": "value"');
    expect(result).toContain('"env": "mainnet:Windows:10.0.26100"');
    expect(result).toContain('"v1.0.0"');
  });

  it('formats the message electron-log sends when a rotation fails', () => {
    const error = Object.assign(new Error('EBUSY: resource busy or locked'), {
      code: 'EBUSY',
    });
    const result = format([libraryText, error]);

    expect(typeof result).toBe('string');
    expect(result).toContain(`${libraryText}: Error EBUSY`);
    expect(result).toContain('"env": "mainnet:Windows:10.0.26100"');
    expect(result).toContain('"v11.6.3"');
    expect(result).toContain('"sev": "warn"');
  });

  it('formats the message electron-log sends without an error', () => {
    const result = format([
      'electron-log.transports.file: Could not set a log file',
    ]);

    expect(typeof result).toBe('string');
    expect(result).toContain('Could not set a log file');
  });

  it('formats a library message whose second element is a string', () => {
    const result = format([libraryText, 'access denied']);

    expect(result).toContain(`${libraryText}: access denied`);
  });
});

describe('formatRendererConsoleMessage', () => {
  const date = new Date('2026-11-19T05:42:47.123Z');
  const libraryText = 'electron-log.transports.file: Could not rotate log';
  const format = (data: Array<unknown>) =>
    formatRendererConsoleMessage({
      date,
      level: 'warn',
      data,
    } as unknown as ElectronLoggerMessage);

  it('formats a message in the Daedalus shape as before', () => {
    expect(
      format([
        '[daedalus.*mainnet*:error:ipcMain]',
        {
          message: 'Failure',
          data: { key: 'value' },
          environmentData: {},
        },
      ])
    ).toBe(
      `[2026-11-19T05:42:47.123Z] [daedalus.*mainnet*:error:ipcMain] ${JSON.stringify(
        { msg: 'Failure', data: { key: 'value' } },
        null,
        2
      )}`
    );
  });

  it('formats the message electron-log sends when a rotation fails', () => {
    const error = Object.assign(new Error('EBUSY: resource busy or locked'), {
      code: 'EBUSY',
    });
    const result = format([libraryText, error]);

    expect(result).toContain(`${libraryText}: Error EBUSY`);
  });

  it('formats a library message without a second element', () => {
    expect(format([libraryText])).toContain(`"msg": "${libraryText}"`);
  });
});
