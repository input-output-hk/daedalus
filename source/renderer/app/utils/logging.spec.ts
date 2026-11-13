import type { Logger } from '../../../common/types/logging.types';

const mockElectronLog = {
  debug: jest.fn(),
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
};

Object.assign(global, {
  environment: {
    network: 'mainnet',
    os: 'Linux',
    platformVersion: 'test',
    version: 'test',
  },
  electronLog: mockElectronLog,
});

// The module reads the globals on load, so it is required after they are set.
// eslint-disable-next-line @typescript-eslint/no-var-requires
const { logger }: { logger: Logger } = require('./logging');

describe('logger', () => {
  it('logs an error passed as the message by name and code', () => {
    const error = Object.assign(new Error('spawnSync cardano-address ENOENT'), {
      code: 'ENOENT',
    });

    expect(() => logger.error(error as unknown as string)).not.toThrow();
    expect(mockElectronLog.error).toHaveBeenCalledWith(
      '[daedalus.*mainnet*:error:ipcRenderer]',
      expect.objectContaining({ message: 'Error ENOENT' })
    );
  });

  it('redacts a text message', () => {
    logger.warn(`Invalid receiver addr1${'q'.repeat(98)}`);

    expect(mockElectronLog.warn).toHaveBeenCalledWith(
      '[daedalus.*mainnet*:warn:ipcRenderer]',
      expect.objectContaining({
        message: 'Invalid receiver [redacted-address]',
      })
    );
  });
});
