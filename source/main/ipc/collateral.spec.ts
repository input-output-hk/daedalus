import { parseDappCollateralRequest } from './collateral';

jest.mock('../cip30/Cip30Broker', () => ({
  getCollateralService: jest.fn(),
}));
jest.mock('./dappBrowser', () => ({
  getCurrentDappRouteLease: jest.fn(),
}));

describe('collateral IPC', () => {
  it.each([
    'snapshot',
    'prepare',
    'cancel-preparation',
    'clear',
    'repair',
  ] as const)('accepts only the exact %s request', (type) => {
    expect(parseDappCollateralRequest({ type })).toEqual({ type });
    expect(() =>
      parseDappCollateralRequest({ type, walletId: 'renderer' })
    ).toThrow('Invalid collateral request');
  });

  it('accepts only an exact preparation transaction identity', () => {
    const request = {
      type: 'track-preparation' as const,
      transactionId: '12'.repeat(32),
    };
    expect(parseDappCollateralRequest(request)).toEqual(request);
    expect(() =>
      parseDappCollateralRequest({ ...request, transactionId: '12' })
    ).toThrow('Invalid collateral request');
  });

  it.each([undefined, null, {}, { type: 'submit' }, [], 'snapshot'])(
    'rejects malformed request %#',
    (value) => {
      expect(() => parseDappCollateralRequest(value)).toThrow(
        'Invalid collateral request'
      );
    }
  );
});
