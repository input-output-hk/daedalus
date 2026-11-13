import { spawnSync } from 'child_process';
import {
  handleAddressIntrospectionRequests,
  introspectAddressChannel,
} from './introspect-address';

jest.mock('child_process', () => ({ spawnSync: jest.fn() }));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onReceive: jest.fn() })),
}));

const mockSpawnSync = spawnSync as jest.Mock;
const address = `addr1${'q'.repeat(98)}`;

const introspect = (input: string) => {
  handleAddressIntrospectionRequests();
  const [[handler]] = (introspectAddressChannel.onReceive as jest.Mock).mock
    .calls;
  return handler({ input });
};

describe('handleAddressIntrospectionRequests', () => {
  it('passes the address to cardano-address on standard input', async () => {
    mockSpawnSync.mockReturnValue({
      stdout: Buffer.from('{"address_type":0}'),
      stderr: Buffer.from(''),
    });

    await introspect(address);

    expect(mockSpawnSync).toHaveBeenCalledWith(
      'cardano-address',
      ['address', 'inspect'],
      { input: address }
    );
  });

  it('resolves the parsed introspection', async () => {
    mockSpawnSync.mockReturnValue({
      stdout: Buffer.from('{"address_style":"Shelley","network_tag":1}'),
      stderr: Buffer.from(''),
    });

    await expect(introspect(address)).resolves.toEqual({
      introspection: { address_style: 'Shelley', network_tag: 1 },
    });
  });

  it('resolves Invalid when cardano-address reports a user error', async () => {
    mockSpawnSync.mockReturnValue({
      stdout: Buffer.from(''),
      stderr: Buffer.from('cardano-address: user error (invalid address)'),
    });

    await expect(introspect('not an address')).resolves.toBe('Invalid');
  });

  it('rejects with the error output of any other failure', async () => {
    mockSpawnSync.mockReturnValue({
      stdout: Buffer.from(''),
      stderr: Buffer.from('cardano-address: internal error'),
    });

    await expect(introspect(address)).rejects.toThrow(
      'cardano-address: internal error'
    );
  });

  it('rejects with the spawn error when cardano-address cannot be started', async () => {
    const spawnError = Object.assign(
      new Error('spawnSync cardano-address ENOENT'),
      { code: 'ENOENT' }
    );
    mockSpawnSync.mockReturnValue({
      error: spawnError,
      stdout: null,
      stderr: null,
    });

    await expect(introspect(address)).rejects.toBe(spawnError);
  });
});
