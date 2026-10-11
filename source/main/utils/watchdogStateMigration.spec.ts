/**
 * @jest-environment node
 */
import {
  buildMigrateStateCommand,
  forgetMigratedSettings,
} from './watchdogStateMigration';
import {
  STORAGE_KEYS as keys,
  STORAGE_TYPES as types,
} from '../../common/config/electron-store.config';

jest.mock('../ipc/electronStoreConversation', () => ({
  requestElectronStore: jest.fn(),
}));

jest.mock('./rtsFlagsSettings', () => ({
  getRtsFlagsSettings: jest.fn(),
  deleteRtsFlagsSettings: jest.fn(),
}));

const { requestElectronStore } = jest.requireMock(
  '../ipc/electronStoreConversation'
);
const { getRtsFlagsSettings, deleteRtsFlagsSettings } =
  jest.requireMock('./rtsFlagsSettings');

describe('buildMigrateStateCommand', () => {
  it('carries the stored chain path and wraps the stored RTS flags', () => {
    requestElectronStore.mockReturnValue('D:\\Cardano');
    getRtsFlagsSettings.mockReturnValue(['-c']);

    expect(buildMigrateStateCommand('preprod')).toEqual({
      cmd: 'migrate_state',
      chain_path: 'D:\\Cardano',
      electron_flags: [],
      node_extra_args: ['+RTS', '-c', '-RTS'],
    });
    expect(requestElectronStore).toHaveBeenCalledWith({
      type: types.GET,
      key: keys.CUSTOM_CHAIN_PATH,
    });
    expect(getRtsFlagsSettings).toHaveBeenCalledWith('preprod');
  });

  it('sends no chain path and no node arguments when nothing is stored', () => {
    requestElectronStore.mockReturnValue(undefined);
    getRtsFlagsSettings.mockReturnValue(null);

    expect(buildMigrateStateCommand('mainnet')).toEqual({
      cmd: 'migrate_state',
      chain_path: null,
      electron_flags: [],
      node_extra_args: [],
    });
  });

  it('keeps the stored settings, so an unfinished migration can be retried', () => {
    requestElectronStore.mockReturnValue('/mnt/chain');
    getRtsFlagsSettings.mockReturnValue(['-c']);

    buildMigrateStateCommand('preprod');

    expect(requestElectronStore).not.toHaveBeenCalledWith(
      expect.objectContaining({ type: types.DELETE })
    );
    expect(deleteRtsFlagsSettings).not.toHaveBeenCalled();
  });
});

describe('forgetMigratedSettings', () => {
  it('deletes the chain path and the RTS flags for the network', () => {
    forgetMigratedSettings('preview');

    expect(requestElectronStore).toHaveBeenCalledWith({
      type: types.DELETE,
      key: keys.CUSTOM_CHAIN_PATH,
    });
    expect(deleteRtsFlagsSettings).toHaveBeenCalledWith('preview');
  });
});
