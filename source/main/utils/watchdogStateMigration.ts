import { requestElectronStore } from '../ipc/electronStoreConversation';
import {
  STORAGE_KEYS as keys,
  STORAGE_TYPES as types,
} from '../../common/config/electron-store.config';
import {
  deleteRtsFlagsSettings,
  getRtsFlagsSettings,
} from './rtsFlagsSettings';

export type MigrateStateCommand = {
  cmd: 'migrate_state';
  chain_path: string | null;
  electron_flags: Array<string>;
  node_extra_args: Array<string>;
};

/**
 * The reply to the watchdog's `migrate_state_request`: the settings that
 * versions before watchdog-state.json kept in electron-store.
 */
export const buildMigrateStateCommand = (
  network: string
): MigrateStateCommand => {
  const chainPath =
    (requestElectronStore({
      type: types.GET,
      key: keys.CUSTOM_CHAIN_PATH,
    }) as string | undefined) ?? null;

  // Raw RTS flags are stored as e.g. ['-c']; cardano-node needs them wrapped
  // in +RTS/-RTS delimiters when they follow its own arguments.
  const rawRtsFlags = getRtsFlagsSettings(network) ?? [];
  const nodeExtraArgs =
    rawRtsFlags.length > 0 ? ['+RTS', ...rawRtsFlags, '-RTS'] : [];

  return {
    cmd: 'migrate_state',
    chain_path: chainPath,
    electron_flags: [],
    node_extra_args: nodeExtraArgs,
  };
};

/**
 * Delete the migrated keys once the watchdog reports `migrate_state_saved`.
 * Until then they stay, so a migration the watchdog did not complete is
 * retried on the next launch.
 */
export const forgetMigratedSettings = (network: string): void => {
  requestElectronStore({ type: types.DELETE, key: keys.CUSTOM_CHAIN_PATH });
  deleteRtsFlagsSettings(network);
};
