// @flow
import { IpcChannel } from './lib/IpcChannel';
import { TLS_CONFIG_CHANNEL } from '../../../common/ipc-api/tls-config';
import type { TlsConfig } from '../../../common/ipc-api/tls-config.js';

export const tlsConfigChannel: IpcChannel<void, TlsConfig> = (
  new IpcChannel(TLS_CONFIG_CHANNEL)
);
