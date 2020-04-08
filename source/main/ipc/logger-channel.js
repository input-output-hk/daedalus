// @flow
import electronLog from 'electron-log-daedalus';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ELECTRON_LOG_CHANNEL } from '../../common/ipc/api';
import type {
  ElectronLogRenderRequest,
  ElectronLogMainResponse,
} from '../../common/ipc/api';

export const electronLogChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ElectronLogRenderRequest,
  ElectronLogMainResponse
> = new MainIpcChannel(ELECTRON_LOG_CHANNEL);

const funcMap = {
  debug: electronLog.debug,
  info: electronLog.info,
  error: electronLog.error,
  warn: electronLog.warn,
};

export const handleElectronLogRequests = () => {
  electronLogChannel.onReceive(
    (request: ElectronLogRenderRequest) =>
      new Promise(resolve => {
        const { level, args } = request;

        funcMap[level](...args);
        resolve();
      })
  );
};
