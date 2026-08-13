import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const openExternalUrlChannel: MainIpcChannel<
  OpenExternalUrlRendererRequest,
  OpenExternalUrlMainResponse
> = new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);

export const normalizeExternalUrl = (value: unknown): string => {
  if (typeof value !== 'string') throw new Error('External URL is not allowed');

  try {
    const url = new URL(value);
    if (
      url.protocol !== 'https:' ||
      !url.hostname ||
      url.username ||
      url.password
    ) {
      throw new Error();
    }
    return url.href;
  } catch (_error) {
    throw new Error('External URL is not allowed');
  }
};

export const openExternalUrl = async (value: unknown): Promise<void> => {
  const url = normalizeExternalUrl(value);
  try {
    await shell.openExternal(url);
  } catch (_error) {
    throw new Error('Unable to open external URL');
  }
};

export const registerOpenExternalUrlChannel = (): void => {
  openExternalUrlChannel.onReceive(openExternalUrl);
};
