import { shell } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { OPEN_EXTERNAL_URL_CHANNEL } from '../../common/ipc/api';
import type {
  OpenExternalUrlMainResponse,
  OpenExternalUrlRendererRequest,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';

const ALLOWED_EXTERNAL_URL_PROTOCOL = 'https:';

// The URL parser lowercases the scheme, so case-variant input needs no extra handling.
const externalUrlScheme = (url: string): string => {
  try {
    return new URL(url).protocol;
  } catch {
    return 'unparseable';
  }
};

export const isAllowedExternalUrl = (url: string): boolean =>
  externalUrlScheme(url) === ALLOWED_EXTERNAL_URL_PROTOCOL;

export const handleOpenExternalUrl = (
  url: OpenExternalUrlRendererRequest
): Promise<OpenExternalUrlMainResponse> => {
  if (!isAllowedExternalUrl(url)) {
    logger.warn('Open external URL: rejected non-https scheme', {
      scheme: externalUrlScheme(url),
    });
    return Promise.reject(new Error('Rejected non-https external URL'));
  }
  return shell.openExternal(url) ? Promise.resolve() : Promise.reject();
};

// IpcChannel<Incoming, Outgoing>
export const openExternalUrlChannel: MainIpcChannel<
  OpenExternalUrlRendererRequest,
  OpenExternalUrlMainResponse
> = new MainIpcChannel(OPEN_EXTERNAL_URL_CHANNEL);
openExternalUrlChannel.onReceive(handleOpenExternalUrl);
