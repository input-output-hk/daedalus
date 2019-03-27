// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { DETECT_SYSTEM_LOCALE_CHANNEL } from '../../../common/ipc/api';
import type {
  DetectSystemLocaleMainResponse,
  DetectSystemLocaleRendererRequest,
} from '../../../common/ipc/api';

export const detectSystemLocaleChannel: RendererIpcChannel<
  DetectSystemLocaleMainResponse,
  DetectSystemLocaleRendererRequest
> = new RendererIpcChannel(DETECT_SYSTEM_LOCALE_CHANNEL);
