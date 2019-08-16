// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { DETECT_SYSTEM_DATE_LOCALE_CHANNEL } from '../../../common/ipc/api';
import type {
  DetectSystemDateLocaleMainResponse,
  DetectSystemDateLocaleRendererRequest,
} from '../../../common/ipc/api';

export const detectSystemDateLocaleChannel: RendererIpcChannel<
  DetectSystemDateLocaleMainResponse,
  DetectSystemDateLocaleRendererRequest
> = new RendererIpcChannel(DETECT_SYSTEM_DATE_LOCALE_CHANNEL);
