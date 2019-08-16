// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DETECT_SYSTEM_DATE_LOCALE_CHANNEL } from '../../common/ipc/api';
import type {
  DetectSystemDateLocaleMainResponse,
  DetectSystemDateLocaleRendererRequest,
} from '../../common/ipc/api';

export const detectSystemDateLocaleChannel: MainIpcChannel<
  DetectSystemDateLocaleRendererRequest,
  DetectSystemDateLocaleMainResponse
> = new MainIpcChannel(DETECT_SYSTEM_DATE_LOCALE_CHANNEL);
