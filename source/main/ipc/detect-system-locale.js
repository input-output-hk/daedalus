// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DETECT_SYSTEM_LOCALE_CHANNEL } from '../../common/ipc/api';
import type {
  DetectSystemLocaleMainResponse,
  DetectSystemLocaleRendererRequest,
} from '../../common/ipc/api';

export const detectSystemLocaleChannel: MainIpcChannel<
  DetectSystemLocaleRendererRequest,
  DetectSystemLocaleMainResponse
> = new MainIpcChannel(DETECT_SYSTEM_LOCALE_CHANNEL);
