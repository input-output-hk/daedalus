// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DETECT_SYSTEM_LOCALE_CHANNEL } from '../../common/ipc/api';
import type { DetectSystemLocaleResponse } from '../../common/ipc/api';

export const detectSystemLocaleChannel: MainIpcChannel<
  void,
  DetectSystemLocaleResponse
> = new MainIpcChannel(DETECT_SYSTEM_LOCALE_CHANNEL);
