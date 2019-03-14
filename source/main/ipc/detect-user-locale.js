// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { DETECT_USER_LOCALE_CHANNEL } from '../../common/ipc/api';
import type { DetectUserLocaleResponse } from '../../common/ipc/api';

export const detectUserLocaleChannel: (
  MainIpcChannel<void, DetectUserLocaleResponse>
) = new MainIpcChannel(DETECT_USER_LOCALE_CHANNEL);
