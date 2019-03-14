// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { DETECT_USER_LOCALE_CHANNEL } from '../../../common/ipc/api';
import type { DetectUserLocaleResponse } from '../../../common/ipc/api';

export const detectUserLocaleChannel: (
  RendererIpcChannel<DetectUserLocaleResponse, void>
) = new RendererIpcChannel(DETECT_USER_LOCALE_CHANNEL);
