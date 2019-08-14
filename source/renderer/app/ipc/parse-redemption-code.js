// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { PARSE_REDEMPTION_CODE_CHANNEL } from '../../../common/ipc/api';
import type {
  ParseRedemptionCodeRendererRequest,
  ParseRedemptionCodeMainResponse,
} from '../../../common/ipc/api';

export const parseRedemptionCodeChannel: RendererIpcChannel<
  ParseRedemptionCodeMainResponse,
  ParseRedemptionCodeRendererRequest
> = new RendererIpcChannel(PARSE_REDEMPTION_CODE_CHANNEL);
