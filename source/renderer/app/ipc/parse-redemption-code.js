// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { PARSE_REDEMPTION_CODE_CHANNEL } from '../../../common/ipc/api';
import type {
  ParseRedemptionCodeRequest,
  ParseRedemptionCodeResponse
} from '../../../common/ipc/api';

export const parseRedemptionCodeChannel: (
  RendererIpcChannel<ParseRedemptionCodeResponse, ParseRedemptionCodeRequest>
) = new RendererIpcChannel(PARSE_REDEMPTION_CODE_CHANNEL);
