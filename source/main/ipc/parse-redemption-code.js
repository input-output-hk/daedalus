// @flow
import { MainIpcChannel } from './lib/MainIpcChannel';
import { PARSE_REDEMPTION_CODE_CHANNEL } from '../../common/ipc/api';
import type {
  ParseRedemptionCodeRequest,
  ParseRedemptionCodeResponse
} from '../../common/ipc/api';

// CHANNEL
const parseRedemptionCodeChannel: (
  MainIpcChannel<ParseRedemptionCodeRequest, ParseRedemptionCodeResponse>
) = new MainIpcChannel(PARSE_REDEMPTION_CODE_CHANNEL);

// HANDLER
const parseRedemptionCodeHandler = async (request: ParseRedemptionCodeRequest) => {
  console.log(request);
  return Promise.resolve('test');
};

// SETUP
export const parseRedemptionCode = () => {
  parseRedemptionCodeChannel.onRequest(parseRedemptionCodeHandler);
};
