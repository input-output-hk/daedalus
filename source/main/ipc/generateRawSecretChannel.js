// @flow
import cbor from 'cbor';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GenerateRawSecretChannelName } from '../../common/ipc/api';
import type {
  GenerateRawSecretRendererRequest,
  GenerateRawSecretMainResponse,
} from '../../common/ipc/api';

export const generateRawSecretChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<GenerateRawSecretRendererRequest, GenerateRawSecretMainResponse>
) = (
  new MainIpcChannel(GenerateRawSecretChannelName)
);

export const handleGenerateRawSecretRequests = () => {
  generateRawSecretChannel.onReceive((request: GenerateRawSecretRendererRequest) => (
    new Promise((resolve) => {
      const { wallet } = request;
      const rawSecret = cbor.encode(wallet.raw).toString('hex'); // EncryptedSecretKey
      resolve(rawSecret);
    })
  ));
};
