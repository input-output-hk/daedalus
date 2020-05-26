// @flow
import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { GET_HARDWARE_WALLET_TRANSPORT_CHANNEL, GET_EXTENDED_PUBLIC_KEY_CHANNEL, GET_CARDANO_ADA_APP_CHANNEL } from '../../../common/ipc/api';
import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
  getExtendedPublicKeytRendererRequest,
  getExtendedPublicKeytMainResponse,
} from '../../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>
export const getHardwareWalletTransportChannel: RendererIpcChannel<
  getHardwareWalletTransportMainResponse,
  getHardwareWalletTransportRendererRequest
> = new RendererIpcChannel(GET_HARDWARE_WALLET_TRANSPORT_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getExtendedPublicKeyChannel: RendererIpcChannel<
  getExtendedPublicKeytMainResponse,
  getExtendedPublicKeytRendererRequest
> = new RendererIpcChannel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);

// IpcChannel<Incoming, Outgoing>
export const getCardanoAdaAppChannel: RendererIpcChannel<
  getCardanoAdaApptMainResponse,
  getCardanoAdaApptRendererRequest
> = new RendererIpcChannel(GET_CARDANO_ADA_APP_CHANNEL);
