// @flow
import { exec } from 'child_process'
import { MainIpcChannel } from './lib/MainIpcChannel';
import { INTROSPECT_ADDRESS_CHANNEL } from '../../common/ipc/api';
import type {
  IntrospectAddressRendererRequest,
  IntrospectAddressMainResponse,
} from '../../common/ipc/api';

// IpcChannel<Incoming, Outgoing>

export const introspectAddressChannel: MainIpcChannel<
  IntrospectAddressRendererRequest,
  IntrospectAddressMainResponse
> = new MainIpcChannel(INTROSPECT_ADDRESS_CHANNEL);

export const handleAddressIntrospectionRequests = () => {
  introspectAddressChannel.onReceive(
    (request: IntrospectAddressRendererRequest) =>
      new Promise((resolve) => {
        exec(`echo ${request.input} | cardano-addresses address inspect`, (error, stdout) => {
          if (error) {
            return resolve(false)
          }
          return resolve({ introspection: JSON.parse(stdout) })
        })
      })
  )
}
