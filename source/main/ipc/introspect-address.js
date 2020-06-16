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

function extractValueFromLine (line) {
  return line.split(':')[1].trim()
}

export const handleAddressIntrospectionRequests = () => {
  introspectAddressChannel.onReceive(
    (request: IntrospectAddressRendererRequest) =>
      new Promise((resolve) => {
        exec(`echo ${request.input} | cardano-addresses address inspect`, (error, stdout) => {
          if (error) {
            return resolve(false)
          }
          const lines = stdout.split('\n')
          if (lines.length === 1) {
            return resolve({
              result: lines[0]
            })
          }
          const [addressStyleLine] = lines
          return resolve({
            addressStyle: extractValueFromLine(addressStyleLine)
          })
        })
      })
  )
}
