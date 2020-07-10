// @flow
import { spawn } from 'child_process';
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
      new Promise((resolve, reject) => {
        const introspect = spawn('cardano-address', ['address', 'inspect']);

        introspect.stderr.on('error', (error) => {
          if (
            error &&
            error.message.match(
              /user error \(Unrecognized address on standard input\)/g
            ) !== null
          ) {
            return resolve('Invalid');
          }
          return reject(error)
        });

        introspect.stdout.on('data', (data) => {
          return resolve({ introspection: JSON.parse(data) })
        });

        introspect.stdin.write(request.input)
      })
  );
};
