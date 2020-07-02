// @flow
import { exec } from 'child_process';
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
        exec(
          `echo ${request.input} | cardano-address address inspect`,
          (error, stdout) => {
            if (
              error &&
              error.message.match(
                /user error \(Unrecognized address on standard input\)/g
              ) !== null
            ) {
              return resolve('Invalid');
            }
            if (error) {
              reject(error);
            }
            return resolve({ introspection: JSON.parse(stdout) });
          }
        );
      })
  );
};
