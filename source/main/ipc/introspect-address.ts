import { spawnSync } from 'child_process';
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
    ({ input }: IntrospectAddressRendererRequest) =>
      new Promise((resolve, reject) => {
        const { stdout, stderr } = spawnSync(
          'cardano-address',
          ['address', 'inspect'],
          {
            input,
          }
        );

        if (stderr.toString() !== '') {
          if (stderr.toString().match(/user error/g) !== null) {
            return resolve('Invalid');
          }

          return reject(new Error(stderr.toString()));
        }

        return resolve({
          introspection: JSON.parse(stdout.toString()),
        });
      })
  );
};
