'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleAddressIntrospectionRequests = exports.introspectAddressChannel = void 0;
const child_process_1 = require('child_process');
const MainIpcChannel_1 = require('./lib/MainIpcChannel');
const api_1 = require('../../common/ipc/api');
// IpcChannel<Incoming, Outgoing>
exports.introspectAddressChannel = new MainIpcChannel_1.MainIpcChannel(
  api_1.INTROSPECT_ADDRESS_CHANNEL
);
const handleAddressIntrospectionRequests = () => {
  exports.introspectAddressChannel.onReceive(
    ({ input }) =>
      new Promise((resolve, reject) => {
        const { stdout, stderr } = (0, child_process_1.spawnSync)(
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
exports.handleAddressIntrospectionRequests = handleAddressIntrospectionRequests;
//# sourceMappingURL=introspect-address.js.map
