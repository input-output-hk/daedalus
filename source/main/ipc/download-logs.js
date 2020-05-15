// @flow
import '@babel/polyfill'
import TransportNodeHid from "@ledgerhq/hw-transport-node-hid";
import fs from 'fs';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  DownloadLogsRendererRequest,
  DownloadLogsMainResponse,
} from '../../common/ipc/api';
import { DOWNLOAD_LOGS_CHANNEL } from '../../common/ipc/api';

export const downloadLogsChannel: MainIpcChannel<
  DownloadLogsRendererRequest,
  DownloadLogsMainResponse
> = new MainIpcChannel(DOWNLOAD_LOGS_CHANNEL);

export default () => {
  downloadLogsChannel.onRequest(async (request) => {
    try {
      // const TransportNodeHid = require("@ledgerhq/hw-transport-node-hid");
      // console.debug('>>> TransportNodeHid: ', TransportNodeHid.default);
      const aa = await TransportNodeHid.create();
      console.debug('>>> AA: ', aa);

      console.debug('>>> Device: ', aa.device);

      console.debug('>>> Device readTimeout: ', aa.device.readTimeout);
      console.debug('>>> Device events: ', aa.device._events);

      console.debug('>>> Device readTimeout SET: ', aa.device.readTimeout(15000));
      // console.debug('>>> TransportNodeHid - DEFAULT: ', JSON.stringify(TransportNodeHid.default));
    } catch (e) {
      console.debug('>>> I HAVE an error: ', e);
    }
    return Promise.resolve();
    // const { compressedLogsFilePath, destinationPath } = request;
//
    // if (!fs.existsSync(compressedLogsFilePath)) {
    //   return Promise.reject(new Error('File does not exist'));
    // }
//
    // const file = fs.readFileSync(compressedLogsFilePath);
    // fs.writeFileSync(destinationPath, file);
//
    // return Promise.resolve();
  });
};
