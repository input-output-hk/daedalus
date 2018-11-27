// @flow
import http from 'http';
import FormData from 'form-data/lib/form_data';
import fs from 'fs';
import { extractFileNameFromPath } from '../../common/utils/files';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { ReportRequestChannelName } from '../../common/ipc/api';
import type {
  ReportRequestMainResponse,
  ReportRequestRendererRequest
} from '../../common/ipc/api';
import { Logger} from '../utils/logging';
import { stringifyData } from '../../common/utils/logging';

export const reportRequestChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<ReportRequestRendererRequest, ReportRequestMainResponse>
) = (
  new MainIpcChannel(ReportRequestChannelName)
);

export const handleReportRequests = () => {
  reportRequestChannel.onReceive((request: ReportRequestRendererRequest) => (
    new Promise((resolve, reject) => {
      Logger.info(`reportRequestChannel::onReceive ${stringifyData(request)}`);
      const { httpOptions, requestPayload } = request;
      const options = Object.assign({}, httpOptions);
      const payload = Object.assign({}, requestPayload);
      // Prepare multipart/form-data
      const formData = new FormData();
      formData.append('payload', JSON.stringify(payload));

      // prepare file stream (attachment)
      if (payload.compressedLogsFile) {
        const stream = fs.createReadStream(payload.compressedLogsFile);
        const fileName = extractFileNameFromPath(payload.compressedLogsFile);
        formData.append(fileName, stream);
      }

      options.headers = formData.getHeaders();

      Logger.info(`Sending report request with options: ${stringifyData(options)}`);
      const httpRequest = http.request(options);
      httpRequest.on('response', (response) => {
        if (response.statusCode !== 200) {
          return reject();
        }
        response.on('data', () => {});
        response.on('error', (error) => {
          reject(error);
        });
        response.on('end', () => {
          resolve();
        });
      });
      httpRequest.on('error', (error) => reject(error));

      // Attach form-data and trigger the request
      formData.pipe(httpRequest);
    })
  ));
};
