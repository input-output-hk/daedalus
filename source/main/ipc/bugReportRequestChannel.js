// @flow
import http from 'http';
import FormData from 'form-data/lib/form_data';
import fs from 'fs';
import { extractFileNameFromPath } from '../../common/utils/files';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { SUBMIT_BUG_REPORT_REQUEST_CHANNEL } from '../../common/ipc/api';
import type {
  SubmitBugReportRequestMainResponse,
  SubmitBugReportRendererRequest,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';

/* eslint-disable consistent-return */

export const bugReportRequestChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  SubmitBugReportRendererRequest,
  SubmitBugReportRequestMainResponse
> = new MainIpcChannel(SUBMIT_BUG_REPORT_REQUEST_CHANNEL);

export const handleBugReportRequests = () => {
  bugReportRequestChannel.onReceive(
    (request: SubmitBugReportRendererRequest) =>
      new Promise((resolve, reject) => {
        logger.info('bugReportRequestChannel::onReceive', { request });
        const { httpOptions, requestPayload } = request;
        const options = Object.assign({}, httpOptions);
        const payload = Object.assign({}, requestPayload);
        // Prepare multipart/form-data
        const formData = new FormData();
        formData.append('payload', JSON.stringify(payload));

        // prepare file stream (attachment)
        if (payload.compressedLogsFilePath) {
          const stream = fs.createReadStream(payload.compressedLogsFilePath);
          const fileName = extractFileNameFromPath(
            payload.compressedLogsFilePath
          );
          formData.append(fileName, stream);
        }

        options.headers = formData.getHeaders();

        logger.info('Sending bug report request with options', { options });
        const httpRequest = http.request(options);
        httpRequest.on('response', response => {
          if (response.statusCode !== 200) {
            return reject();
          }
          response.on('data', () => {});
          response.on('error', error => {
            reject(error);
          });
          response.on('end', () => {
            resolve();
          });
        });
        httpRequest.on('error', error => reject(error));

        // Attach form-data and trigger the request
        formData.pipe(httpRequest);
      })
  );
};
