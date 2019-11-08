// @flow
import fs from 'fs';
import csvStringify from 'csv-stringify';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_REWARDS_CSV_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateRewardsCsvMainResponse,
  GenerateRewardsCsvRendererRequest,
} from '../../common/ipc/api';

export const generateRewardsCsvChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateRewardsCsvRendererRequest,
  GenerateRewardsCsvMainResponse
> = new MainIpcChannel(GENERATE_REWARDS_CSV_CHANNEL);

export const handleRewardsCsvRequests = () => {
  generateRewardsCsvChannel.onReceive(
    (request: GenerateRewardsCsvRendererRequest) =>
      new Promise((resolve, reject) => {
        const { rewards, filePath } = request;

        csvStringify(rewards, (csvErr, output) => {
          if (csvErr) {
            return reject(csvErr);
          }

          return fs.writeFile(filePath, output, fileErr => {
            if (fileErr) {
              return reject(fileErr);
            }

            return resolve();
          });
        });
      })
  );
};
