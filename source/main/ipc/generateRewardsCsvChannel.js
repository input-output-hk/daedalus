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
        // Prepare params
        const { rewards, filePath } = request;

        // Stringify rewards data
        csvStringify(rewards, (csvErr, output) => {
          if (csvErr) {
            return reject(csvErr);
          }

          // Write file to disk
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
