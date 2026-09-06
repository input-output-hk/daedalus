import fs from 'fs';
import csvStringify from 'csv-stringify';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GENERATE_CSV_CHANNEL } from '../../common/ipc/api';
import type {
  GenerateCsvMainResponse,
  GenerateCsvRendererRequest,
} from '../../common/ipc/api';

export const generateCsvChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  GenerateCsvRendererRequest,
  GenerateCsvMainResponse
> = new MainIpcChannel(GENERATE_CSV_CHANNEL);
export const handleRewardsCsvRequests = () => {
  generateCsvChannel.onReceive(
    (request: GenerateCsvRendererRequest) =>
      new Promise((resolve, reject) => {
        const { fileContent, filePath } = request;
        csvStringify(fileContent, (csvErr, output) => {
          if (csvErr) {
            return reject(csvErr);
          }

          return fs.writeFile(filePath, output, (fileErr) => {
            if (fileErr) {
              return reject(fileErr);
            }

            return resolve();
          });
        });
      })
  );
};
