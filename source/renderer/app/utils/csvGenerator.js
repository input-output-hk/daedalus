// @flow
import { generateCsvChannel } from '../ipc/generateCsvChannel';
import type { CsvRecord } from '../../../common/types/csv-request.types';

type Params = {
  fileContent: Array<CsvRecord>,
  filePath: string,
};

export const downloadCsv = async ({ fileContent, filePath }: Params) => {
  await generateCsvChannel.send({
    fileContent,
    filePath,
  });
};
