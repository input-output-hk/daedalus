import { generateCsvChannel } from '../ipc/generateCsvChannel';
import type { CsvFileContent } from '../../../common/types/csv-request.types';

type Params = {
  fileContent: CsvFileContent;
  filePath: string;
};
export const downloadCsv = async ({ fileContent, filePath }: Params) => {
  await generateCsvChannel.send({
    fileContent,
    filePath,
  });
};
