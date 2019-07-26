// @flow
import { generateFileContentChannel } from '../ipc/generateFileContentChannel';

type Params = { filePath: string };

export const generateFileContent = async ({ filePath }: Params) => {
  const fileContent = await generateFileContentChannel.send({ filePath });

  return fileContent;
};
