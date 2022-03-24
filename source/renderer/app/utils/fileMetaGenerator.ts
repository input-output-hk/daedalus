import { generateFileMetaChannel } from '../ipc/generateFileMetaChannel';

type Params = {
  filePath: string;
};
export const generateFileMeta = async ({ filePath }: Params) => {
  const fileMeta = await generateFileMetaChannel.send({
    filePath,
  });
  return fileMeta;
};
