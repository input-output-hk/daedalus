// @flow
export type CoreSystemInfo = {
  daedalusVersion: string,
  daedalusProcessID: string,
  daedalusMainProcessID: string,
  isBlankScreenFixActive: boolean,
  cardanoVersion: string,
  cardanoProcessID: number,
  cardanoAPIPort: number,
  cardanoNetwork: string,
  cardanoRawNetwork?: string,
  daedalusStateDirectoryPath: string,
};
