// @flow
export type CoreSystemInfo = {
  daedalusVersion: string,
  daedalusProcessID: string,
  daedalusMainProcessID: string,
  isBlankScreenFixActive: boolean,
  cardanoNodeVersion: string,
  cardanoNodePID: number,
  cardanoWalletVersion: string,
  cardanoWalletPID: number,
  cardanoWalletApiPort: number,
  cardanoNetwork: string,
  cardanoRawNetwork?: string,
  daedalusStateDirectoryPath: string,
};
