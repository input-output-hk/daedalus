export type CoreSystemInfo = {
  daedalusVersion: string;
  daedalusBuildNumber: string;
  daedalusProcessID: string;
  daedalusMainProcessID: string;
  isBlankScreenFixActive: boolean;
  cardanoNodeVersion: string;
  cardanoNodePID: number;
  cardanoNodeUptime: string;
  cardanoWalletVersion: string;
  cardanoWalletPID: number;
  cardanoWalletUptime: string;
  cardanoWalletRestartCount: number;
  cardanoWalletApiPort: number;
  cardanoNetwork: string;
  daedalusStateDirectoryPath: string;
};
