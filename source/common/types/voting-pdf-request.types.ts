export type GenerateVotingPDFParams = {
  title: string;
  currentLocale: string;
  creationDate: string;
  qrCode: string;
  walletNameLabel: string;
  walletName: string;
  isMainnet: boolean;
  networkLabel: string;
  networkName: string;
  filePath: string;
  author: string;
};
