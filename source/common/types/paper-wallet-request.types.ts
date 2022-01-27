export type GeneratePaperWalletParams = {
  address: string;
  filePath: string;
  mnemonics: Array<string>;
  isMainnet: boolean;
  buildLabel: string;
  timestamp: string;
  messages: {
    walletAddressLabel: string;
    recoveryPhraseLabel: string;
    infoTitle: string;
    infoAuthor: string;
  };
};
