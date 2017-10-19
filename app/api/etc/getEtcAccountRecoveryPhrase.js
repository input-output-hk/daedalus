// @flow
export type GetEtcAccountRecoveryPhraseResponse = string[];

export const getEtcAccountRecoveryPhrase = (): GetEtcAccountRecoveryPhraseResponse => {
  const mnemonics = 'chapter canoe member inch only plastic come album arrow mountain disagree settle';
  return mnemonics.split(' ');
};
