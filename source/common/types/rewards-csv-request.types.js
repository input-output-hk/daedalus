// @flow
export type CsvRecord = Array<string>;
export type GenerateRewardsCsvParams = {
  rewards: Array<CsvRecord>,
  filePath: string,
};
