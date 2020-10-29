// @flow
export type CsvRecord = Array<string>;
export type GenerateCsvParams = {
  fileContent: Array<CsvRecord>,
  filePath: string,
};
