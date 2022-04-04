export type CsvRecord = Array<string>;
export type CsvFileContent = Array<CsvRecord>;
export type GenerateCsvParams = {
  fileContent: Array<CsvRecord>;
  filePath: string;
};
