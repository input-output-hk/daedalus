export type MetadataInteger = {
  int: number;
};
export type MetadataString = {
  string: string;
};
export type MetadataBytes = {
  bytes: string;
};
export type MetadataList = {
  list: MetadataValue[];
};
export type MetadataMapValue = {
  k: MetadataValue;
  v: MetadataValue;
};
export type MetadataMap = {
  map: MetadataMapValue[];
};
export type MetadataValue =
  | MetadataInteger
  | MetadataString
  | MetadataBytes
  | MetadataList
  | MetadataMap;
export type TransactionMetadata = Record<string, MetadataValue>;
