// @flow
export type CoreSystemInfo = {
  daedalusVersion: string,
  daedalusProcessID: string,
  daedalusMainProcessID: string,
  isInSafeMode: boolean,
  cardanoVersion: string,
  cardanoProcessID: number,
  cardanoAPIPort: number,
  cardanoNetwork: string,
  daedalusStateDirectory: string,
};
