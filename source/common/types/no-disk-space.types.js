// @flow
export type CheckDiskSpaceResponse = {
  isNotEnoughDiskSpace: boolean,
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
  diskSpaceAvailable: string,
  diskSpaceAvailableRaw: number,
  diskTotalSpaceRaw: number,
  hadNotEnoughSpaceLeft: boolean,
};
