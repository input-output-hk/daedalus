// @flow
export type CheckDiskSpaceResponse = {
  isNotEnoughDiskSpace: boolean,
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
  diskSpaceAvailable: string,
};
