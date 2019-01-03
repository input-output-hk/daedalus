// @flow
export type CheckDiskSpaceResponse = {
  notEnoughSpace: boolean,
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
};
