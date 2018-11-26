// @flow
export const DISK_SPACE_STATUS_CHANNEL = 'disk-space-status';

export type CheckDiskSpaceResponse = {
  diskSpaceAvailable: number,
  diskSpaceRequired: number,
  diskSpaceMissing: number,
  notEnoughSpace: boolean,
};
