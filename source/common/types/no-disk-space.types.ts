export type CheckDiskSpaceResponse = {
  isNotEnoughDiskSpace: boolean;
  diskSpaceRequired: string;
  diskSpaceMissing: string;
  diskSpaceRecommended: string;
  diskSpaceAvailable: string;
  hadNotEnoughSpaceLeft: boolean;
  diskSpaceAvailableRaw: number;
  diskTotalSpaceRaw: number;
  isError: boolean;
};
