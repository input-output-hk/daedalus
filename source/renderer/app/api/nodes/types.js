// @flow
export type LocalTimeInformationStatus = 'unavailable' | 'pending' | 'available';

export type NodeInfo = {
  syncProgress: {
    quantity: number,
    unit: 'percent'
  },
  blockchainHeight: ?{
    quantity: number,
    unit: ?'blocks'
  },
  localBlockchainHeight: {
    quantity: number,
    unit: ?'blocks'
  },
  localTimeInformation: {
    status: LocalTimeInformationStatus,
    localTimeDifference?: {
      quantity: number,
      unit: ?'microseconds'
    }
  },
  subscriptionStatus: Object
};

export type NodeSettings = {
  slotDuration: {
    quantity: number,
    unit: ?'milliseconds'
  },
  softwareInfo: NodeSoftware,
  projectVersion: string,
  gitRevision: string
};

export type NodeSoftware = {
  applicationName: string,
  version: number
};

// req/res Node Types
export type GetNetworkStatusResponse = {
  subscriptionStatus: Object,
  syncProgress: number,
  blockchainHeight: number,
  localBlockchainHeight: number,
  localTimeInformation: {
    status: LocalTimeInformationStatus,
    difference: ?number
  }
};

export type GetLatestAppVersionResponse = {
  latestAppVersion: ?string,
};

export type Platform = {
  hash: string,
  SHA256: string,
  signature: string,
  version: string,
  URL: string,
};

export type Platforms = {
  ['windows' | 'darwin' | 'linux']: Platform,
};

export type DaedalusLatestVersionResponse = {
  linux: string,
  linuxHash: string,
  linuxSHA256: string,
  linuxSignature: string,
  linuxURL: string,
  macos: string,
  macosHash: string,
  macosSHA256: string,
  macosSignature: string,
  macosURL: string,
  platforms: Platforms,
  release_notes: ?string,
  win64: string,
  win64Hash: string,
  win64SHA256: string,
  win64Signature: string,
  win64URL: string,
};

export type LatestAppVersionInfoResponse = DaedalusLatestVersionResponse;
