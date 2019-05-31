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

export type Platform = {
  signature: string,
  hash: string,
  URL: string,
  version: string,
  SHA256: string,
};

export type Platforms = {
  ['windows' | 'darwin' | 'linux']: Platform,
};

export type DaedalusLatestVersionResponse = {
  platforms: Platforms,
  linuxHash: string,
  macosSignature: string,
  win64: string,
  linuxSignature: string,
  linuxURL: string,
  macosSHA256: string,
  win64Hash: string,
  macosURL: string,
  win64URL: string,
  linuxSHA256: string,
  release_notes: ?string,
  macos: string,
  win64Signature: string,
  linux: string,
  macosHash: string,
  win64SHA256: string,
};

export type GetLatestAppVersionInfoResponse = DaedalusLatestVersionResponse;
