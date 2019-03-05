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
