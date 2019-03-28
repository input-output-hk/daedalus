// @flow
export type LocalTimeInformationStatus =
  | 'unavailable'
  | 'pending'
  | 'available';

export type NodeInfoResponse = {
  syncProgress: {
    quantity: number,
    unit: 'percent',
  },
  blockchainHeight: ?{
    quantity: number,
    unit: ?'blocks',
  },
  localBlockchainHeight: {
    quantity: number,
    unit: ?'blocks',
  },
  localTimeInformation: {
    status: LocalTimeInformationStatus,
    localTimeDifference?: {
      quantity: number,
      unit: ?'microseconds',
    },
  },
  subscriptionStatus: Object,
};

export type NodeSettingsResponse = {
  slotDuration: {
    quantity: number,
    unit: ?'milliseconds',
  },
  softwareInfo: NodeSoftware,
  projectVersion: string,
  gitRevision: string,
  slotId: {
    slot: number,
    epoch: number,
  },
};

export type CardanoExplorerResponse = {
  Right: Array<number | Object>,
};

export type NodeSoftware = {
  applicationName: string,
  version: number,
};

// req/res Node Types

export type GetNetworkStatusResponse = {
  subscriptionStatus: Object,
  syncProgress: number,
  blockchainHeight: number,
  localBlockchainHeight: number,
  localTimeInformation: {
    status: LocalTimeInformationStatus,
    difference: ?number,
  },
};

export type GetNodeSettingsResponse = {
  slotId?: {
    epoch: number,
  },
};

export type GetCurrentEpochFallbackResponse = {
  currentEpoch: number,
};
