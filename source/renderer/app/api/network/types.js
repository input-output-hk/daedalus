// @flow
export type LocalTimeInformationStatus =
  | 'unavailable'
  | 'pending'
  | 'available';

export type GetNetworkInfoResponse = {
  syncProgress: number,
  localTip: {
    epoch: number,
    slot: number,
  },
  networkTip: {
    epoch: number,
    slot: number,
  },
  localTimeInformation: {
    status: LocalTimeInformationStatus,
    difference: ?number,
  },
};

export type NetworkInfoResponse = {
  sync_progress: {
    status: 'ready' | 'syncing',
    progress?: {
      quantity: number,
      unit: 'percent',
    },
  },
  node_tip: {
    slot_number: number,
    epoch_number: number,
    height: {
      quantity: number,
      unit: 'block',
    },
  },
  network_tip: {
    slot_number: number,
    epoch_number: number,
  },
};
