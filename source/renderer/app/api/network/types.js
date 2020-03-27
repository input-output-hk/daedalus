// @flow
export type TipInfo = {
  epoch: number,
  slot: number,
};

export type NextEpoch = {
  epochNumber: number,
  epochStart: string,
};

export type FutureEpoch = {
  epochNumber: number,
  epochStart: string,
};

export type ClockOffset = {
  quantity: number,
  unit: string,
};

export type GetNetworkInfoResponse = {
  syncProgress: number,
  localTip: TipInfo,
  networkTip: TipInfo,
  nextEpoch: NextEpoch,
  futureEpoch: FutureEpoch,
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
  next_epoch: {
    epoch_number: number,
    epoch_start_time: string,
  },
};

export type NetworkClockResponse = {
  status: 'available' | 'unavailable',
  offset?: ClockOffset,
};

export type GetNetworkClockResponse = NetworkClockResponse;
