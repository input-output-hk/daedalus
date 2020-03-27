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

export type SlotLength = {
  quantity: number,
  unit: string,
};

export type EpochLength = {
  quantity: number,
  unit: string,
};

export type EpochStability = {
  quantity: number,
  unit: string,
};

export type ActiveSlotCoefficient = {
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

export type GetNetworkParametersResponse = {
  genesisBlockHash: string,
  blockchainStartTime: number,
  slotLength: SlotLength,
  epochLength: EpochLength,
  epochStability: EpochStability,
  activeSlotCoefficient: ActiveSlotCoefficient,
};

export type NetworkParametersResponse = {
  genesis_block_hash: string,
  blockchain_start_time: string,
  slot_length: SlotLength,
  epoch_length: EpochLength,
  epoch_stability: EpochStability,
  active_slot_coefficient: ActiveSlotCoefficient,
};
