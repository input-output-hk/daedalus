export type TipInfo = {
  epoch: number;
  slot: number;
  absoluteSlotNumber: number;
};
export type NextEpoch = {
  epochNumber: number | null | undefined;
  epochStart: string | null | undefined;
};
export type FutureEpoch = {
  epochNumber: number | null | undefined;
  epochStart: string | null | undefined;
};
export type ClockOffset = {
  quantity: number;
  unit: 'microsecond';
};
export type SlotLength = {
  quantity: number;
  unit: string;
};
export type EpochLength = {
  quantity: number;
  unit: string;
};
export type SecurityParameter = {
  quantity: number;
  unit: string;
};
export type ActiveSlotCoefficient = {
  quantity: number;
  unit: string;
};
export type DecentralizationLevel = {
  quantity: number;
  unit: string;
};
export type MinimumUtxoValue = {
  quantity: number;
  unit: string;
};
export type GetNetworkInfoResponse = {
  syncProgress: number;
  localTip: TipInfo;
  networkTip: TipInfo | null | undefined;
  nextEpoch: NextEpoch | null | undefined;
};
export type NetworkInfoResponse = {
  sync_progress: {
    status: 'ready' | 'syncing';
    progress?: {
      quantity: number;
      unit: 'percent';
    };
  };
  node_tip: {
    slot_number: number;
    epoch_number: number;
    height: {
      quantity: number;
      unit: 'block';
    };
  };
  network_tip?:
    | {
        slot_number: number | null | undefined;
        epoch_number: number | null | undefined;
      }
    | null
    | undefined;
  next_epoch?:
    | {
        epoch_number: number | null | undefined;
        epoch_start_time: string | null | undefined;
      }
    | null
    | undefined;
};
export type NetworkClockResponse = {
  status: 'available' | 'unavailable' | 'pending';
  offset?: ClockOffset;
};
export type GetNetworkClockResponse = {
  status: 'available' | 'unavailable' | 'pending';
  offset: number | null | undefined;
};
export type HardforkAt = {
  epoch_start_time: string;
  epoch_number: number;
};
export type GetNetworkParametersResponse = {
  genesisBlockHash: string;
  blockchainStartTime: number;
  slotLength: SlotLength;
  epochLength: EpochLength;
  securityParameter: SecurityParameter;
  activeSlotCoefficient: ActiveSlotCoefficient;
  decentralizationLevel: DecentralizationLevel;
  desiredPoolNumber: number;
  minimumUtxoValue: MinimumUtxoValue;
  eras: {
    byron?: HardforkAt;
    shelley?: HardforkAt;
    allegra?: HardforkAt;
    mary?: HardforkAt;
  };
};
export type GetNetworkParametersApiResponse = {
  genesis_block_hash: string;
  blockchain_start_time: string;
  slot_length: SlotLength;
  epoch_length: EpochLength;
  security_parameter: SecurityParameter;
  active_slot_coefficient: ActiveSlotCoefficient;
  decentralization_level: DecentralizationLevel;
  desired_pool_number: number;
  minimum_utxo_value: MinimumUtxoValue;
  eras: {
    byron?: HardforkAt;
    shelley?: HardforkAt;
    allegra?: HardforkAt;
    mary?: HardforkAt;
  };
};
