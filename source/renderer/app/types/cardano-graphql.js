// @flow

/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {|
  ID: string,
  String: string,
  Boolean: boolean,
  Int: number,
  Float: number,
  AssetFingerprint: any,
  BigInt: any,
  DateTime: any,
  Hash28Hex: any,
  Hash32Hex: any,
  IPv4: any,
  IPv6: any,
  JSON: any,
  JSONObject: any,
  Percentage: any,
  StakeAddress: any,
  StakePoolID: any,
  Timestamp: any,
  VRFVerificationKey: any,
|};

export type ActiveStake = {|
  __typename?: 'ActiveStake',
  address: $ElementType<Scalars, 'StakeAddress'>,
  amount: $ElementType<Scalars, 'String'>,
  epoch?: ?Epoch,
  epochNo: $ElementType<Scalars, 'Int'>,
  stakePoolHash: $ElementType<Scalars, 'Hash28Hex'>,
  stakePoolId: $ElementType<Scalars, 'StakePoolID'>,
  registeredWith: StakePool,
|};

export type ActiveStake_Aggregate = {|
  __typename?: 'ActiveStake_aggregate',
  aggregate?: ?ActiveStake_Aggregate_Fields,
|};

export type ActiveStake_Aggregate_Fields = {|
  __typename?: 'ActiveStake_aggregate_fields',
  avg: ActiveStake_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: ActiveStake_Max_Fields,
  min: ActiveStake_Min_Fields,
  sum: ActiveStake_Sum_Fields,
|};

export type ActiveStake_Avg_Fields = {|
  __typename?: 'ActiveStake_avg_fields',
  amount?: ?$ElementType<Scalars, 'Float'>,
|};

export type ActiveStake_Bool_Exp = {|
  _and?: ?Array<?ActiveStake_Bool_Exp>,
  _not?: ?ActiveStake_Bool_Exp,
  _or?: ?Array<?ActiveStake_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  amount?: ?Text_Comparison_Exp,
  epoch?: ?Epoch_Bool_Exp,
  epochNo?: ?Int_Comparison_Exp,
  stakePoolHash?: ?Hash28Hex_Comparison_Exp,
  stakePoolId?: ?StakePoolId_Comparison_Exp,
  registeredWith?: ?StakePool_Bool_Exp,
|};

export type ActiveStake_Max_Fields = {|
  __typename?: 'ActiveStake_max_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type ActiveStake_Min_Fields = {|
  __typename?: 'ActiveStake_min_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type ActiveStake_Order_By = {|
  address?: ?Order_By,
  amount?: ?Order_By,
  epoch?: ?Epoch_Order_By,
  epochNo?: ?Order_By,
  stakePoolHash?: ?Order_By,
  stakePoolId?: ?Order_By,
  registeredWith?: ?StakePool_Order_By,
|};

export type ActiveStake_Sum_Fields = {|
  __typename?: 'ActiveStake_sum_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Ada = {|
  __typename?: 'Ada',
  supply: AssetSupply,
|};

export type AdaPots = {|
  __typename?: 'AdaPots',
  deposits: $ElementType<Scalars, 'String'>,
  fees: $ElementType<Scalars, 'String'>,
  reserves: $ElementType<Scalars, 'String'>,
  rewards: $ElementType<Scalars, 'String'>,
  slotNo: $ElementType<Scalars, 'Int'>,
  treasury: $ElementType<Scalars, 'String'>,
  utxo: $ElementType<Scalars, 'String'>,
|};

export type Asset = {|
  __typename?: 'Asset',
  assetId: $ElementType<Scalars, 'String'>,
  assetName?: ?$ElementType<Scalars, 'String'>,
  description?: ?$ElementType<Scalars, 'String'>,
  fingerprint?: ?$ElementType<Scalars, 'AssetFingerprint'>,
  logo?: ?$ElementType<Scalars, 'String'>,
  metadataHash?: ?$ElementType<Scalars, 'String'>,
  tokenMints: Array<?TokenMint>,
  tokenMints_aggregate: TokenMint_Aggregate,
  name?: ?$ElementType<Scalars, 'String'>,
  policyId: $ElementType<Scalars, 'Hash28Hex'>,
  ticker?: ?$ElementType<Scalars, 'String'>,
  url?: ?$ElementType<Scalars, 'String'>,
|};


export type AssetTokenMintsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<TokenMint_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TokenMint_Bool_Exp,
|};


export type AssetTokenMints_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<TokenMint_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TokenMint_Bool_Exp,
|};

export type AssetBalance = {|
  __typename?: 'AssetBalance',
  asset: Asset,
  quantity: $ElementType<Scalars, 'String'>,
|};


export type AssetFingerprint_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'String'>,
  _in?: ?Array<?$ElementType<Scalars, 'String'>>,
  _neq?: ?$ElementType<Scalars, 'String'>,
  _nin?: ?Array<?$ElementType<Scalars, 'String'>>,
|};

export type AssetSupply = {|
  __typename?: 'AssetSupply',
  circulating: $ElementType<Scalars, 'String'>,
  max: $ElementType<Scalars, 'String'>,
  total?: ?$ElementType<Scalars, 'String'>,
|};

export type AssetSupply_Bool_Exp = {|
  _and?: ?Array<?AssetSupply_Bool_Exp>,
  _not?: ?AssetSupply_Bool_Exp,
  _or?: ?Array<?AssetSupply_Bool_Exp>,
  circulating?: ?Text_Comparison_Exp,
  max?: ?Text_Comparison_Exp,
  total?: ?Text_Comparison_Exp,
|};

export type AssetSupply_Order_By = {|
  circulating?: ?Order_By,
  max?: ?Order_By,
  total?: ?Order_By,
|};

export type Asset_Aggregate = {|
  __typename?: 'Asset_aggregate',
  aggregate?: ?Asset_Aggregate_Fields,
|};

export type Asset_Aggregate_Fields = {|
  __typename?: 'Asset_aggregate_fields',
  count: $ElementType<Scalars, 'String'>,
|};

export type Asset_Bool_Exp = {|
  _and?: ?Array<?Asset_Bool_Exp>,
  _not?: ?Asset_Bool_Exp,
  _or?: ?Array<?Asset_Bool_Exp>,
  ticker?: ?Text_Comparison_Exp,
  assetId?: ?Text_Comparison_Exp,
  assetName?: ?Text_Comparison_Exp,
  description?: ?Text_Comparison_Exp,
  fingerprint?: ?AssetFingerprint_Comparison_Exp,
  logo?: ?Text_Comparison_Exp,
  name?: ?Text_Comparison_Exp,
  policyId?: ?Text_Comparison_Exp,
  url?: ?Text_Comparison_Exp,
|};

export type Asset_Order_By = {|
  assetId?: ?Order_By,
  fingerprint?: ?Order_By,
  name?: ?Order_By,
|};


export type BigInt_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'BigInt'>,
  _gt?: ?$ElementType<Scalars, 'BigInt'>,
  _gte?: ?$ElementType<Scalars, 'BigInt'>,
  _in?: ?Array<$ElementType<Scalars, 'BigInt'>>,
  _is_null?: ?$ElementType<Scalars, 'Boolean'>,
  _lt?: ?$ElementType<Scalars, 'BigInt'>,
  _lte?: ?$ElementType<Scalars, 'BigInt'>,
  _neq?: ?$ElementType<Scalars, 'BigInt'>,
  _nin?: ?Array<$ElementType<Scalars, 'BigInt'>>,
|};

export type Block = {|
  __typename?: 'Block',
  epoch?: ?Epoch,
  epochNo?: ?$ElementType<Scalars, 'Int'>,
  fees: $ElementType<Scalars, 'BigInt'>,
  forgedAt: $ElementType<Scalars, 'DateTime'>,
  slotLeader: SlotLeader,
  hash: $ElementType<Scalars, 'Hash32Hex'>,
  merkleRoot?: ?$ElementType<Scalars, 'Hash32Hex'>,
  number?: ?$ElementType<Scalars, 'Int'>,
  opCert?: ?$ElementType<Scalars, 'Hash32Hex'>,
  slotInEpoch?: ?$ElementType<Scalars, 'Int'>,
  slotNo?: ?$ElementType<Scalars, 'Int'>,
  previousBlock?: ?Block,
  protocolVersion?: ?$ElementType<Scalars, 'JSONObject'>,
  nextBlock?: ?Block,
  size: $ElementType<Scalars, 'BigInt'>,
  transactions: Array<?Transaction>,
  transactions_aggregate: Transaction_Aggregate,
  transactionsCount: $ElementType<Scalars, 'String'>,
  vrfKey?: ?$ElementType<Scalars, 'VRFVerificationKey'>,
|};


export type BlockTransactionsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Transaction_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Transaction_Bool_Exp,
|};


export type BlockTransactions_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Transaction_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Transaction_Bool_Exp,
|};

export type Block_Aggregate = {|
  __typename?: 'Block_aggregate',
  aggregate?: ?Block_Aggregate_Fields,
|};

export type Block_Aggregate_Fields = {|
  __typename?: 'Block_aggregate_fields',
  avg: Block_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: Block_Max_Fields,
  min: Block_Min_Fields,
  sum: Block_Sum_Fields,
|};

export type Block_Avg_Fields = {|
  __typename?: 'Block_avg_fields',
  fees?: ?$ElementType<Scalars, 'Float'>,
  size?: ?$ElementType<Scalars, 'Float'>,
|};

export type Block_Bool_Exp = {|
  _and?: ?Array<?Block_Bool_Exp>,
  _not?: ?Block_Bool_Exp,
  _or?: ?Array<?Block_Bool_Exp>,
  forgedAt?: ?Date_Comparison_Exp,
  slotLeader?: ?SlotLeader_Bool_Exp,
  epoch?: ?Epoch_Bool_Exp,
  fees?: ?BigInt_Comparison_Exp,
  hash?: ?Hash32Hex_Comparison_Exp,
  number?: ?Int_Comparison_Exp,
  previousBlock?: ?Block_Bool_Exp,
  nextBlock?: ?Block_Bool_Exp,
  size?: ?BigInt_Comparison_Exp,
  slotInEpoch?: ?Int_Comparison_Exp,
  slotNo?: ?Int_Comparison_Exp,
  transactions?: ?Transaction_Bool_Exp,
  transactionsCount?: ?Text_Comparison_Exp,
  vrfKey?: ?VrfVerificationKey_Comparison_Exp,
|};

export type Block_Max_Fields = {|
  __typename?: 'Block_max_fields',
  fees?: ?$ElementType<Scalars, 'String'>,
  size?: ?$ElementType<Scalars, 'String'>,
|};

export type Block_Min_Fields = {|
  __typename?: 'Block_min_fields',
  fees?: ?$ElementType<Scalars, 'String'>,
  size?: ?$ElementType<Scalars, 'String'>,
|};

export type Block_Order_By = {|
  forgedAt?: ?Order_By,
  slotLeader?: ?SlotLeader_Order_By,
  epoch?: ?Epoch_Order_By,
  fees?: ?Order_By,
  hash?: ?Order_By,
  number?: ?Order_By_With_Nulls,
  size?: ?Order_By,
  slotNo?: ?Order_By_With_Nulls,
  transactionsCount?: ?Order_By,
  vrfKey?: ?Order_By_With_Nulls,
|};

export type Block_Sum_Fields = {|
  __typename?: 'Block_sum_fields',
  fees?: ?$ElementType<Scalars, 'String'>,
  size?: ?$ElementType<Scalars, 'String'>,
|};

export type ByronBlockVersionData = {|
  __typename?: 'ByronBlockVersionData',
  scriptVersion: $ElementType<Scalars, 'Int'>,
  slotDuration: $ElementType<Scalars, 'Int'>,
  maxBlockSize: $ElementType<Scalars, 'Int'>,
  maxHeaderSize: $ElementType<Scalars, 'Int'>,
  maxTxSize: $ElementType<Scalars, 'Int'>,
  maxProposalSize: $ElementType<Scalars, 'Int'>,
  mpcThd: $ElementType<Scalars, 'String'>,
  heavyDelThd: $ElementType<Scalars, 'String'>,
  updateVoteThd: $ElementType<Scalars, 'String'>,
  updateProposalThd: $ElementType<Scalars, 'String'>,
  updateImplicit: $ElementType<Scalars, 'String'>,
  softforkRule: ByronSoftForkRule,
  txFeePolicy: ByronTxFeePolicy,
  unlockStakeEpoch: $ElementType<Scalars, 'String'>,
|};

export type ByronGenesis = {|
  __typename?: 'ByronGenesis',
  bootStakeholders: $ElementType<Scalars, 'JSONObject'>,
  heavyDelegation: $ElementType<Scalars, 'JSONObject'>,
  startTime: $ElementType<Scalars, 'Timestamp'>,
  nonAvvmBalances: $ElementType<Scalars, 'JSONObject'>,
  blockVersionData: ByronBlockVersionData,
  protocolConsts: ByronProtocolConsts,
  avvmDistr: $ElementType<Scalars, 'JSONObject'>,
|};

export type ByronProtocolConsts = {|
  __typename?: 'ByronProtocolConsts',
  k: $ElementType<Scalars, 'Int'>,
  protocolMagic?: ?$ElementType<Scalars, 'Int'>,
|};

export type ByronSoftForkRule = {|
  __typename?: 'ByronSoftForkRule',
  initThd: $ElementType<Scalars, 'String'>,
  minThd: $ElementType<Scalars, 'String'>,
  thdDecrement: $ElementType<Scalars, 'String'>,
|};

export type ByronTxFeePolicy = {|
  __typename?: 'ByronTxFeePolicy',
  summand: $ElementType<Scalars, 'String'>,
  multiplier: $ElementType<Scalars, 'String'>,
|};

export type Cardano = {|
  __typename?: 'Cardano',
  tip: Block,
  currentEpoch: Epoch,
|};

export type CardanoDbMeta = {|
  __typename?: 'CardanoDbMeta',
  initialized: $ElementType<Scalars, 'Boolean'>,
  syncPercentage: $ElementType<Scalars, 'Percentage'>,
|};


export type Date_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'DateTime'>,
  _gt?: ?$ElementType<Scalars, 'DateTime'>,
  _gte?: ?$ElementType<Scalars, 'DateTime'>,
  _in?: ?Array<?$ElementType<Scalars, 'DateTime'>>,
  _lt?: ?$ElementType<Scalars, 'DateTime'>,
  _lte?: ?$ElementType<Scalars, 'DateTime'>,
  _neq?: ?$ElementType<Scalars, 'DateTime'>,
  _nin?: ?Array<?$ElementType<Scalars, 'DateTime'>>,
|};

export type Delegation = {|
  __typename?: 'Delegation',
  address: $ElementType<Scalars, 'StakeAddress'>,
  stakePool: StakePool,
  stakePoolHash: $ElementType<Scalars, 'Hash28Hex'>,
  stakePoolId: $ElementType<Scalars, 'StakePoolID'>,
  transaction?: ?Transaction,
|};

export type Delegation_Aggregate = {|
  __typename?: 'Delegation_aggregate',
  aggregate?: ?Delegation_Aggregate_Fields,
|};

export type Delegation_Aggregate_Fields = {|
  __typename?: 'Delegation_aggregate_fields',
  count?: ?$ElementType<Scalars, 'String'>,
|};

export type Delegation_Bool_Exp = {|
  _and?: ?Array<?Delegation_Bool_Exp>,
  _not?: ?Delegation_Bool_Exp,
  _or?: ?Array<?Delegation_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  stakePool?: ?StakePool_Bool_Exp,
  stakePoolHash?: ?Hash28Hex_Comparison_Exp,
  stakePoolId?: ?StakePoolId_Comparison_Exp,
  transaction?: ?Transaction_Bool_Exp,
|};

export type Delegation_Order_By = {|
  address?: ?Order_By,
  stakePool?: ?StakePool_Order_By,
  stakePoolHash?: ?Order_By,
  stakePoolId?: ?Order_By,
  transaction?: ?Transaction_Order_By,
|};

export type Epoch = {|
  __typename?: 'Epoch',
  activeStake?: ?Array<?ActiveStake>,
  activeStake_aggregate?: ?ActiveStake_Aggregate,
  adaPots?: ?AdaPots,
  blocks: Array<Block>,
  blocks_aggregate: Block_Aggregate,
  blocksCount: $ElementType<Scalars, 'String'>,
  fees: $ElementType<Scalars, 'BigInt'>,
  output: $ElementType<Scalars, 'String'>,
  nonce?: ?$ElementType<Scalars, 'Hash32Hex'>,
  number: $ElementType<Scalars, 'Int'>,
  protocolParams?: ?ShelleyProtocolParams,
  transactionsCount: $ElementType<Scalars, 'String'>,
  startedAt: $ElementType<Scalars, 'DateTime'>,
  lastBlockTime: $ElementType<Scalars, 'DateTime'>,
|};


export type EpochBlocksArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};


export type EpochBlocks_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};

export type Epoch_Aggregate = {|
  __typename?: 'Epoch_aggregate',
  aggregate: Epoch_Aggregate_Fields,
|};

export type Epoch_Aggregate_Fields = {|
  __typename?: 'Epoch_aggregate_fields',
  ave: Epoch_Ave_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: Epoch_Max_Fields,
  min: Epoch_Min_Fields,
  sum: Epoch_Sum_Fields,
|};

export type Epoch_Ave_Fields = {|
  __typename?: 'Epoch_ave_fields',
  fees: $ElementType<Scalars, 'Float'>,
  output: $ElementType<Scalars, 'Float'>,
  transactionsCount: $ElementType<Scalars, 'Float'>,
|};

export type Epoch_Bool_Exp = {|
  _and?: ?Array<?Epoch_Bool_Exp>,
  _not?: ?Epoch_Bool_Exp,
  _or?: ?Array<?Epoch_Bool_Exp>,
  blocks?: ?Block_Bool_Exp,
  blocksCount?: ?Text_Comparison_Exp,
  fees?: ?BigInt_Comparison_Exp,
  number?: ?Int_Comparison_Exp,
  output?: ?Text_Comparison_Exp,
  transactionsCount?: ?Text_Comparison_Exp,
|};

export type Epoch_Max_Fields = {|
  __typename?: 'Epoch_max_fields',
  blocksCount: $ElementType<Scalars, 'String'>,
  fees: $ElementType<Scalars, 'String'>,
  number: $ElementType<Scalars, 'Int'>,
  output: $ElementType<Scalars, 'String'>,
  transactionsCount: $ElementType<Scalars, 'String'>,
|};

export type Epoch_Min_Fields = {|
  __typename?: 'Epoch_min_fields',
  blocksCount: $ElementType<Scalars, 'String'>,
  fees: $ElementType<Scalars, 'String'>,
  output: $ElementType<Scalars, 'String'>,
  transactionsCount: $ElementType<Scalars, 'String'>,
|};

export type Epoch_Order_By = {|
  blocksCount?: ?Order_By,
  fees?: ?Order_By,
  number?: ?Order_By,
  output?: ?Order_By,
  transactionsCount?: ?Order_By,
|};

export type Epoch_Sum_Fields = {|
  __typename?: 'Epoch_sum_fields',
  blocksCount: $ElementType<Scalars, 'String'>,
  fees: $ElementType<Scalars, 'String'>,
  output: $ElementType<Scalars, 'String'>,
  transactionsCount: $ElementType<Scalars, 'String'>,
|};

export type Float_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'Float'>,
  _gt?: ?$ElementType<Scalars, 'Float'>,
  _gte?: ?$ElementType<Scalars, 'Float'>,
  _in?: ?Array<$ElementType<Scalars, 'Float'>>,
  _is_null?: ?$ElementType<Scalars, 'Boolean'>,
  _lt?: ?$ElementType<Scalars, 'Float'>,
  _lte?: ?$ElementType<Scalars, 'Float'>,
  _neq?: ?$ElementType<Scalars, 'Float'>,
  _nin?: ?Array<$ElementType<Scalars, 'Float'>>,
|};

export type Genesis = {|
  __typename?: 'Genesis',
  byron?: ?ByronGenesis,
  shelley?: ?ShelleyGenesis,
|};


export type Hash28Hex_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'Hash28Hex'>,
  _in?: ?Array<?$ElementType<Scalars, 'Hash28Hex'>>,
  _neq?: ?$ElementType<Scalars, 'Hash28Hex'>,
  _nin?: ?Array<?$ElementType<Scalars, 'Hash28Hex'>>,
|};


export type Hash32Hex_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'Hash32Hex'>,
  _in?: ?Array<?$ElementType<Scalars, 'Hash32Hex'>>,
  _neq?: ?$ElementType<Scalars, 'Hash32Hex'>,
  _nin?: ?Array<?$ElementType<Scalars, 'Hash32Hex'>>,
|};



export type Int_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'Int'>,
  _gt?: ?$ElementType<Scalars, 'Int'>,
  _gte?: ?$ElementType<Scalars, 'Int'>,
  _in?: ?Array<$ElementType<Scalars, 'Int'>>,
  _is_null?: ?$ElementType<Scalars, 'Boolean'>,
  _lt?: ?$ElementType<Scalars, 'Int'>,
  _lte?: ?$ElementType<Scalars, 'Int'>,
  _neq?: ?$ElementType<Scalars, 'Int'>,
  _nin?: ?Array<$ElementType<Scalars, 'Int'>>,
|};



export type Mutation = {|
  __typename?: 'Mutation',
  submitTransaction: TransactionSubmitResponse,
|};


export type MutationSubmitTransactionArgs = {|
  transaction: $ElementType<Scalars, 'String'>,
|};

export type PaymentAddress = {|
  __typename?: 'PaymentAddress',
  address: $ElementType<Scalars, 'String'>,
  summary?: ?PaymentAddressSummary,
|};


export type PaymentAddressSummaryArgs = {|
  atBlock?: ?$ElementType<Scalars, 'Int'>,
|};

export type PaymentAddressSummary = {|
  __typename?: 'PaymentAddressSummary',
  assetBalances: Array<?AssetBalance>,
  utxosCount: $ElementType<Scalars, 'Int'>,
|};


export type Percentage_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'Percentage'>,
  _gt?: ?$ElementType<Scalars, 'Percentage'>,
  _gte?: ?$ElementType<Scalars, 'Percentage'>,
  _lt?: ?$ElementType<Scalars, 'Percentage'>,
  _lte?: ?$ElementType<Scalars, 'Percentage'>,
  _neq?: ?$ElementType<Scalars, 'Percentage'>,
|};

export type Query = {|
  __typename?: 'Query',
  activeStake: Array<?ActiveStake>,
  activeStake_aggregate: ActiveStake_Aggregate,
  ada: Ada,
  assets: Array<?Asset>,
  assets_aggregate: Asset_Aggregate,
  blocks: Array<?Block>,
  blocks_aggregate: Block_Aggregate,
  cardano: Cardano,
  cardanoDbMeta: CardanoDbMeta,
  delegations?: ?Array<?Delegation>,
  delegations_aggregate?: ?Delegation_Aggregate,
  epochs: Array<?Epoch>,
  epochs_aggregate: Epoch_Aggregate,
  genesis: Genesis,
  paymentAddresses?: ?Array<?PaymentAddress>,
  rewards: Array<?Reward>,
  rewards_aggregate: Reward_Aggregate,
  stakeDeregistrations?: ?Array<?StakeDeregistration>,
  stakeDeregistrations_aggregate?: ?StakeDeregistration_Aggregate,
  stakePools?: ?Array<?StakePool>,
  stakePools_aggregate?: ?StakePool_Aggregate,
  stakeRegistrations?: ?Array<?StakeRegistration>,
  stakeRegistrations_aggregate?: ?StakeRegistration_Aggregate,
  transactions: Array<?Transaction>,
  transactions_aggregate: Transaction_Aggregate,
  utxos: Array<?TransactionOutput>,
  utxos_aggregate: TransactionOutput_Aggregate,
  withdrawals: Array<?Withdrawal>,
  withdrawals_aggregate?: ?Withdrawal_Aggregate,
|};


export type QueryActiveStakeArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<ActiveStake_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?ActiveStake_Bool_Exp,
|};


export type QueryActiveStake_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<ActiveStake_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?ActiveStake_Bool_Exp,
|};


export type QueryAssetsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Asset_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Asset_Bool_Exp,
|};


export type QueryAssets_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Asset_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Asset_Bool_Exp,
|};


export type QueryBlocksArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};


export type QueryBlocks_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};


export type QueryDelegationsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Delegation_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Delegation_Bool_Exp,
|};


export type QueryDelegations_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Delegation_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Delegation_Bool_Exp,
|};


export type QueryEpochsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Epoch_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Epoch_Bool_Exp,
|};


export type QueryEpochs_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Epoch_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Epoch_Bool_Exp,
|};


export type QueryPaymentAddressesArgs = {|
  addresses: Array<?$ElementType<Scalars, 'String'>>,
|};


export type QueryRewardsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Reward_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Reward_Bool_Exp,
|};


export type QueryRewards_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Reward_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Reward_Bool_Exp,
|};


export type QueryStakeDeregistrationsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakeDeregistration_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakeDeregistration_Bool_Exp,
|};


export type QueryStakeDeregistrations_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakeDeregistration_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakeDeregistration_Bool_Exp,
|};


export type QueryStakePoolsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakePool_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakePool_Bool_Exp,
|};


export type QueryStakePools_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakePool_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakePool_Bool_Exp,
|};


export type QueryStakeRegistrationsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakeRegistration_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakeRegistration_Bool_Exp,
|};


export type QueryStakeRegistrations_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<StakeRegistration_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?StakeRegistration_Bool_Exp,
|};


export type QueryTransactionsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Transaction_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Transaction_Bool_Exp,
|};


export type QueryTransactions_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Transaction_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Transaction_Bool_Exp,
|};


export type QueryUtxosArgs = {|
  distinct_on?: ?Array<TransactionOutput_Distinct_On>,
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<TransactionOutput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionOutput_Bool_Exp,
|};


export type QueryUtxos_AggregateArgs = {|
  distinct_on?: ?Array<TransactionOutput_Distinct_On>,
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<TransactionOutput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionOutput_Bool_Exp,
|};


export type QueryWithdrawalsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Withdrawal_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Withdrawal_Bool_Exp,
|};


export type QueryWithdrawals_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Withdrawal_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Withdrawal_Bool_Exp,
|};

export type Relay = {|
  __typename?: 'Relay',
  ipv4?: ?$ElementType<Scalars, 'IPv4'>,
  ipv6?: ?$ElementType<Scalars, 'IPv6'>,
  dnsName?: ?$ElementType<Scalars, 'String'>,
  dnsSrvName?: ?$ElementType<Scalars, 'String'>,
  port?: ?$ElementType<Scalars, 'Int'>,
|};

export type Relay_Bool_Exp = {|
  _and?: ?Array<?Relay_Bool_Exp>,
  _not?: ?Relay_Bool_Exp,
  _or?: ?Array<?Relay_Bool_Exp>,
  ipv4?: ?Text_Comparison_Exp,
  ipv6?: ?Text_Comparison_Exp,
  dnsName?: ?Text_Comparison_Exp,
  dnsSrvName?: ?Text_Comparison_Exp,
  port?: ?Int_Comparison_Exp,
|};

export type Reward = {|
  __typename?: 'Reward',
  address: $ElementType<Scalars, 'StakeAddress'>,
  amount: $ElementType<Scalars, 'String'>,
  earnedIn: Epoch,
  stakePool: StakePool,
|};

export type Reward_Aggregate = {|
  __typename?: 'Reward_aggregate',
  aggregate?: ?Reward_Aggregate_Fields,
|};

export type Reward_Aggregate_Fields = {|
  __typename?: 'Reward_aggregate_fields',
  avg: Reward_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: Reward_Max_Fields,
  min: Reward_Min_Fields,
  sum: Reward_Sum_Fields,
|};

export type Reward_Avg_Fields = {|
  __typename?: 'Reward_avg_fields',
  amount?: ?$ElementType<Scalars, 'Float'>,
|};

export type Reward_Bool_Exp = {|
  _and?: ?Array<?Reward_Bool_Exp>,
  _not?: ?Reward_Bool_Exp,
  _or?: ?Array<?Reward_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  amount?: ?Text_Comparison_Exp,
  earnedIn?: ?Epoch_Bool_Exp,
  stakePool?: ?StakePool_Bool_Exp,
|};

export type Reward_Max_Fields = {|
  __typename?: 'Reward_max_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Reward_Min_Fields = {|
  __typename?: 'Reward_min_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Reward_Order_By = {|
  address?: ?Order_By,
  amount?: ?Order_By,
  earnedIn?: ?Epoch_Order_By,
  stakePool?: ?StakePool_Order_By,
|};

export type Reward_Sum_Fields = {|
  __typename?: 'Reward_sum_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type ShelleyGenesis = {|
  __typename?: 'ShelleyGenesis',
  activeSlotsCoeff: $ElementType<Scalars, 'Float'>,
  epochLength: $ElementType<Scalars, 'Int'>,
  genDelegs?: ?$ElementType<Scalars, 'JSONObject'>,
  initialFunds: $ElementType<Scalars, 'JSONObject'>,
  maxKESEvolutions: $ElementType<Scalars, 'Int'>,
  maxLovelaceSupply: $ElementType<Scalars, 'String'>,
  networkId: $ElementType<Scalars, 'String'>,
  networkMagic: $ElementType<Scalars, 'Int'>,
  protocolParams: ShelleyProtocolParams,
  securityParam: $ElementType<Scalars, 'Int'>,
  slotLength: $ElementType<Scalars, 'Int'>,
  slotsPerKESPeriod: $ElementType<Scalars, 'Int'>,
  staking?: ?ShelleyGenesisStaking,
  systemStart: $ElementType<Scalars, 'String'>,
  updateQuorum: $ElementType<Scalars, 'Int'>,
|};

export type ShelleyGenesisStaking = {|
  __typename?: 'ShelleyGenesisStaking',
  pools: $ElementType<Scalars, 'JSONObject'>,
  stake: $ElementType<Scalars, 'JSONObject'>,
|};

export type ShelleyProtocolParams = {|
  __typename?: 'ShelleyProtocolParams',
  a0: $ElementType<Scalars, 'Float'>,
  decentralisationParam: $ElementType<Scalars, 'Float'>,
  eMax: $ElementType<Scalars, 'Int'>,
  extraEntropy?: ?$ElementType<Scalars, 'JSONObject'>,
  keyDeposit: $ElementType<Scalars, 'Int'>,
  maxBlockBodySize: $ElementType<Scalars, 'Int'>,
  maxBlockHeaderSize: $ElementType<Scalars, 'Int'>,
  maxTxSize: $ElementType<Scalars, 'Int'>,
  minFeeA: $ElementType<Scalars, 'Int'>,
  minFeeB: $ElementType<Scalars, 'Int'>,
  minPoolCost: $ElementType<Scalars, 'Int'>,
  minUTxOValue: $ElementType<Scalars, 'Int'>,
  nOpt: $ElementType<Scalars, 'Int'>,
  poolDeposit: $ElementType<Scalars, 'Int'>,
  protocolVersion: $ElementType<Scalars, 'JSONObject'>,
  rho: $ElementType<Scalars, 'Float'>,
  tau: $ElementType<Scalars, 'Float'>,
|};

export type SlotLeader = {|
  __typename?: 'SlotLeader',
  description: $ElementType<Scalars, 'String'>,
  hash: $ElementType<Scalars, 'Hash28Hex'>,
  stakePool?: ?StakePool,
|};

export type SlotLeader_Bool_Exp = {|
  _and?: ?Array<?SlotLeader_Bool_Exp>,
  _not?: ?SlotLeader_Bool_Exp,
  _or?: ?Array<?SlotLeader_Bool_Exp>,
  description?: ?Text_Comparison_Exp,
  hash?: ?Hash28Hex_Comparison_Exp,
  stakePool?: ?StakePool_Bool_Exp,
|};

export type SlotLeader_Order_By = {|
  hash?: ?Order_By,
  stakePool?: ?StakePool_Order_By,
|};


export type StakeAddress_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'StakeAddress'>,
  _in?: ?Array<?$ElementType<Scalars, 'StakeAddress'>>,
  _neq?: ?$ElementType<Scalars, 'StakeAddress'>,
  _nin?: ?Array<?$ElementType<Scalars, 'StakeAddress'>>,
|};

export type StakeDeregistration = {|
  __typename?: 'StakeDeregistration',
  address: $ElementType<Scalars, 'StakeAddress'>,
  transaction: Transaction,
|};

export type StakeDeregistration_Aggregate = {|
  __typename?: 'StakeDeregistration_aggregate',
  aggregate?: ?StakeDeregistration_Aggregate_Fields,
|};

export type StakeDeregistration_Aggregate_Fields = {|
  __typename?: 'StakeDeregistration_aggregate_fields',
  count?: ?$ElementType<Scalars, 'String'>,
|};

export type StakeDeregistration_Bool_Exp = {|
  _and?: ?Array<?StakeDeregistration_Bool_Exp>,
  _not?: ?StakeDeregistration_Bool_Exp,
  _or?: ?Array<?StakeDeregistration_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  transaction?: ?Transaction_Bool_Exp,
|};

export type StakeDeregistration_Order_By = {|
  address?: ?Order_By,
  transaction?: ?Transaction_Order_By,
|};

export type StakePool = {|
  __typename?: 'StakePool',
  activeStake: Array<?ActiveStake>,
  activeStake_aggregate: ActiveStake_Aggregate,
  blocks: Array<Block>,
  blocks_aggregate: Block_Aggregate,
  delegators: Array<?Delegation>,
  delegators_aggregate: Delegation_Aggregate,
  fixedCost: $ElementType<Scalars, 'String'>,
  hash: $ElementType<Scalars, 'Hash28Hex'>,
  id: $ElementType<Scalars, 'StakePoolID'>,
  margin: $ElementType<Scalars, 'Float'>,
  metadataHash?: ?$ElementType<Scalars, 'Hash32Hex'>,
  owners: Array<StakePoolOwner>,
  pledge: $ElementType<Scalars, 'String'>,
  relays?: ?Array<?Relay>,
  retirements?: ?Array<?StakePoolRetirement>,
  rewardAddress: $ElementType<Scalars, 'StakeAddress'>,
  rewards: Array<?Reward>,
  rewards_aggregate: Reward_Aggregate,
  updatedIn: Transaction,
  url?: ?$ElementType<Scalars, 'String'>,
|};


export type StakePoolActiveStakeArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<ActiveStake_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?ActiveStake_Bool_Exp,
|};


export type StakePoolActiveStake_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<ActiveStake_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?ActiveStake_Bool_Exp,
|};


export type StakePoolBlocksArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};


export type StakePoolBlocks_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Block_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Block_Bool_Exp,
|};


export type StakePoolDelegatorsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Delegation_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Delegation_Bool_Exp,
|};


export type StakePoolDelegators_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Delegation_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Delegation_Bool_Exp,
|};


export type StakePoolRewards_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Reward_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Reward_Bool_Exp,
|};


export type StakePoolId_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'StakePoolID'>,
  _in?: ?Array<?$ElementType<Scalars, 'StakePoolID'>>,
  _neq?: ?$ElementType<Scalars, 'StakePoolID'>,
  _nin?: ?Array<?$ElementType<Scalars, 'StakePoolID'>>,
|};

export type StakePoolOwner = {|
  __typename?: 'StakePoolOwner',
  hash: $ElementType<Scalars, 'Hash28Hex'>,
|};

export type StakePoolOwner_Bool_Exp = {|
  _and?: ?Array<?StakePoolOwner_Bool_Exp>,
  _not?: ?StakePoolOwner_Bool_Exp,
  _or?: ?Array<?StakePoolOwner_Bool_Exp>,
  hash?: ?Hash28Hex_Comparison_Exp,
|};

export type StakePoolRetirement = {|
  __typename?: 'StakePoolRetirement',
  announcedIn?: ?Transaction,
  inEffectFrom: $ElementType<Scalars, 'Int'>,
  retiredInEpoch?: ?Epoch,
|};

export type StakePoolRetirement_Bool_Exp = {|
  _and?: ?Array<?StakePoolRetirement_Bool_Exp>,
  _not?: ?StakePoolRetirement_Bool_Exp,
  _or?: ?Array<?StakePoolRetirement_Bool_Exp>,
  announcedIn?: ?Transaction_Bool_Exp,
  inEffectFrom?: ?Int_Comparison_Exp,
|};

export type StakePool_Aggregate = {|
  __typename?: 'StakePool_aggregate',
  aggregate?: ?StakePool_Aggregate_Fields,
|};

export type StakePool_Aggregate_Fields = {|
  __typename?: 'StakePool_aggregate_fields',
  avg: StakePool_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: StakePool_Max_Fields,
  min: StakePool_Min_Fields,
  sum: StakePool_Sum_Fields,
|};

export type StakePool_Avg_Fields = {|
  __typename?: 'StakePool_avg_fields',
  fixedCost?: ?$ElementType<Scalars, 'String'>,
  margin?: ?$ElementType<Scalars, 'Float'>,
  pledge?: ?$ElementType<Scalars, 'String'>,
|};

export type StakePool_Bool_Exp = {|
  _and?: ?Array<?StakePool_Bool_Exp>,
  _not?: ?StakePool_Bool_Exp,
  _or?: ?Array<?StakePool_Bool_Exp>,
  hash?: ?Hash28Hex_Comparison_Exp,
  id?: ?StakePoolId_Comparison_Exp,
  margin?: ?Float_Comparison_Exp,
  metadataHash?: ?Hash32Hex_Comparison_Exp,
  owners?: ?StakePoolOwner_Bool_Exp,
  pledge?: ?Text_Comparison_Exp,
  registrationTransaction?: ?Transaction_Bool_Exp,
  relays?: ?Relay_Bool_Exp,
  retirements?: ?StakePoolRetirement_Bool_Exp,
  rewardAddress?: ?Text_Comparison_Exp,
  rewards?: ?Reward_Bool_Exp,
  url?: ?Text_Comparison_Exp,
|};

export type StakePool_Max_Fields = {|
  __typename?: 'StakePool_max_fields',
  fixedCost?: ?$ElementType<Scalars, 'String'>,
  margin?: ?$ElementType<Scalars, 'Float'>,
  pledge?: ?$ElementType<Scalars, 'String'>,
|};

export type StakePool_Min_Fields = {|
  __typename?: 'StakePool_min_fields',
  fixedCost?: ?$ElementType<Scalars, 'String'>,
  margin?: ?$ElementType<Scalars, 'Float'>,
  pledge?: ?$ElementType<Scalars, 'String'>,
|};

export type StakePool_Order_By = {|
  fixedCost?: ?Order_By,
  hash?: ?Order_By,
  id?: ?Order_By,
  margin?: ?Order_By,
  pledge?: ?Order_By,
  updatedIn?: ?Transaction_Order_By,
  url?: ?Order_By,
|};

export type StakePool_Sum_Fields = {|
  __typename?: 'StakePool_sum_fields',
  fixedCost?: ?$ElementType<Scalars, 'String'>,
  margin?: ?$ElementType<Scalars, 'Float'>,
  pledge?: ?$ElementType<Scalars, 'String'>,
|};

export type StakeRegistration = {|
  __typename?: 'StakeRegistration',
  address: $ElementType<Scalars, 'StakeAddress'>,
  transaction: Transaction,
|};

export type StakeRegistration_Aggregate = {|
  __typename?: 'StakeRegistration_aggregate',
  aggregate?: ?StakeRegistration_Aggregate_Fields,
|};

export type StakeRegistration_Aggregate_Fields = {|
  __typename?: 'StakeRegistration_aggregate_fields',
  count?: ?$ElementType<Scalars, 'String'>,
|};

export type StakeRegistration_Bool_Exp = {|
  _and?: ?Array<?StakeRegistration_Bool_Exp>,
  _not?: ?StakeRegistration_Bool_Exp,
  _or?: ?Array<?StakeRegistration_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  transaction?: ?Transaction_Bool_Exp,
|};

export type StakeRegistration_Order_By = {|
  address?: ?Order_By,
  transaction?: ?Transaction_Order_By,
|};


export type Token = {|
  __typename?: 'Token',
  asset: Asset,
  quantity: $ElementType<Scalars, 'String'>,
  transactionOutput: TransactionOutput,
|};

export type TokenMint = {|
  __typename?: 'TokenMint',
  asset: Asset,
  quantity: $ElementType<Scalars, 'String'>,
  transaction: Transaction,
|};

export type TokenMint_Aggregate = {|
  __typename?: 'TokenMint_aggregate',
  aggregate?: ?TokenMint_Aggregate_Fields,
  nodes: Array<TokenMint>,
|};

export type TokenMint_Aggregate_Fields = {|
  __typename?: 'TokenMint_aggregate_fields',
  avg: TokenMint_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: TokenMint_Max_Fields,
  min: TokenMint_Min_Fields,
  sum: TokenMint_Sum_Fields,
|};

export type TokenMint_Avg_Fields = {|
  __typename?: 'TokenMint_avg_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type TokenMint_Bool_Exp = {|
  _and?: ?Array<?TokenMint_Bool_Exp>,
  _not?: ?TokenMint_Bool_Exp,
  _or?: ?Array<?TokenMint_Bool_Exp>,
  asset?: ?Asset_Bool_Exp,
  quantity?: ?Text_Comparison_Exp,
  transaction?: ?Transaction_Bool_Exp,
|};

export type TokenMint_Max_Fields = {|
  __typename?: 'TokenMint_max_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type TokenMint_Min_Fields = {|
  __typename?: 'TokenMint_min_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type TokenMint_Order_By = {|
  asset?: ?Asset_Order_By,
  quantity?: ?Order_By,
  transaction?: ?Transaction_Order_By,
|};

export type TokenMint_Sum_Fields = {|
  __typename?: 'TokenMint_sum_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type Token_Aggregate = {|
  __typename?: 'Token_aggregate',
  aggregate?: ?Token_Aggregate_Fields,
  nodes: Array<Token>,
|};

export type Token_Aggregate_Fields = {|
  __typename?: 'Token_aggregate_fields',
  avg: Token_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: Token_Max_Fields,
  min: Token_Min_Fields,
  sum: Token_Sum_Fields,
|};

export type Token_Avg_Fields = {|
  __typename?: 'Token_avg_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type Token_Bool_Exp = {|
  _and?: ?Array<?Token_Bool_Exp>,
  _not?: ?Token_Bool_Exp,
  _or?: ?Array<?Token_Bool_Exp>,
  asset?: ?Asset_Bool_Exp,
  quantity?: ?Text_Comparison_Exp,
  transactionOutput?: ?TransactionOutput_Bool_Exp,
|};

export type Token_Max_Fields = {|
  __typename?: 'Token_max_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type Token_Min_Fields = {|
  __typename?: 'Token_min_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type Token_Order_By = {|
  asset?: ?Asset_Order_By,
  quantity?: ?Order_By,
|};

export type Token_Sum_Fields = {|
  __typename?: 'Token_sum_fields',
  quantity?: ?$ElementType<Scalars, 'String'>,
|};

export type Transaction = {|
  __typename?: 'Transaction',
  block?: ?Block,
  blockIndex: $ElementType<Scalars, 'Int'>,
  deposit: $ElementType<Scalars, 'BigInt'>,
  fee: $ElementType<Scalars, 'BigInt'>,
  hash: $ElementType<Scalars, 'Hash32Hex'>,
  inputs: Array<TransactionInput>,
  inputs_aggregate: TransactionInput_Aggregate,
  invalidBefore?: ?$ElementType<Scalars, 'String'>,
  invalidHereafter?: ?$ElementType<Scalars, 'String'>,
  metadata?: ?Array<?TransactionMetadata>,
  mint: Array<Token>,
  mint_aggregate: Token_Aggregate,
  outputs: Array<?TransactionOutput>,
  outputs_aggregate: TransactionOutput_Aggregate,
  size: $ElementType<Scalars, 'BigInt'>,
  totalOutput: $ElementType<Scalars, 'String'>,
  includedAt: $ElementType<Scalars, 'DateTime'>,
  withdrawals: Array<?Withdrawal>,
  withdrawals_aggregate: Withdrawal_Aggregate,
|};


export type TransactionInputsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?TransactionInput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionInput_Bool_Exp,
|};


export type TransactionInputs_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?TransactionInput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionInput_Bool_Exp,
|};


export type TransactionMintArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?Token_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Token_Bool_Exp,
|};


export type TransactionMint_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?Token_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Token_Bool_Exp,
|};


export type TransactionOutputsArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?TransactionOutput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionOutput_Bool_Exp,
|};


export type TransactionOutputs_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?TransactionOutput_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?TransactionOutput_Bool_Exp,
|};


export type TransactionWithdrawals_AggregateArgs = {|
  limit?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<?Withdrawal_Order_By>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  where?: ?Withdrawal_Bool_Exp,
|};

export type TransactionInput = {|
  __typename?: 'TransactionInput',
  address: $ElementType<Scalars, 'String'>,
  sourceTransaction: Transaction,
  sourceTxHash: $ElementType<Scalars, 'Hash32Hex'>,
  sourceTxIndex: $ElementType<Scalars, 'Int'>,
  tokens: Array<Token>,
  tokens_aggregate: Token_Aggregate,
  transaction: Transaction,
  txHash: $ElementType<Scalars, 'Hash32Hex'>,
  value: $ElementType<Scalars, 'String'>,
|};

export type TransactionInput_Aggregate = {|
  __typename?: 'TransactionInput_aggregate',
  aggregate?: ?TransactionInput_Aggregate_Fields,
|};

export type TransactionInput_Aggregate_Fields = {|
  __typename?: 'TransactionInput_aggregate_fields',
  avg: TransactionInput_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: TransactionInput_Max_Fields,
  min: TransactionInput_Min_Fields,
  sum: TransactionInput_Sum_Fields,
|};

export type TransactionInput_Avg_Fields = {|
  __typename?: 'TransactionInput_avg_fields',
  tokens?: ?Token_Avg_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionInput_Bool_Exp = {|
  _and?: ?Array<?TransactionInput_Bool_Exp>,
  _not?: ?TransactionInput_Bool_Exp,
  _or?: ?Array<?TransactionInput_Bool_Exp>,
  address?: ?Text_Comparison_Exp,
  sourceTransaction?: ?Transaction_Bool_Exp,
  tokens?: ?Token_Bool_Exp,
  transaction?: ?Transaction_Bool_Exp,
  value?: ?Text_Comparison_Exp,
|};

export type TransactionInput_Max_Fields = {|
  __typename?: 'TransactionInput_max_fields',
  tokens?: ?Token_Max_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionInput_Min_Fields = {|
  __typename?: 'TransactionInput_min_fields',
  tokens?: ?Token_Min_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionInput_Order_By = {|
  address?: ?Order_By,
  sourceTxHash?: ?Order_By,
  txHash?: ?Order_By,
  value?: ?Order_By,
|};

export type TransactionInput_Sum_Fields = {|
  __typename?: 'TransactionInput_sum_fields',
  tokens?: ?Token_Sum_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionMetadata = {|
  __typename?: 'TransactionMetadata',
  key: $ElementType<Scalars, 'String'>,
  value: $ElementType<Scalars, 'JSON'>,
|};

export type TransactionMetadata_Bool_Exp = {|
  key?: ?Text_Comparison_Exp,
|};

export type TransactionOutput = {|
  __typename?: 'TransactionOutput',
  address: $ElementType<Scalars, 'String'>,
  index: $ElementType<Scalars, 'Int'>,
  transaction: Transaction,
  txHash: $ElementType<Scalars, 'Hash32Hex'>,
  tokens: Array<Token>,
  tokens_aggregate: Token_Aggregate,
  value: $ElementType<Scalars, 'String'>,
|};

export type TransactionOutput_Aggregate = {|
  __typename?: 'TransactionOutput_aggregate',
  aggregate?: ?TransactionOutput_Aggregate_Fields,
|};

export type TransactionOutput_Aggregate_Fields = {|
  __typename?: 'TransactionOutput_aggregate_fields',
  avg: TransactionOutput_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: TransactionOutput_Max_Fields,
  min: TransactionOutput_Min_Fields,
  sum: TransactionOutput_Sum_Fields,
|};

export type TransactionOutput_Avg_Fields = {|
  __typename?: 'TransactionOutput_avg_fields',
  tokens?: ?Token_Avg_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionOutput_Bool_Exp = {|
  _and?: ?Array<?TransactionOutput_Bool_Exp>,
  _not?: ?TransactionOutput_Bool_Exp,
  _or?: ?Array<?TransactionOutput_Bool_Exp>,
  address?: ?Text_Comparison_Exp,
  tokens?: ?Token_Bool_Exp,
  transaction?: ?Transaction_Bool_Exp,
  value?: ?Text_Comparison_Exp,
|};

export const TransactionOutput_Distinct_OnValues = Object.freeze({
  Address: 'address'
});


export type TransactionOutput_Distinct_On = $Values<typeof TransactionOutput_Distinct_OnValues>;

export type TransactionOutput_Max_Fields = {|
  __typename?: 'TransactionOutput_max_fields',
  tokens?: ?Token_Max_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionOutput_Min_Fields = {|
  __typename?: 'TransactionOutput_min_fields',
  tokens?: ?Token_Min_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionOutput_Order_By = {|
  address?: ?Order_By,
  index?: ?Order_By,
  txHash?: ?Order_By,
  value?: ?Order_By,
|};

export type TransactionOutput_Sum_Fields = {|
  __typename?: 'TransactionOutput_sum_fields',
  tokens?: ?Token_Sum_Fields,
  value?: ?$ElementType<Scalars, 'String'>,
|};

export type TransactionSubmitResponse = {|
  __typename?: 'TransactionSubmitResponse',
  hash: $ElementType<Scalars, 'String'>,
|};

export type Transaction_Aggregate = {|
  __typename?: 'Transaction_aggregate',
  aggregate?: ?Transaction_Aggregate_Fields,
|};

export type Transaction_Aggregate_Fields = {|
  __typename?: 'Transaction_aggregate_fields',
  avg: Transaction_Avg_Fields,
  count: $ElementType<Scalars, 'String'>,
  max: Transaction_Max_Fields,
  min: Transaction_Min_Fields,
  sum: Transaction_Sum_Fields,
|};

export type Transaction_Avg_Fields = {|
  __typename?: 'Transaction_avg_fields',
  deposit?: ?$ElementType<Scalars, 'Float'>,
  fee?: ?$ElementType<Scalars, 'Float'>,
  mint?: ?Token_Avg_Fields,
  size?: ?$ElementType<Scalars, 'Float'>,
  totalOutput?: ?$ElementType<Scalars, 'Float'>,
  withdrawals?: ?Withdrawal_Ave_Fields,
|};

export type Transaction_Bool_Exp = {|
  _and?: ?Array<?Transaction_Bool_Exp>,
  _not?: ?Transaction_Bool_Exp,
  _or?: ?Array<?Transaction_Bool_Exp>,
  block?: ?Block_Bool_Exp,
  blockIndex?: ?Int_Comparison_Exp,
  deposit?: ?BigInt_Comparison_Exp,
  fee?: ?BigInt_Comparison_Exp,
  hash?: ?Hash32Hex_Comparison_Exp,
  includedAt?: ?Date_Comparison_Exp,
  inputs?: ?TransactionInput_Bool_Exp,
  invalidBefore?: ?Text_Comparison_Exp,
  invalidHereafter?: ?Text_Comparison_Exp,
  metadata?: ?TransactionMetadata_Bool_Exp,
  mint?: ?Token_Bool_Exp,
  outputs?: ?TransactionOutput_Bool_Exp,
  size?: ?BigInt_Comparison_Exp,
  totalOutput?: ?Text_Comparison_Exp,
  withdrawals?: ?Withdrawal_Bool_Exp,
|};

export type Transaction_Max_Fields = {|
  __typename?: 'Transaction_max_fields',
  deposit?: ?$ElementType<Scalars, 'String'>,
  fee?: ?$ElementType<Scalars, 'String'>,
  invalidBefore?: ?$ElementType<Scalars, 'String'>,
  invalidHereafter?: ?$ElementType<Scalars, 'String'>,
  mint?: ?Token_Max_Fields,
  size?: ?$ElementType<Scalars, 'String'>,
  totalOutput?: ?$ElementType<Scalars, 'String'>,
  withdrawals?: ?Withdrawal_Max_Fields,
|};

export type Transaction_Min_Fields = {|
  __typename?: 'Transaction_min_fields',
  deposit?: ?$ElementType<Scalars, 'String'>,
  fee?: ?$ElementType<Scalars, 'String'>,
  invalidBefore?: ?$ElementType<Scalars, 'String'>,
  invalidHereafter?: ?$ElementType<Scalars, 'String'>,
  mint?: ?Token_Min_Fields,
  size?: ?$ElementType<Scalars, 'String'>,
  totalOutput?: ?$ElementType<Scalars, 'String'>,
  withdrawals?: ?Withdrawal_Min_Fields,
|};

export type Transaction_Order_By = {|
  block?: ?Block_Order_By,
  blockIndex?: ?Order_By,
  deposit?: ?Order_By,
  fee?: ?Order_By,
  hash?: ?Order_By,
  includedAt?: ?Order_By,
  invalidBefore?: ?Order_By_With_Nulls,
  invalidHereafter?: ?Order_By_With_Nulls,
  size?: ?Order_By,
  totalOutput?: ?Order_By,
  withdrawals?: ?Order_By,
|};

export type Transaction_Sum_Fields = {|
  __typename?: 'Transaction_sum_fields',
  deposit?: ?$ElementType<Scalars, 'String'>,
  fee?: ?$ElementType<Scalars, 'String'>,
  mint?: ?Token_Sum_Fields,
  size?: ?$ElementType<Scalars, 'String'>,
  totalOutput?: ?$ElementType<Scalars, 'String'>,
  withdrawals?: ?Withdrawal_Sum_Fields,
|};


export type VrfVerificationKey_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'VRFVerificationKey'>,
  _in?: ?Array<?$ElementType<Scalars, 'VRFVerificationKey'>>,
  _is_null?: ?$ElementType<Scalars, 'Boolean'>,
  _neq?: ?$ElementType<Scalars, 'VRFVerificationKey'>,
  _nin?: ?Array<?$ElementType<Scalars, 'VRFVerificationKey'>>,
|};

export type Withdrawal = {|
  __typename?: 'Withdrawal',
  address: $ElementType<Scalars, 'StakeAddress'>,
  amount: $ElementType<Scalars, 'String'>,
  transaction: Transaction,
|};

export type Withdrawal_Aggregate = {|
  __typename?: 'Withdrawal_aggregate',
  aggregate: Withdrawal_Aggregate_Fields,
|};

export type Withdrawal_Aggregate_Fields = {|
  __typename?: 'Withdrawal_aggregate_fields',
  count: $ElementType<Scalars, 'String'>,
  ave: Withdrawal_Ave_Fields,
  max: Withdrawal_Max_Fields,
  min: Withdrawal_Min_Fields,
  sum: Withdrawal_Sum_Fields,
|};

export type Withdrawal_Ave_Fields = {|
  __typename?: 'Withdrawal_ave_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Withdrawal_Bool_Exp = {|
  _and?: ?Array<?Withdrawal_Bool_Exp>,
  _not?: ?Withdrawal_Bool_Exp,
  _or?: ?Array<?Withdrawal_Bool_Exp>,
  address?: ?StakeAddress_Comparison_Exp,
  amount?: ?Text_Comparison_Exp,
  transaction?: ?Transaction_Bool_Exp,
|};

export type Withdrawal_Max_Fields = {|
  __typename?: 'Withdrawal_max_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Withdrawal_Min_Fields = {|
  __typename?: 'Withdrawal_min_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export type Withdrawal_Order_By = {|
  address?: ?Order_By,
  amount?: ?Order_By,
  transaction?: ?Transaction_Order_By,
|};

export type Withdrawal_Sum_Fields = {|
  __typename?: 'Withdrawal_sum_fields',
  amount?: ?$ElementType<Scalars, 'String'>,
|};

export const Order_ByValues = Object.freeze({
  Asc: 'asc',
  Desc: 'desc'
});


export type Order_By = $Values<typeof Order_ByValues>;

export const Order_By_With_NullsValues = Object.freeze({
  Asc: 'asc',
  AscNullsFirst: 'asc_nulls_first',
  AscNullsLast: 'asc_nulls_last',
  Desc: 'desc',
  DescNullsFirst: 'desc_nulls_first',
  DescNullsLast: 'desc_nulls_last'
});


export type Order_By_With_Nulls = $Values<typeof Order_By_With_NullsValues>;

export type Text_Comparison_Exp = {|
  _eq?: ?$ElementType<Scalars, 'String'>,
  _gt?: ?$ElementType<Scalars, 'String'>,
  _gte?: ?$ElementType<Scalars, 'String'>,
  _ilike?: ?$ElementType<Scalars, 'String'>,
  _in?: ?Array<?$ElementType<Scalars, 'String'>>,
  _is_null?: ?$ElementType<Scalars, 'Boolean'>,
  _like?: ?$ElementType<Scalars, 'String'>,
  _lt?: ?$ElementType<Scalars, 'String'>,
  _lte?: ?$ElementType<Scalars, 'String'>,
  _neq?: ?$ElementType<Scalars, 'String'>,
  _nilike?: ?$ElementType<Scalars, 'String'>,
  _nin?: ?Array<?$ElementType<Scalars, 'String'>>,
  _nlike?: ?$ElementType<Scalars, 'String'>,
  _nsimilar?: ?$ElementType<Scalars, 'String'>,
  _similar?: ?$ElementType<Scalars, 'String'>,
|};

type $Pick<Origin: Object, Keys: Object> = $ObjMapi<Keys, <Key>(k: Key) => $ElementType<Origin, Key>>;

export type GetRewardsForAddressesQueryVariables = {
  addresses: Array<$ElementType<Scalars, 'StakeAddress'>> | $ElementType<Scalars, 'StakeAddress'>,
  limit?: ?$ElementType<Scalars, 'Int'>,
  offset?: ?$ElementType<Scalars, 'Int'>,
  order_by?: ?Array<Reward_Order_By> | Reward_Order_By,
};


export type GetRewardsForAddressesQuery = ({
    ...{ __typename?: 'Query' },
  ...{| rewards: Array<?({
      ...{ __typename?: 'Reward' },
    ...$Pick<Reward, {| address: *, amount: * |}>,
    ...{| earnedIn: ({
        ...{ __typename?: 'Epoch' },
      ...$Pick<Epoch, {| number: * |}>
    }), stakePool: ({
        ...{ __typename?: 'StakePool' },
      ...$Pick<StakePool, {| id: * |}>
    }) |}
  })> |}
});
