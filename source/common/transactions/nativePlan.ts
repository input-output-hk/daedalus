import { Cardano } from '@cardano-sdk/core';
import { blake2b } from 'blakejs';

import { bytesForSpan, CborItem, parseCborItem } from '../cardano/cborSlices';
import type {
  DappNetwork,
  TransactionContextSnapshot,
} from '../cardano/transactionContext';
import type {
  TransactionReviewAsset,
  TransactionReviewDisplay,
  TransactionReviewEntry,
  TransactionReviewValue,
} from './reviewDisplay';

const MAX_WORD64 = (BigInt(1) << BigInt(64)) - BigInt(1);
const MAX_WORD32 = (BigInt(1) << BigInt(32)) - BigInt(1);
const ACTIONS = [
  'payment',
  'collateral-preparation',
  'byron-payment',
  'migration',
  'delegate',
  'redelegate',
  'undelegate',
  'drep-delegation',
  'catalyst-registration',
  'itn-redemption',
] as const;

export type NativeTransactionAction = typeof ACTIONS[number];
export type NativeTransactionAuthorization = 'software' | 'ledger' | 'trezor';
export type NativeApprovalResult =
  | Readonly<{ status: 'submitted'; transactionIds: readonly string[] }>
  | Readonly<{
      status: 'partial';
      transactionIds: readonly string[];
      failedIndex: number;
      errorCode: string;
    }>
  | Readonly<{
      status: 'submission-unknown';
      transactionIds: readonly string[];
    }>
  | Readonly<{ status: 'rejected'; errorCode: string }>;

export type NativePreparedApproval = Readonly<{
  walletId: string;
  network: DappNetwork;
  action: NativeTransactionAction;
  authorization: NativeTransactionAuthorization;
  items: readonly (
    | Readonly<{
        kind: 'exact-cbor';
        cbor: string;
        transactionContext: TransactionContextSnapshot;
        selectionFacts: Readonly<{
          deposits: string | null;
          refunds: string | null;
        }>;
      }>
    | Readonly<{
        kind: 'native-plan';
        planCbor: string;
        planDigest: string;
      }>
  )[];
  context: Readonly<{
    walletName: string;
    networkName: string;
    destinationWalletName?: string;
  }>;
}>;

type Value = Readonly<{
  coin: bigint;
  assets: readonly Readonly<{
    policyId: string;
    assetName: string;
    quantity: bigint;
  }>[];
}>;
type Input = Readonly<{
  transactionId: string;
  index: bigint;
  address: string;
  value: Value;
}>;
type Output = Readonly<{
  address: string;
  value: Value;
  sourceWalletOwned: boolean;
}>;
type Withdrawal = Readonly<{
  account: string;
  coin: bigint;
  sourceWalletOwned: boolean;
}>;
export type NativeTransactionPlanItem = Readonly<{
  inputs: readonly Input[];
  outputs: readonly Output[];
  fee: bigint;
  withdrawals: readonly Withdrawal[];
  certificatesCbor: readonly string[];
  metadataBinding:
    | null
    | Readonly<{ kind: 'exact'; metadataCbor: string }>
    | Readonly<{
        kind: 'catalyst';
        votingPublicKey: string;
        stakePublicKey: string;
        rewardAddress: string;
        nonce: bigint;
        votingPurpose: bigint | null;
      }>;
  deposits: bigint;
  refunds: bigint;
  collateralInputs: readonly Input[];
  collateralReturn: Output | null;
  maximumCollateralLoss: bigint | null;
}>;
export type NativeTransactionPlan = Readonly<{
  walletId: string;
  network: DappNetwork;
  action: NativeTransactionAction;
  protocolParametersHash: string;
  validity: Readonly<{
    invalidBefore: bigint | null;
    invalidHereafter: bigint | null;
  }>;
  items: readonly NativeTransactionPlanItem[];
  sourceLeftoverValue: Value;
}>;

const fail = (detail = 'Invalid native transaction plan'): never => {
  throw new Error(detail);
};
const parts = (item: CborItem, size?: number): readonly CborItem[] => {
  if (item.major !== 4 || item.additionalInformation === 31 || !item.items)
    return fail();
  if (size !== undefined && item.items.length !== size) return fail();
  return item.items;
};
const canonicalArgument = (item: CborItem): void => {
  if (item.additionalInformation === 31)
    fail('Indefinite CBOR is not canonical');
  const value = item.value;
  if (value === undefined) return;
  const information = item.additionalInformation;
  if (
    (value < BigInt(24) && information >= 24) ||
    (value <= BigInt(0xff) && information > 24) ||
    (value <= BigInt(0xffff) && information > 25) ||
    (value <= BigInt(0xffffffff) && information > 26)
  )
    fail('Non-canonical CBOR integer');
};
const walkCanonical = (item: CborItem): void => {
  canonicalArgument(item);
  item.items?.forEach(walkCanonical);
  item.entries?.forEach(({ key, value }) => {
    walkCanonical(key);
    walkCanonical(value);
  });
};
const integer = (item: CborItem, max = MAX_WORD64): bigint => {
  canonicalArgument(item);
  if (item.major !== 0 || item.value === undefined || item.value > max)
    return fail();
  return item.value;
};
const nullableInteger = (item: CborItem, max = MAX_WORD64): bigint | null => {
  if (item.major === 7 && item.additionalInformation === 22) return null;
  return integer(item, max);
};
const bytes = (source: Buffer, item: CborItem, length?: number): Buffer => {
  if (
    item.major !== 2 ||
    item.additionalInformation === 31 ||
    !item.content ||
    (length !== undefined && item.content.end - item.content.start !== length)
  )
    return fail();
  return bytesForSpan(source, item.content);
};
const text = (source: Buffer, item: CborItem): string => {
  if (item.major !== 3 || item.additionalInformation === 31 || !item.content)
    return fail();
  const raw = bytesForSpan(source, item.content);
  const value = new TextDecoder('utf-8', { fatal: true }).decode(raw);
  if (!Buffer.from(value, 'utf8').equals(raw)) fail('Non-canonical CBOR text');
  return value;
};
const boolean = (item: CborItem): boolean => {
  if (item.major !== 7 || ![20, 21].includes(item.additionalInformation))
    return fail();
  return item.additionalInformation === 21;
};
const hexBytes = (source: Buffer, item: CborItem, length?: number): string =>
  bytes(source, item, length).toString('hex');

const parseValue = (source: Buffer, item: CborItem): Value => {
  const [coinItem, assetItems] = parts(item, 2);
  const coin = integer(coinItem);
  const seen = new Set<string>();
  let previous = '';
  const assets = parts(assetItems).map((entry) => {
    const [policyItem, nameItem, quantityItem] = parts(entry, 3);
    const policyId = hexBytes(source, policyItem, 28);
    const assetName = hexBytes(source, nameItem);
    if (assetName.length > 64) fail();
    const id = `${policyId}:${assetName}`;
    if (seen.has(id) || (previous && previous.localeCompare(id) >= 0))
      fail('Native assets must be unique and sorted');
    seen.add(id);
    previous = id;
    return Object.freeze({
      policyId,
      assetName,
      quantity: integer(quantityItem),
    });
  });
  return Object.freeze({ coin, assets: Object.freeze(assets) });
};
const parseInput = (source: Buffer, item: CborItem): Input => {
  const [id, index, address, value] = parts(item, 4);
  return Object.freeze({
    transactionId: hexBytes(source, id, 32),
    index: integer(index, MAX_WORD32),
    address: hexBytes(source, address),
    value: parseValue(source, value),
  });
};
const parseOutput = (source: Buffer, item: CborItem): Output => {
  const [address, value, owned] = parts(item, 3);
  return Object.freeze({
    address: hexBytes(source, address),
    value: parseValue(source, value),
    sourceWalletOwned: boolean(owned),
  });
};
const parseWithdrawal = (source: Buffer, item: CborItem): Withdrawal => {
  const [account, coin, owned] = parts(item, 3);
  return Object.freeze({
    account: hexBytes(source, account),
    coin: integer(coin),
    sourceWalletOwned: boolean(owned),
  });
};
const parseMetadata = (
  source: Buffer,
  item: CborItem
): NativeTransactionPlanItem['metadataBinding'] => {
  if (item.major === 7 && item.additionalInformation === 22) return null;
  const values = parts(item);
  const kind = text(source, values[0]);
  if (kind === 'exact' && values.length === 2)
    return Object.freeze({ kind, metadataCbor: hexBytes(source, values[1]) });
  if (kind === 'catalyst' && values.length === 6)
    return Object.freeze({
      kind,
      votingPublicKey: hexBytes(source, values[1]),
      stakePublicKey: hexBytes(source, values[2]),
      rewardAddress: hexBytes(source, values[3]),
      nonce: integer(values[4]),
      votingPurpose: nullableInteger(values[5]),
    });
  return fail();
};
const parseItem = (
  source: Buffer,
  item: CborItem
): NativeTransactionPlanItem => {
  const values = parts(item, 11);
  const certificateItems = parts(values[4]);
  const parsed = Object.freeze({
    inputs: Object.freeze(
      parts(values[0]).map((value) => parseInput(source, value))
    ),
    outputs: Object.freeze(
      parts(values[1]).map((value) => parseOutput(source, value))
    ),
    fee: integer(values[2]),
    withdrawals: Object.freeze(
      parts(values[3]).map((value) => parseWithdrawal(source, value))
    ),
    certificatesCbor: Object.freeze(
      certificateItems.map((value) =>
        bytesForSpan(source, value.span).toString('hex')
      )
    ),
    metadataBinding: parseMetadata(source, values[5]),
    deposits: integer(values[6]),
    refunds: integer(values[7]),
    collateralInputs: Object.freeze(
      parts(values[8]).map((value) => parseInput(source, value))
    ),
    collateralReturn:
      values[9].major === 7 && values[9].additionalInformation === 22
        ? null
        : parseOutput(source, values[9]),
    maximumCollateralLoss: nullableInteger(values[10]),
  });
  const inputCoin =
    parsed.inputs.reduce(
      (total, input) => total + input.value.coin,
      BigInt(0)
    ) +
    parsed.withdrawals.reduce(
      (total, withdrawal) => total + withdrawal.coin,
      BigInt(0)
    ) +
    parsed.refunds;
  const outputCoin =
    parsed.outputs.reduce(
      (total, output) => total + output.value.coin,
      BigInt(0)
    ) +
    parsed.fee +
    parsed.deposits;
  if (inputCoin !== outputCoin) fail('Native plan coin is not conserved');
  const assetTotals = new Map<string, bigint>();
  const addAssets = (value: Value, direction: bigint) =>
    value.assets.forEach(({ policyId, assetName, quantity }) => {
      const id = `${policyId}:${assetName}`;
      assetTotals.set(
        id,
        (assetTotals.get(id) || BigInt(0)) + direction * quantity
      );
    });
  parsed.inputs.forEach(({ value }) => addAssets(value, BigInt(1)));
  parsed.outputs.forEach(({ value }) => addAssets(value, -BigInt(1)));
  if ([...assetTotals.values()].some((quantity) => quantity !== BigInt(0)))
    fail('Native plan assets are not conserved');
  if (parsed.maximumCollateralLoss !== null) {
    const collateralLoss =
      parsed.collateralInputs.reduce(
        (total, input) => total + input.value.coin,
        BigInt(0)
      ) - (parsed.collateralReturn?.value.coin || BigInt(0));
    if (collateralLoss !== parsed.maximumCollateralLoss)
      fail('Native plan collateral loss mismatch');
  }
  return parsed;
};

export const parseNativeTransactionPlan = (
  planCbor: string,
  expected?: Readonly<{
    walletId: string;
    network: DappNetwork;
    action: NativeTransactionAction;
  }>
): NativeTransactionPlan => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(planCbor)) fail();
  const source = Buffer.from(planCbor, 'hex');
  const root = parseCborItem(source);
  if (root.span.end !== source.length) fail('Trailing native plan data');
  walkCanonical(root);
  const values = parts(root, 9);
  if (text(source, values[0]) !== 'daedalus-native-review') fail();
  if (integer(values[1]) !== BigInt(1)) fail();
  const walletId = text(source, values[2]);
  const networkValues = parts(values[3], 3);
  const networkId = integer(networkValues[0], BigInt(1)) as BigInt;
  const networkMagic = integer(networkValues[1], MAX_WORD32);
  const network = Object.freeze({
    networkId: Number(networkId) as 0 | 1,
    networkMagic: Number(networkMagic),
    genesisHash: hexBytes(source, networkValues[2], 32),
  });
  const action = text(source, values[4]) as NativeTransactionAction;
  if (!(ACTIONS as readonly string[]).includes(action)) fail();
  const validityValues = parts(values[6], 2);
  const parsed = Object.freeze({
    walletId,
    network,
    action,
    protocolParametersHash: hexBytes(source, values[5], 32),
    validity: Object.freeze({
      invalidBefore: nullableInteger(validityValues[0]),
      invalidHereafter: nullableInteger(validityValues[1]),
    }),
    items: Object.freeze(
      parts(values[7]).map((item) => parseItem(source, item))
    ),
    sourceLeftoverValue: parseValue(source, values[8]),
  });
  if (!parsed.items.length) fail('Native transaction plan is empty');
  if (
    expected &&
    (parsed.walletId !== expected.walletId ||
      parsed.action !== expected.action ||
      parsed.network.networkId !== expected.network.networkId ||
      parsed.network.networkMagic !== expected.network.networkMagic ||
      parsed.network.genesisHash !== expected.network.genesisHash)
  )
    fail('Native transaction plan binding mismatch');
  return parsed;
};

export const nativeTransactionPlanDigest = (planCbor: string): string => {
  parseNativeTransactionPlan(planCbor);
  return Buffer.from(
    blake2b(Buffer.from(planCbor, 'hex'), undefined, 32)
  ).toString('hex');
};

const fingerprint = (policyId: string, assetName: string): string =>
  Cardano.AssetFingerprint.fromParts(
    Cardano.PolicyId(policyId),
    Cardano.AssetName(assetName)
  );
const objectKeys = (value: object): readonly string[] =>
  Object.keys(value).sort();
const exactKeys = (
  value: object,
  required: readonly string[],
  optional: readonly string[] = []
): void => {
  const actual = objectKeys(value);
  if (
    required.some((key) => !actual.includes(key)) ||
    actual.some((key) => !required.includes(key) && !optional.includes(key))
  )
    fail('Invalid native approval fields');
};
const canonicalUnsignedString = (value: unknown): value is string =>
  typeof value === 'string' && /^(0|[1-9]\d*)$/u.test(value);

type JsonObject = { [key: string]: unknown };
const object = (value: unknown): JsonObject => {
  if (!value || typeof value !== 'object' || Array.isArray(value)) fail();
  return value as JsonObject;
};
const isTransactionContextSnapshot = (
  value: unknown
): value is TransactionContextSnapshot => {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
  const context = value as JsonObject;
  if (
    !context.network ||
    typeof context.network !== 'object' ||
    Array.isArray(context.network)
  )
    return false;
  const network = context.network as JsonObject;
  return (
    typeof context.walletId === 'string' &&
    [0, 1].includes(Number(network.networkId)) &&
    Number.isSafeInteger(network.networkMagic) &&
    typeof network.genesisHash === 'string' &&
    typeof context.contextDigest === 'string' &&
    /^[0-9a-f]{64}$/u.test(context.contextDigest) &&
    Array.isArray(context.outputs) &&
    Array.isArray(context.ownership)
  );
};

export const parseNativePreparedApproval = (
  value: unknown
): NativePreparedApproval => {
  const preparedValue = object(value);
  exactKeys(preparedValue, [
    'walletId',
    'network',
    'action',
    'authorization',
    'items',
    'context',
  ]);
  const networkValue = object(preparedValue.network);
  const contextValue = object(preparedValue.context);
  if (
    typeof preparedValue.walletId !== 'string' ||
    !preparedValue.walletId ||
    typeof preparedValue.action !== 'string' ||
    !(ACTIONS as readonly string[]).includes(preparedValue.action) ||
    !['software', 'ledger', 'trezor'].includes(
      String(preparedValue.authorization)
    ) ||
    !Array.isArray(preparedValue.items) ||
    !preparedValue.items.length
  )
    fail();
  exactKeys(networkValue, ['networkId', 'networkMagic', 'genesisHash']);
  if (
    ![0, 1].includes(Number(networkValue.networkId)) ||
    !Number.isSafeInteger(networkValue.networkMagic) ||
    Number(networkValue.networkMagic) < 0 ||
    typeof networkValue.genesisHash !== 'string' ||
    !/^[0-9a-f]{64}$/u.test(networkValue.genesisHash)
  )
    fail();
  exactKeys(
    contextValue,
    ['walletName', 'networkName'],
    ['destinationWalletName']
  );
  if (
    typeof contextValue.walletName !== 'string' ||
    !contextValue.walletName ||
    typeof contextValue.networkName !== 'string' ||
    !contextValue.networkName ||
    (contextValue.destinationWalletName !== undefined &&
      typeof contextValue.destinationWalletName !== 'string')
  )
    fail();
  const network: DappNetwork = Object.freeze({
    networkId: Number(networkValue.networkId) as 0 | 1,
    networkMagic: Number(networkValue.networkMagic),
    genesisHash: networkValue.genesisHash as string,
  });
  const walletId = preparedValue.walletId as string;
  const action = preparedValue.action as NativeTransactionAction;
  const itemValues = preparedValue.items as unknown[];
  const items: Array<NativePreparedApproval['items'][number]> = itemValues.map(
    (candidateValue) => {
      const candidate = object(candidateValue);
      if (typeof candidate.kind !== 'string') return fail();
      if (candidate.kind === 'native-plan') {
        exactKeys(candidate, ['kind', 'planCbor', 'planDigest']);
        if (
          typeof candidate.planCbor !== 'string' ||
          typeof candidate.planDigest !== 'string'
        )
          return fail();
        parseNativeTransactionPlan(candidate.planCbor, {
          walletId,
          network,
          action,
        });
        if (
          candidate.planDigest !==
          nativeTransactionPlanDigest(candidate.planCbor)
        )
          return fail('Native plan digest mismatch');
        return Object.freeze({
          kind: 'native-plan' as const,
          planCbor: candidate.planCbor,
          planDigest: candidate.planDigest,
        });
      }
      if (candidate.kind !== 'exact-cbor') return fail();
      exactKeys(candidate, [
        'kind',
        'cbor',
        'transactionContext',
        'selectionFacts',
      ]);
      const selectionFacts = object(candidate.selectionFacts);
      if (
        typeof candidate.cbor !== 'string' ||
        !/^(?:[0-9a-f]{2})+$/u.test(candidate.cbor) ||
        !isTransactionContextSnapshot(candidate.transactionContext)
      )
        return fail();
      const rawDeposits = selectionFacts.deposits;
      const rawRefunds = selectionFacts.refunds;
      if (
        (rawDeposits !== null && !canonicalUnsignedString(rawDeposits)) ||
        (rawRefunds !== null && !canonicalUnsignedString(rawRefunds))
      )
        return fail();
      const deposits = rawDeposits as string | null;
      const refunds = rawRefunds as string | null;
      const context = candidate.transactionContext;
      if (
        context.walletId !== walletId ||
        context.network.networkId !== network.networkId ||
        context.network.networkMagic !== network.networkMagic ||
        context.network.genesisHash !== network.genesisHash
      )
        return fail('Native exact transaction context mismatch');
      return Object.freeze({
        kind: 'exact-cbor' as const,
        cbor: candidate.cbor,
        transactionContext: context,
        selectionFacts: Object.freeze({ deposits, refunds }),
      });
    }
  );
  return Object.freeze({
    walletId,
    network,
    action,
    authorization: preparedValue.authorization as NativeTransactionAuthorization,
    items: Object.freeze(items),
    context: Object.freeze({
      walletName: contextValue.walletName as string,
      networkName: contextValue.networkName as string,
      ...(contextValue.destinationWalletName === undefined
        ? {}
        : {
            destinationWalletName: contextValue.destinationWalletName as string,
          }),
    }),
  });
};

type BindingValue =
  | string
  | Buffer
  | bigint
  | number
  | null
  | readonly BindingValue[];
const cborHead = (major: number, value: bigint): Buffer => {
  if (value < BigInt(24)) return Buffer.from([(major << 5) | Number(value)]);
  const widths = [
    { maximum: BigInt(0xff), info: 24, bytes: 1 },
    { maximum: BigInt(0xffff), info: 25, bytes: 2 },
    { maximum: BigInt(0xffffffff), info: 26, bytes: 4 },
    { maximum: MAX_WORD64, info: 27, bytes: 8 },
  ];
  const width = widths.find(({ maximum }) => value <= maximum);
  if (!width) fail('Native binding integer out of range');
  const result = Buffer.alloc(1 + width.bytes);
  result[0] = (major << 5) | width.info;
  let remaining = value;
  for (let index = width.bytes; index > 0; index -= 1) {
    result[index] = Number(remaining & BigInt(0xff));
    remaining >>= BigInt(8);
  }
  return result;
};
const encodeBinding = (value: BindingValue): Buffer => {
  if (value === null) return Buffer.from([0xf6]);
  if (typeof value === 'number') return encodeBinding(BigInt(value));
  if (typeof value === 'bigint') return cborHead(0, value);
  if (typeof value === 'string') {
    const encoded = Buffer.from(value, 'utf8');
    return Buffer.concat([cborHead(3, BigInt(encoded.length)), encoded]);
  }
  if (Buffer.isBuffer(value))
    return Buffer.concat([cborHead(2, BigInt(value.length)), value]);
  const encoded = value.map(encodeBinding);
  return Buffer.concat([cborHead(4, BigInt(encoded.length)), ...encoded]);
};

export const nativeApprovalBindingDigest = (
  raw: NativePreparedApproval
): string => {
  const prepared = parseNativePreparedApproval(raw);
  const authorization =
    prepared.authorization === 'software' ? 'software' : prepared.authorization;
  const itemBindings: readonly BindingValue[] = prepared.items.map((item) =>
    item.kind === 'native-plan'
      ? ['native-plan', Buffer.from(item.planDigest, 'hex')]
      : [
          'exact-cbor',
          Buffer.from(blake2b(Buffer.from(item.cbor, 'hex'), undefined, 32)),
          Buffer.from(item.transactionContext.contextDigest, 'hex'),
          [
            item.selectionFacts.deposits === null
              ? null
              : BigInt(item.selectionFacts.deposits),
            item.selectionFacts.refunds === null
              ? null
              : BigInt(item.selectionFacts.refunds),
          ],
        ]
  );
  const binding: BindingValue = [
    'daedalus-native-execution',
    1,
    prepared.walletId,
    [
      prepared.network.networkId,
      prepared.network.networkMagic,
      Buffer.from(prepared.network.genesisHash, 'hex'),
    ],
    prepared.action,
    authorization,
    itemBindings,
  ];
  return Buffer.from(blake2b(encodeBinding(binding), undefined, 32)).toString(
    'hex'
  );
};

const displayValue = (value: Value): TransactionReviewValue =>
  Object.freeze({
    coin: value.coin.toString(),
    assets: Object.freeze(
      value.assets.map(({ policyId, assetName, quantity }) =>
        Object.freeze({
          policyId,
          assetName,
          fingerprint: fingerprint(policyId, assetName),
          quantity: quantity.toString(),
        })
      )
    ),
  });
const sum = (values: readonly Value[]): Value => {
  let coin = BigInt(0);
  const assets = new Map<
    string,
    { policyId: string; assetName: string; quantity: bigint }
  >();
  values.forEach((value) => {
    coin += value.coin;
    value.assets.forEach((asset) => {
      const id = `${asset.policyId}:${asset.assetName}`;
      const old = assets.get(id);
      assets.set(id, {
        ...asset,
        quantity: (old?.quantity || BigInt(0)) + asset.quantity,
      });
    });
  });
  return {
    coin,
    assets: [...assets.values()].filter(
      ({ quantity }) => quantity !== BigInt(0)
    ),
  };
};
const difference = (left: Value, right: Value): Value =>
  sum([
    left,
    {
      coin: -right.coin,
      assets: right.assets.map((asset) => ({
        ...asset,
        quantity: -asset.quantity,
      })),
    },
  ]);
const renderAddress = (
  raw: string
): Readonly<{
  address: string | null;
  control: TransactionReviewEntry['control'];
}> => {
  try {
    const address = Cardano.Address.fromBytes(
      raw as Parameters<typeof Cardano.Address.fromBytes>[0]
    );
    return address.getType() === Cardano.AddressType.Byron
      ? { address: address.toBase58(), control: 'byron' }
      : {
          address: address.toBech32(),
          control:
            address.getProps().paymentPart?.type ===
            Cardano.CredentialType.ScriptHash
              ? 'script'
              : 'key',
        };
  } catch {
    return { address: null, control: 'unknown' };
  }
};

export const createNativePlanReviewDisplay = (
  plan: NativeTransactionPlan,
  itemIndex = 0
): TransactionReviewDisplay => {
  const item = plan.items[itemIndex];
  if (!item) fail('Invalid native plan item index');
  let effectIndex = 0;
  const entry = (
    value: Input | Output,
    role: TransactionReviewEntry['role'],
    position: number,
    owned: boolean,
    outpoint: TransactionReviewEntry['outpoint']
  ): TransactionReviewEntry =>
    Object.freeze({
      effectIndex: effectIndex++,
      role,
      position,
      outpoint,
      ...renderAddress(value.address),
      ownership: owned ? 'wallet' : 'other',
      value: displayValue(value.value),
      hasDatum: null,
      hasReferenceScript: null,
    });
  const inputs = item.inputs.map((value, position) =>
    entry(value, 'input', position, true, {
      transactionId: value.transactionId,
      index: value.index.toString(),
    })
  );
  const outputs = item.outputs.map((value, position) =>
    entry(value, 'output', position, value.sourceWalletOwned, null)
  );
  const collateralInputs = item.collateralInputs.map((value, position) =>
    entry(value, 'collateral-input', position, true, {
      transactionId: value.transactionId,
      index: value.index.toString(),
    })
  );
  const collateralReturn = item.collateralReturn
    ? [
        entry(
          item.collateralReturn,
          'collateral-return',
          0,
          item.collateralReturn.sourceWalletOwned,
          null
        ),
      ]
    : [];
  const walletInputs = sum(item.inputs.map(({ value }) => value));
  const walletOutputs = sum(
    item.outputs
      .filter(({ sourceWalletOwned }) => sourceWalletOwned)
      .map(({ value }) => value)
  );
  const certificateKind = (raw: string): number => {
    const certificate = parseCborItem(Buffer.from(raw, 'hex'));
    const tag = parts(certificate)[0];
    return Number(integer(tag, BigInt(Number.MAX_SAFE_INTEGER)));
  };
  return Object.freeze({
    entries: Object.freeze([
      ...inputs,
      ...outputs,
      ...collateralInputs,
      ...collateralReturn,
    ]),
    walletInputs: displayValue(walletInputs),
    walletOutputs: displayValue(walletOutputs),
    walletChange: displayValue(difference(walletOutputs, walletInputs)),
    fee: item.fee.toString(),
    deposits: item.deposits.toString(),
    refunds: item.refunds.toString(),
    maximumCollateralLoss:
      item.maximumCollateralLoss === null
        ? null
        : Object.freeze({
            coin: item.maximumCollateralLoss.toString(),
            assets: [],
          }),
    mint: Object.freeze([] as TransactionReviewAsset[]),
    withdrawals: Object.freeze(
      item.withdrawals.map(({ account, coin, sourceWalletOwned }) =>
        Object.freeze({
          account,
          coin: coin.toString(),
          ownership: sourceWalletOwned
            ? ('wallet' as const)
            : ('other' as const),
        })
      )
    ),
    certificates: Object.freeze(
      item.certificatesCbor.map((raw) =>
        Object.freeze({
          kind: certificateKind(raw),
          poolId: null,
          credentialIdentities: Object.freeze([] as string[]),
          targetCredentialIdentities: Object.freeze([] as string[]),
        })
      )
    ),
    votes: Object.freeze([]),
    proposalCount: 0,
    donation: null,
  });
};
