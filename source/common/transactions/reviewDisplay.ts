import { Cardano } from '@cardano-sdk/core';

import {
  AssetQuantity,
  decodeConwayOutput,
  Output,
  SemanticTransaction,
  Value,
} from '../cardano/transaction';
import {
  ContextOwnership,
  TransactionContextSnapshot,
} from '../cardano/transactionContext';

const MAX_WORD64 = (BigInt(1) << BigInt(64)) - BigInt(1);
const MAX_WORD32 = (BigInt(1) << BigInt(32)) - BigInt(1);
const unsignedInteger = /^(0|[1-9]\d*)$/u;
const signedInteger = /^(0|-?[1-9]\d*)$/u;
const hex = /^(?:[0-9a-f]{2})*$/u;
const hash = /^[0-9a-f]{64}$/u;

export type TransactionReviewAsset = Readonly<{
  policyId: string;
  assetName: string;
  fingerprint: string;
  quantity: string;
}>;
export type TransactionReviewValue = Readonly<{
  coin: string;
  assets: readonly TransactionReviewAsset[];
}>;
export type TransactionReviewOwnership = 'wallet' | 'other' | 'unknown';
export type TransactionReviewEntry = Readonly<{
  effectIndex: number;
  role:
    | 'input'
    | 'output'
    | 'collateral-input'
    | 'collateral-return'
    | 'reference-input';
  position: number;
  outpoint: Readonly<{ transactionId: string; index: string }> | null;
  address: string | null;
  control: 'key' | 'script' | 'byron' | 'unknown';
  ownership: TransactionReviewOwnership;
  value: TransactionReviewValue | null;
  hasDatum: boolean | null;
  hasReferenceScript: boolean | null;
}>;
export type TransactionReviewContext = Pick<
  TransactionContextSnapshot,
  'outputs' | 'ownership' | 'network'
>;
export type TransactionReviewDisplay = Readonly<{
  entries: readonly TransactionReviewEntry[];
  walletInputs: TransactionReviewValue | null;
  walletOutputs: TransactionReviewValue | null;
  walletChange: TransactionReviewValue | null;
  fee: string;
  deposits: string | null;
  refunds: string | null;
  maximumCollateralLoss: TransactionReviewValue | null;
  mint: readonly TransactionReviewAsset[];
  withdrawals: readonly Readonly<{
    account: string;
    coin: string;
    ownership: TransactionReviewOwnership;
  }>[];
  certificates: readonly Readonly<{
    kind: number;
    poolId: string | null;
    credentialIdentities: readonly string[];
    targetCredentialIdentities: readonly string[];
  }>[];
  votes: readonly Readonly<{ voter: string; actionIds: readonly string[] }>[];
  proposalCount: number;
  donation: string | null;
}>;

type Classified = Readonly<{
  address: string | null;
  control: TransactionReviewEntry['control'];
  ownership: TransactionReviewOwnership;
  value: Value | null;
  hasDatum: boolean | null;
  hasReferenceScript: boolean | null;
}>;

const fail = (message = 'Invalid transaction review display'): never => {
  throw new Error(message);
};

const assetId = ({
  policyId,
  assetName,
}: Pick<AssetQuantity, 'policyId' | 'assetName'>) => `${policyId}:${assetName}`;

const fingerprint = (policyId: string, assetName: string): string =>
  Cardano.AssetFingerprint.fromParts(
    Cardano.PolicyId(policyId),
    Cardano.AssetName(assetName)
  );

const displayAssets = (
  assets: readonly AssetQuantity[]
): readonly TransactionReviewAsset[] =>
  Object.freeze(
    assets
      .filter(({ quantity }) => quantity !== BigInt(0))
      .map(({ policyId, assetName, quantity }) =>
        Object.freeze({
          policyId,
          assetName,
          fingerprint: fingerprint(policyId, assetName),
          quantity: quantity.toString(),
        })
      )
      .sort(
        (left, right) =>
          left.policyId.localeCompare(right.policyId) ||
          left.assetName.localeCompare(right.assetName)
      )
  );

const displayValue = (value: Value): TransactionReviewValue =>
  Object.freeze({
    coin: value.coin.toString(),
    assets: displayAssets(value.assets),
  });

const addValues = (values: readonly Value[]): Value => {
  let coin = BigInt(0);
  const assets = new Map<string, AssetQuantity>();
  values.forEach((value) => {
    coin += value.coin;
    value.assets.forEach((asset) => {
      const id = assetId(asset);
      assets.set(id, {
        ...asset,
        quantity: (assets.get(id)?.quantity || BigInt(0)) + asset.quantity,
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

const subtractValues = (left: Value, right: Value): Value =>
  addValues([
    left,
    {
      coin: -right.coin,
      assets: right.assets.map((asset) => ({
        ...asset,
        quantity: -asset.quantity,
      })),
    },
  ]);

const evidenceFor = (
  rows: readonly ContextOwnership[],
  kind: 'payment' | 'stake',
  credential: string
): ContextOwnership | undefined => {
  const matches = rows.filter(
    (row) => row.credentialKind === kind && row.credential === credential
  );
  if (
    matches.some(
      (row) =>
        row.ownership !== matches[0].ownership ||
        row.derivationPath.join(',') !== matches[0].derivationPath.join(',')
    )
  )
    fail('Conflicting transaction ownership evidence');
  return matches[0];
};

const parseAddress = (
  raw: string,
  context: TransactionReviewContext,
  walletMember?: boolean
): Omit<Classified, 'value' | 'hasDatum' | 'hasReferenceScript'> => {
  let address: ReturnType<typeof Cardano.Address.fromBytes>;
  try {
    address = Cardano.Address.fromBytes(
      raw as Parameters<typeof Cardano.Address.fromBytes>[0]
    );
  } catch {
    return { address: null, control: 'unknown', ownership: 'unknown' };
  }
  const type = address.getType();
  if (type === Cardano.AddressType.Byron)
    return {
      address: address.toBase58(),
      control: 'byron',
      ownership: 'other',
    };
  const props = address.getProps();
  const payment = props.paymentPart;
  const rendered = address.toBech32();
  if (!payment)
    return { address: rendered, control: 'unknown', ownership: 'unknown' };
  const script = payment.type === Cardano.CredentialType.ScriptHash;
  const evidence = evidenceFor(context.ownership, 'payment', payment.hash);
  if (script)
    return { address: rendered, control: 'script', ownership: 'other' };
  if (address.getNetworkId() !== context.network.networkId)
    return { address: rendered, control: 'key', ownership: 'other' };
  if (walletMember && evidence && evidence.ownership !== 'owned_key')
    fail('Wallet membership contradicts payment ownership');
  return {
    address: rendered,
    control: 'key',
    ownership: walletMember
      ? 'wallet'
      : evidence?.ownership === 'owned_key'
      ? 'wallet'
      : evidence?.ownership === 'unowned'
      ? 'other'
      : 'unknown',
  };
};

const classifyOutput = (
  output: Output,
  context: TransactionReviewContext,
  walletMember?: boolean
): Classified => ({
  ...parseAddress(output.address, context, walletMember),
  value: output.value,
  hasDatum: Boolean(output.datum),
  hasReferenceScript: Boolean(output.referenceScript),
});

const effectIndexes = (
  transaction: SemanticTransaction,
  kind: string
): number[] =>
  transaction.effects.reduce<number[]>((indexes, effect, index) => {
    if (effect.kind === kind) indexes.push(index);
    return indexes;
  }, []);

const createTransactionReviewEntry = (
  transaction: SemanticTransaction,
  context: TransactionReviewContext,
  role: TransactionReviewEntry['role'],
  position: number,
  classified: Classified,
  outpoint: TransactionReviewEntry['outpoint']
): TransactionReviewEntry => {
  const kind = role === 'input' ? 'input' : role === 'output' ? 'output' : role;
  const effectIndex = effectIndexes(transaction, kind)[position];
  if (effectIndex === undefined) fail(`Missing ${kind} effect`);
  return Object.freeze({
    effectIndex,
    role,
    position,
    outpoint,
    address: classified.address,
    control: classified.control,
    ownership: classified.ownership,
    value: classified.value ? displayValue(classified.value) : null,
    hasDatum: classified.hasDatum,
    hasReferenceScript: classified.hasReferenceScript,
  });
};

export const createTransactionReviewDisplay = (
  transaction: SemanticTransaction,
  context: TransactionReviewContext,
  mode: 'sign' | 'submit'
): TransactionReviewDisplay => {
  const entries: TransactionReviewEntry[] = [];
  const inputEntries = (
    role: 'input' | 'collateral-input' | 'reference-input',
    inputs: SemanticTransaction['inputs']['normal']
  ) =>
    inputs.forEach((input, position) => {
      const resolved = context.outputs.find(
        ({ outpoint }) =>
          outpoint.transactionId === input.transactionId &&
          BigInt(outpoint.index) === input.index
      );
      let classified: Classified = {
        address: null,
        control: 'unknown',
        ownership: 'unknown',
        value: null,
        hasDatum: null,
        hasReferenceScript: null,
      };
      if (resolved) {
        try {
          classified = classifyOutput(
            decodeConwayOutput(Buffer.from(resolved.sourceCbor, 'hex')),
            context,
            resolved.walletMember || undefined
          );
        } catch (error) {
          if (
            error instanceof Error &&
            (error.message.includes('contradicts') ||
              error.message.includes('ownership evidence'))
          )
            throw error;
        }
      }
      entries.push(
        createTransactionReviewEntry(
          transaction,
          context,
          role,
          position,
          classified,
          {
            transactionId: input.transactionId,
            index: input.index.toString(),
          }
        )
      );
    });

  inputEntries('input', transaction.inputs.normal);
  transaction.outputs.forEach((output, position) =>
    entries.push(
      createTransactionReviewEntry(
        transaction,
        context,
        'output',
        position,
        classifyOutput(output, context),
        { transactionId: transaction.transactionId, index: position.toString() }
      )
    )
  );
  inputEntries('collateral-input', transaction.inputs.collateral);
  if (transaction.collateral.return)
    entries.push(
      createTransactionReviewEntry(
        transaction,
        context,
        'collateral-return',
        0,
        classifyOutput(transaction.collateral.return, context),
        {
          transactionId: transaction.transactionId,
          index: transaction.outputs.length.toString(),
        }
      )
    );
  inputEntries('reference-input', transaction.inputs.reference);

  const invalidSubmission = mode === 'submit' && !transaction.envelope.isValid;
  const selectedInputs = entries.filter(({ role }) =>
    invalidSubmission ? role === 'collateral-input' : role === 'input'
  );
  const selectedOutputs = entries.filter(({ role }) =>
    invalidSubmission ? role === 'collateral-return' : role === 'output'
  );
  const complete = [...selectedInputs, ...selectedOutputs].every(
    ({ ownership, value }) => ownership !== 'unknown' && value !== null
  );
  const ownedValues = (candidates: readonly TransactionReviewEntry[]) =>
    candidates
      .filter(({ ownership }) => ownership === 'wallet')
      .map(({ value }) => value)
      .filter((value): value is TransactionReviewValue => value !== null)
      .map((value) => ({
        coin: BigInt(value.coin),
        assets: value.assets.map(({ policyId, assetName, quantity }) => ({
          policyId,
          assetName,
          quantity: BigInt(quantity),
        })),
      }));
  const walletInputsValue = complete
    ? addValues(ownedValues(selectedInputs))
    : null;
  const walletOutputsValue = complete
    ? addValues(ownedValues(selectedOutputs))
    : null;
  const withdrawalOwnership = (account: string): TransactionReviewOwnership => {
    try {
      const address = Cardano.Address.fromBytes(
        account as Parameters<typeof Cardano.Address.fromBytes>[0]
      );
      const credential = address.getProps().paymentPart;
      if (!credential || credential.type === Cardano.CredentialType.ScriptHash)
        return 'other';
      const evidence = evidenceFor(context.ownership, 'stake', credential.hash);
      return evidence?.ownership === 'owned_key'
        ? 'wallet'
        : evidence?.ownership === 'unowned'
        ? 'other'
        : 'unknown';
    } catch {
      return 'unknown';
    }
  };

  return Object.freeze({
    entries: Object.freeze(entries),
    walletInputs: walletInputsValue ? displayValue(walletInputsValue) : null,
    walletOutputs: walletOutputsValue ? displayValue(walletOutputsValue) : null,
    walletChange:
      walletInputsValue && walletOutputsValue
        ? displayValue(subtractValues(walletOutputsValue, walletInputsValue))
        : null,
    fee: transaction.fee.toString(),
    deposits: null,
    refunds: null,
    maximumCollateralLoss: transaction.collateral.maximumLoss
      ? displayValue(transaction.collateral.maximumLoss)
      : null,
    mint: displayAssets(transaction.mint),
    withdrawals: Object.freeze(
      transaction.withdrawals.map(({ account, coin }) =>
        Object.freeze({
          account,
          coin: coin.toString(),
          ownership: withdrawalOwnership(account),
        })
      )
    ),
    certificates: Object.freeze(
      transaction.certificates.map(({ value }) =>
        Object.freeze({
          kind: value.kind,
          poolId: value.poolId || null,
          credentialIdentities: Object.freeze([...value.credentialIdentities]),
          targetCredentialIdentities: Object.freeze([
            ...value.targetCredentialIdentities,
          ]),
        })
      )
    ),
    votes: Object.freeze(
      transaction.governance.votes.map((vote) =>
        Object.freeze({
          voter: vote.voter,
          actionIds: Object.freeze(
            vote.actions.map(({ actionId }) => actionId)
          ),
        })
      )
    ),
    proposalCount: transaction.governance.proposals.length,
    donation:
      transaction.governance.donation === undefined
        ? null
        : transaction.governance.donation.toString(),
  });
};

type Json = Record<string, unknown>;
const object = (
  value: unknown,
  keys: readonly string[],
  name: string
): Json => {
  if (!value || typeof value !== 'object' || Array.isArray(value))
    fail(`Invalid ${name}`);
  const prototype = Object.getPrototypeOf(value);
  const descriptors = Object.getOwnPropertyDescriptors(value);
  if (
    (prototype !== Object.prototype && prototype !== null) ||
    Object.getOwnPropertySymbols(value).length ||
    Object.keys(descriptors).sort().join(',') !== [...keys].sort().join(',') ||
    keys.some(
      (key) => !Object.prototype.hasOwnProperty.call(descriptors[key], 'value')
    )
  )
    fail(`Invalid ${name}`);
  return value as Json;
};
const integer = (
  value: unknown,
  name: string,
  signed = false,
  maximum?: bigint
): string => {
  if (
    typeof value !== 'string' ||
    !(signed ? signedInteger : unsignedInteger).test(value)
  )
    fail(`Invalid ${name}`);
  const parsed = BigInt(value as string);
  if (
    maximum !== undefined &&
    (parsed > maximum || (signed && parsed < -maximum))
  )
    fail(`Invalid ${name}`);
  return value as string;
};
const nullableInteger = (
  value: unknown,
  name: string,
  maximum?: bigint
): string | null =>
  value === null ? null : integer(value, name, false, maximum);
const text = (value: unknown, name: string): string =>
  typeof value === 'string' && value.length ? value : fail(`Invalid ${name}`);
const texts = (value: unknown, name: string): readonly string[] =>
  Array.isArray(value) &&
  value.every((item) => typeof item === 'string' && item.length)
    ? Object.freeze([...value])
    : fail(`Invalid ${name}`);
const ownership = (value: unknown): TransactionReviewOwnership =>
  value === 'wallet' || value === 'other' || value === 'unknown'
    ? value
    : fail('Invalid ownership');

const parseAsset = (
  value: unknown,
  signed: boolean,
  bounded: boolean
): TransactionReviewAsset => {
  const item = object(
    value,
    ['policyId', 'assetName', 'fingerprint', 'quantity'],
    'asset'
  );
  const policyId = item.policyId;
  const assetName = item.assetName;
  if (
    typeof policyId !== 'string' ||
    !/^[0-9a-f]{56}$/u.test(policyId) ||
    typeof assetName !== 'string' ||
    !hex.test(assetName) ||
    assetName.length > 64
  )
    fail('Invalid asset identity');
  const expectedFingerprint = fingerprint(
    policyId as string,
    assetName as string
  );
  if (item.fingerprint !== expectedFingerprint)
    fail('Invalid asset fingerprint');
  return Object.freeze({
    policyId: policyId as string,
    assetName: assetName as string,
    fingerprint: expectedFingerprint,
    quantity: integer(
      item.quantity,
      'asset quantity',
      signed,
      bounded ? MAX_WORD64 : undefined
    ),
  });
};

const parseValue = (
  value: unknown,
  signed: boolean,
  bounded: boolean
): TransactionReviewValue => {
  const item = object(value, ['coin', 'assets'], 'value');
  const rawAssets = item.assets;
  if (!Array.isArray(rawAssets)) fail('Invalid value assets');
  const assets = (rawAssets as unknown[]).map((asset) =>
    parseAsset(asset, signed, bounded)
  );
  const identities = assets.map(
    ({ policyId, assetName }) => `${policyId}:${assetName}`
  );
  if (new Set(identities).size !== identities.length)
    fail('Duplicate asset identity');
  return Object.freeze({
    coin: integer(item.coin, 'coin', signed, bounded ? MAX_WORD64 : undefined),
    assets: Object.freeze(assets),
  });
};

export const parseTransactionReviewDisplay = (
  value: unknown
): TransactionReviewDisplay => {
  const root = object(
    value,
    [
      'entries',
      'walletInputs',
      'walletOutputs',
      'walletChange',
      'fee',
      'deposits',
      'refunds',
      'maximumCollateralLoss',
      'mint',
      'withdrawals',
      'certificates',
      'votes',
      'proposalCount',
      'donation',
    ],
    'transaction review display'
  );
  const rawEntries = root.entries;
  const rawMint = root.mint;
  const rawWithdrawals = root.withdrawals;
  const rawCertificates = root.certificates;
  const rawVotes = root.votes;
  if (
    !Array.isArray(rawEntries) ||
    !Array.isArray(rawMint) ||
    !Array.isArray(rawWithdrawals) ||
    !Array.isArray(rawCertificates) ||
    !Array.isArray(rawVotes)
  )
    fail();
  const entries = (rawEntries as unknown[]).map((candidate, index) => {
    const item = object(
      candidate,
      [
        'effectIndex',
        'role',
        'position',
        'outpoint',
        'address',
        'control',
        'ownership',
        'value',
        'hasDatum',
        'hasReferenceScript',
      ],
      'entry'
    );
    const roles = [
      'input',
      'output',
      'collateral-input',
      'collateral-return',
      'reference-input',
    ];
    const controls = ['key', 'script', 'byron', 'unknown'];
    if (
      !Number.isSafeInteger(item.effectIndex) ||
      Number(item.effectIndex) < 0 ||
      !roles.includes(item.role as string) ||
      !Number.isSafeInteger(item.position) ||
      Number(item.position) < 0 ||
      (item.address !== null &&
        (typeof item.address !== 'string' || !item.address.length)) ||
      !controls.includes(item.control as string) ||
      (item.hasDatum !== null && typeof item.hasDatum !== 'boolean') ||
      (item.hasReferenceScript !== null &&
        typeof item.hasReferenceScript !== 'boolean')
    )
      fail(`Invalid entry ${index}`);
    let outpoint = null;
    if (item.outpoint !== null) {
      const point = object(
        item.outpoint,
        ['transactionId', 'index'],
        'outpoint'
      );
      if (
        typeof point.transactionId !== 'string' ||
        !hash.test(point.transactionId)
      )
        fail('Invalid outpoint');
      outpoint = Object.freeze({
        transactionId: point.transactionId,
        index: integer(point.index, 'outpoint index', false, MAX_WORD32),
      });
    }
    return Object.freeze({
      effectIndex: Number(item.effectIndex),
      role: item.role as TransactionReviewEntry['role'],
      position: Number(item.position),
      outpoint,
      address: item.address as string | null,
      control: item.control as TransactionReviewEntry['control'],
      ownership: ownership(item.ownership),
      value: item.value === null ? null : parseValue(item.value, false, true),
      hasDatum: item.hasDatum as boolean | null,
      hasReferenceScript: item.hasReferenceScript as boolean | null,
    });
  });
  const uniquePositions = new Set(
    entries.map(({ role, position }) => `${role}:${position}`)
  );
  if (uniquePositions.size !== entries.length) fail('Duplicate entry position');
  if (
    new Set(entries.map(({ effectIndex }) => effectIndex)).size !==
    entries.length
  )
    fail('Duplicate effect index');
  for (const role of [
    'input',
    'output',
    'collateral-input',
    'collateral-return',
    'reference-input',
  ] as const) {
    if (
      entries
        .filter((entry) => entry.role === role)
        .some(({ position }, index) => position !== index)
    )
      fail('Invalid entry positions');
  }
  const nullableValue = (
    candidate: unknown,
    signed: boolean,
    bounded: boolean
  ) => (candidate === null ? null : parseValue(candidate, signed, bounded));
  const withdrawals = (rawWithdrawals as unknown[]).map((candidate) => {
    const item = object(
      candidate,
      ['account', 'coin', 'ownership'],
      'withdrawal'
    );
    return Object.freeze({
      account: text(item.account, 'withdrawal account'),
      coin: integer(item.coin, 'withdrawal coin', false, MAX_WORD64),
      ownership: ownership(item.ownership),
    });
  });
  const certificates = (rawCertificates as unknown[]).map((candidate) => {
    const item = object(
      candidate,
      ['kind', 'poolId', 'credentialIdentities', 'targetCredentialIdentities'],
      'certificate'
    );
    if (!Number.isSafeInteger(item.kind) || Number(item.kind) < 0)
      fail('Invalid certificate kind');
    if (
      item.poolId !== null &&
      (typeof item.poolId !== 'string' || !/^[0-9a-f]{56}$/u.test(item.poolId))
    )
      fail('Invalid pool id');
    return Object.freeze({
      kind: Number(item.kind),
      poolId: item.poolId as string | null,
      credentialIdentities: texts(item.credentialIdentities, 'credentials'),
      targetCredentialIdentities: texts(
        item.targetCredentialIdentities,
        'target credentials'
      ),
    });
  });
  const votes = (rawVotes as unknown[]).map((candidate) => {
    const item = object(candidate, ['voter', 'actionIds'], 'vote');
    return Object.freeze({
      voter: text(item.voter, 'voter'),
      actionIds: texts(item.actionIds, 'action ids'),
    });
  });
  if (
    !Number.isSafeInteger(root.proposalCount) ||
    Number(root.proposalCount) < 0
  )
    fail('Invalid proposal count');
  return Object.freeze({
    entries: Object.freeze(entries),
    walletInputs: nullableValue(root.walletInputs, false, false),
    walletOutputs: nullableValue(root.walletOutputs, false, false),
    walletChange: nullableValue(root.walletChange, true, false),
    fee: integer(root.fee, 'fee', false, MAX_WORD64),
    deposits: nullableInteger(root.deposits, 'deposits', MAX_WORD64),
    refunds: nullableInteger(root.refunds, 'refunds', MAX_WORD64),
    maximumCollateralLoss: nullableValue(
      root.maximumCollateralLoss,
      false,
      false
    ),
    mint: Object.freeze(
      (rawMint as unknown[]).map((asset) => parseAsset(asset, true, true))
    ),
    withdrawals: Object.freeze(withdrawals),
    certificates: Object.freeze(certificates),
    votes: Object.freeze(votes),
    proposalCount: Number(root.proposalCount),
    donation: nullableInteger(root.donation, 'donation', MAX_WORD64),
  });
};
