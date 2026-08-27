import { bytesForSpan } from './cborSlices';
import type {
  ContextOutput,
  TransactionContextSnapshot,
} from './transactionContext';
import type { Cip103PreflightItem } from '../types/cip103.types';

export type Cip103InputRole = 'normal' | 'collateral' | 'reference';
export type Cip103InputSource = 'earlier' | 'pending' | 'node';
export type Cip103OverlayFailure =
  | 'invalid_batch'
  | 'self_or_forward_input'
  | 'unresolved_input'
  | 'role_mismatch'
  | 'source_conflict';

export class Cip103OverlayError extends Error {
  public constructor(
    public readonly transactionIndex: number,
    public readonly inputRole: Cip103InputRole,
    public readonly failure: Cip103OverlayFailure
  ) {
    super(failure);
    this.name = 'Cip103OverlayError';
  }
}

export type Cip103ResolvedInput = Readonly<{
  transactionIndex: number;
  inputRole: Cip103InputRole;
  inputIndex: number;
  outpoint: Readonly<{ transactionId: string; index: number }>;
  source: Cip103InputSource;
  sourceTransactionIndex?: number;
  sourceCbor: string;
  provenance: readonly Cip103InputSource[];
}>;

export type Cip103Resolution = Readonly<{
  state: 'resolved';
  items: readonly Readonly<{
    transactionIndex: number;
    bodyHash: string;
    fullCborDigest: string;
    inputs: Readonly<{
      normal: readonly Cip103ResolvedInput[];
      collateral: readonly Cip103ResolvedInput[];
      reference: readonly Cip103ResolvedInput[];
    }>;
  }>[];
}>;

const key = (transactionId: string, index: number): string =>
  `${transactionId}:${index}`;

const fail = (
  transactionIndex: number,
  inputRole: Cip103InputRole,
  failure: Cip103OverlayFailure
): never => {
  throw new Cip103OverlayError(transactionIndex, inputRole, failure);
};

const outputMap = (
  outputs: readonly ContextOutput[]
): ReadonlyMap<string, ContextOutput> => {
  const result = new Map<string, ContextOutput>();
  outputs.forEach((output) => {
    const id = key(output.outpoint.transactionId, output.outpoint.index);
    if (result.has(id)) fail(0, 'normal', 'invalid_batch');
    result.set(id, output);
  });
  return result;
};

export const resolveCip103TransactionOverlay = (
  items: readonly Cip103PreflightItem[],
  snapshot: TransactionContextSnapshot
): Cip103Resolution => {
  const outputs = outputMap(snapshot.outputs);
  const hashes = new Map<string, Cip103PreflightItem[]>();
  items.forEach((item, position) => {
    if (item.index !== position) fail(position, 'normal', 'invalid_batch');
    const matching = hashes.get(item.bodyHash) || [];
    matching.push(item);
    hashes.set(item.bodyHash, matching);
  });

  const resolveRole = (
    item: Cip103PreflightItem,
    inputRole: Cip103InputRole
  ): readonly Cip103ResolvedInput[] =>
    Object.freeze(
      item.transaction.inputs[inputRole].map((input, inputIndex) => {
        const outputIndex = Number(input.index);
        if (!Number.isSafeInteger(outputIndex))
          return fail(item.index, inputRole, 'unresolved_input');
        const matching = hashes.get(input.transactionId) || [];
        const parent = matching
          .filter((candidate) => candidate.index < item.index)
          .pop();
        if (matching.length && !parent)
          return fail(item.index, inputRole, 'self_or_forward_input');

        const output = outputs.get(key(input.transactionId, outputIndex));
        if (!output) return fail(item.index, inputRole, 'unresolved_input');
        if (!output.roles.includes(inputRole))
          return fail(item.index, inputRole, 'role_mismatch');

        let source: Cip103InputSource;
        let sourceTransactionIndex: number | undefined;
        if (parent) {
          const produced = parent.transaction.outputs[outputIndex];
          if (!produced || !output.provenance.includes('earlier'))
            return fail(item.index, inputRole, 'unresolved_input');
          const parentCbor = bytesForSpan(
            parent.envelope.cbor,
            produced.exactSpan
          ).toString('hex');
          if (parentCbor !== output.sourceCbor)
            return fail(item.index, inputRole, 'source_conflict');
          source = 'earlier';
          sourceTransactionIndex = parent.index;
        } else if (output.provenance.includes('pending')) {
          source = 'pending';
        } else if (output.provenance.includes('node')) {
          source = 'node';
        } else {
          return fail(item.index, inputRole, 'unresolved_input');
        }

        return Object.freeze({
          transactionIndex: item.index,
          inputRole,
          inputIndex,
          outpoint: Object.freeze({
            transactionId: input.transactionId,
            index: outputIndex,
          }),
          source,
          ...(sourceTransactionIndex === undefined
            ? {}
            : { sourceTransactionIndex }),
          sourceCbor: output.sourceCbor,
          provenance: Object.freeze([...output.provenance]),
        });
      })
    );

  return Object.freeze({
    state: 'resolved',
    items: Object.freeze(
      items.map((item) =>
        Object.freeze({
          transactionIndex: item.index,
          bodyHash: item.bodyHash,
          fullCborDigest: item.fullCborDigest,
          inputs: Object.freeze({
            normal: resolveRole(item, 'normal'),
            collateral: resolveRole(item, 'collateral'),
            reference: resolveRole(item, 'reference'),
          }),
        })
      )
    ),
  });
};
