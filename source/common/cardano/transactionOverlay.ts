import { bytesForSpan } from './cborSlices';
import type { Output, SemanticTransaction } from './transaction';
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

export type Cip103Conflict = Readonly<{
  transactionIndex: number;
  inputRole: Cip103InputRole;
  outpoint: Readonly<{ transactionId: string; index: number }>;
  earlierTransactionIndex: number;
}>;

export type Cip103ProducedOutput = Readonly<{
  transactionIndex: number;
  outputIndex: number;
  kind: 'output' | 'collateral-return';
  outpoint: Readonly<{ transactionId: string; index: number }>;
  sourceCbor: string;
  value: Output;
}>;

export type Cip103ItemEffects = Readonly<{
  outcome: 'valid' | 'invalid';
  spent: readonly Cip103ResolvedInput[];
  referenced: readonly Cip103ResolvedInput[];
  produced: readonly Cip103ProducedOutput[];
  semantic: SemanticTransaction['effects'];
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
    conflicts: readonly Cip103Conflict[];
    effects: Cip103ItemEffects;
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

const producedOutputAt = (
  item: Cip103PreflightItem,
  outputIndex: number
):
  | Readonly<{ kind: Cip103ProducedOutput['kind']; value: Output }>
  | undefined => {
  if (item.envelope.isValid) {
    const value = item.transaction.outputs[outputIndex];
    return value ? { kind: 'output', value } : undefined;
  }
  return outputIndex === item.transaction.outputs.length &&
    item.transaction.collateral.return
    ? { kind: 'collateral-return', value: item.transaction.collateral.return }
    : undefined;
};

const producedOutputs = (
  item: Cip103PreflightItem
): readonly Cip103ProducedOutput[] => {
  let values: readonly Readonly<{
    kind: Cip103ProducedOutput['kind'];
    value: Output;
  }>[] = [];
  if (item.envelope.isValid)
    values = item.transaction.outputs.map((value) => ({
      kind: 'output',
      value,
    }));
  else if (item.transaction.collateral.return)
    values = [
      {
        kind: 'collateral-return',
        value: item.transaction.collateral.return,
      },
    ];
  const firstIndex = item.envelope.isValid
    ? 0
    : item.transaction.outputs.length;
  return Object.freeze(
    values.map(({ kind: outputKind, value }, offset) => {
      const outputIndex = firstIndex + offset;
      return Object.freeze({
        transactionIndex: item.index,
        outputIndex,
        kind: outputKind,
        outpoint: Object.freeze({
          transactionId: item.bodyHash,
          index: outputIndex,
        }),
        sourceCbor: bytesForSpan(item.envelope.cbor, value.exactSpan).toString(
          'hex'
        ),
        value,
      });
    })
  );
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
          const produced = producedOutputAt(parent, outputIndex);
          if (!produced || !output.provenance.includes('earlier'))
            return fail(item.index, inputRole, 'unresolved_input');
          const parentCbor = bytesForSpan(
            parent.envelope.cbor,
            produced.value.exactSpan
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

  const claimed = new Map<string, number>();
  const resolvedItems = items.map((item) => {
    const inputs = Object.freeze({
      normal: resolveRole(item, 'normal'),
      collateral: resolveRole(item, 'collateral'),
      reference: resolveRole(item, 'reference'),
    });
    const conflicts: Cip103Conflict[] = [];
    [...inputs.normal, ...inputs.collateral].forEach((input) => {
      const earlierTransactionIndex = claimed.get(
        key(input.outpoint.transactionId, input.outpoint.index)
      );
      if (earlierTransactionIndex !== undefined)
        conflicts.push(
          Object.freeze({
            transactionIndex: item.index,
            inputRole: input.inputRole,
            outpoint: input.outpoint,
            earlierTransactionIndex,
          })
        );
    });
    const spent = item.envelope.isValid ? inputs.normal : inputs.collateral;
    spent.forEach((input) => {
      const id = key(input.outpoint.transactionId, input.outpoint.index);
      if (!claimed.has(id)) claimed.set(id, item.index);
    });
    return Object.freeze({
      transactionIndex: item.index,
      bodyHash: item.bodyHash,
      fullCborDigest: item.fullCborDigest,
      inputs,
      conflicts,
      effects: Object.freeze({
        outcome: item.envelope.isValid ? 'valid' : 'invalid',
        spent: Object.freeze([...spent]),
        referenced: Object.freeze([...inputs.reference]),
        produced: producedOutputs(item),
        semantic: Object.freeze([...item.transaction.effects]),
      }),
    });
  });

  return Object.freeze({
    state: 'resolved',
    items: Object.freeze(resolvedItems),
  });
};
