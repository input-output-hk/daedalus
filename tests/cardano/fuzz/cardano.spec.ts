import { generateKeyPairSync, sign } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import cip8Fixture from '../../../source/common/cip30/contracts/fixtures/cip8-cip95-fixture.json';
import regressionFixture from '../../../source/common/cardano/fixtures/exact-cbor/conway-regression.json';
import {
  Cip8BackendResponse,
  verifyCip8BackendResponse,
} from '../../../source/common/cardano/cip8';
import { prepareCip8Request } from '../../../source/common/cardano/cip8Request';
import {
  CborParseError,
  parseCborItem,
} from '../../../source/common/cardano/cborSlices';
import {
  decodeConwayTransaction,
  TransactionSemanticError,
} from '../../../source/common/cardano/transaction';
import {
  parseConwayTransactionEnvelope,
  TransactionEnvelopeError,
} from '../../../source/common/cardano/transactionEnvelope';
import {
  diffVKeyWitnesses,
  encodeVKeyWitnessSet,
} from '../../../source/common/cardano/witnessSet';

const encode = (value: unknown): Buffer => cbor.encodeCanonical(value);
const hash = (value: Buffer): string =>
  Buffer.from(blake2b(value, undefined, 32)).toString('hex');
const mutateByte = (source: Buffer, index: number): Buffer => {
  const mutated = Buffer.from(source);
  mutated[index] ^= 1 << index % 8;
  return mutated;
};
const transaction = (
  body: Map<number, unknown>,
  witnesses: Map<number, unknown> = new Map(),
  auxiliary: unknown = null
) =>
  parseConwayTransactionEnvelope(encode([body, witnesses, false, auxiliary]));
const baseBody = () =>
  new Map<number, unknown>([
    [0, []],
    [1, []],
    [2, 0],
  ]);

const rejectedParserError = (error: unknown): boolean =>
  error instanceof CborParseError ||
  error instanceof TransactionEnvelopeError ||
  error instanceof TransactionSemanticError;

const cip8Response = (
  overrides: Partial<Cip8BackendResponse> = {}
): Cip8BackendResponse => ({
  revision: 1,
  credential_kind: 'drep',
  credential: cip8Fixture.drepId,
  cose_sign1: cip8Fixture.coseSign1,
  cose_key: cip8Fixture.coseKey,
  ...overrides,
});

describe('deterministic Conway parser fuzzing', () => {
  it('preserves exact body and review commitments for every accepted byte mutation', () => {
    const source = Buffer.from(regressionFixture.cborHex, 'hex');
    let accepted = 0;
    let rejected = 0;

    for (let index = 0; index < source.length; index += 1) {
      const mutated = mutateByte(source, index);
      try {
        const envelope = parseConwayTransactionEnvelope(mutated);
        const decoded = decodeConwayTransaction(envelope);
        const exactBody = mutated.subarray(
          envelope.spans.body.start,
          envelope.spans.body.end
        );

        expect(envelope.transactionId).toBe(hash(exactBody));
        expect(decoded.envelope.cbor).toEqual(mutated);
        expect(decoded.envelope.transactionId).toBe(envelope.transactionId);
        accepted += 1;
      } catch (error) {
        if (!rejectedParserError(error)) throw error;
        rejected += 1;
      }
    }

    expect(accepted).toBeGreaterThan(0);
    expect(accepted + rejected).toBe(source.length);
  });

  it('rejects duplicate keys, invalid tags, malformed depth, truncation, and trailing bytes', () => {
    const source = Buffer.from(regressionFixture.cborHex, 'hex');
    const duplicateFee = Buffer.from('84a3008002000200a0f5f6', 'hex');
    const wrongSetTag = encode([
      new Map<number, unknown>([
        [0, new cbor.Tagged(257, [])],
        [1, []],
        [2, 0],
      ]),
      new Map(),
      true,
      null,
    ]);
    const unknownAuxiliaryTag = encode([
      new Map<number, unknown>([
        [0, []],
        [1, []],
        [2, 0],
      ]),
      new Map(),
      true,
      new cbor.Tagged(999, new Map()),
    ]);
    let deeplyNested: unknown = 0;
    for (let depth = 0; depth < 130; depth += 1) deeplyNested = [deeplyNested];

    [duplicateFee, wrongSetTag, unknownAuxiliaryTag].forEach((value) =>
      expect(() => parseConwayTransactionEnvelope(value)).toThrow()
    );
    expect(() => parseCborItem(encode(deeplyNested))).toThrow(CborParseError);
    expect(() =>
      parseConwayTransactionEnvelope(Buffer.concat([source, Buffer.from([0])]))
    ).toThrow(TransactionEnvelopeError);
    for (let end = 0; end < source.length; end += 1)
      expect(() =>
        parseConwayTransactionEnvelope(source.subarray(0, end))
      ).toThrow();
  });

  it('rejects mutations of auxiliary data and script-data commitments', () => {
    const auxiliary = new Map([[0, 1]]);
    const auxiliaryBody = baseBody();
    auxiliaryBody.set(7, Buffer.from(hash(encode(auxiliary)), 'hex'));
    expect(
      decodeConwayTransaction(transaction(auxiliaryBody, new Map(), auxiliary))
    ).toBeDefined();
    expect(() =>
      decodeConwayTransaction(
        transaction(auxiliaryBody, new Map(), new Map([[0, 2]]))
      )
    ).toThrow(TransactionSemanticError);

    const plutusV1 = Buffer.alloc(8, 0x22);
    const plutusV1Hash = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([1]), plutusV1]), undefined, 28)
    ).toString('hex');
    const input = [Buffer.alloc(32, 6), 0];
    const datums = [0];
    const redeemers = [[0, 0, 0, [0, 0]]];
    const languageView = Buffer.from('439f01ff', 'hex');
    const scriptData = Buffer.concat([
      encode(redeemers),
      encode(datums),
      Buffer.concat([Buffer.from('a14100', 'hex'), languageView]),
    ]);
    const scriptBody = new Map<number, unknown>([
      [0, [input]],
      [1, [[Buffer.alloc(0), 0]]],
      [2, 0],
      [11, Buffer.from(hash(scriptData), 'hex')],
    ]);
    const unresolved = decodeConwayTransaction(
      transaction(
        scriptBody,
        new Map<number, unknown>([
          [3, [plutusV1]],
          [4, datums],
          [5, redeemers],
        ])
      )
    );
    const context = {
      languageViews: new Map([[0, languageView]]),
      usedPlutusLanguages: [0] as const,
      resolvedInputs: [
        {
          outpoint: unresolved.inputs.normal[0],
          datum: {
            kind: 'hash' as const,
            hash: hash(Buffer.from([0])),
            cbor: '00',
          },
          scriptHash: plutusV1Hash,
          value: { coin: BigInt(0), assets: [] },
        },
      ],
      redeemerScriptHashes: new Map([
        [`${Buffer.alloc(32, 6).toString('hex')}:0`, plutusV1Hash],
      ]),
    };
    const witnesses = (
      scripts: readonly Buffer[],
      datumValues: readonly unknown[],
      redeemerValues: readonly unknown[]
    ) =>
      new Map<number, unknown>([
        [3, scripts],
        [4, datumValues],
        [5, redeemerValues],
      ]);

    expect(
      decodeConwayTransaction(
        transaction(scriptBody, witnesses([plutusV1], datums, redeemers)),
        context
      ).review.complete
    ).toBe(true);
    [
      () => transaction(scriptBody, witnesses([plutusV1], [1], redeemers)),
      () =>
        transaction(
          scriptBody,
          witnesses([plutusV1], datums, [[0, 0, 1, [0, 0]]])
        ),
      () =>
        transaction(
          scriptBody,
          witnesses([mutateByte(plutusV1, 0)], datums, redeemers)
        ),
    ].forEach((mutated) =>
      expect(() => decodeConwayTransaction(mutated(), context)).toThrow(
        TransactionSemanticError
      )
    );
    expect(() =>
      decodeConwayTransaction(
        transaction(scriptBody, witnesses([plutusV1], datums, redeemers)),
        {
          ...context,
          languageViews: new Map([[0, mutateByte(languageView, 0)]]),
        }
      )
    ).toThrow(TransactionSemanticError);
  });
});

describe('deterministic signature fuzzing', () => {
  it('rejects every mutated reference-script body and existing VKey witness', () => {
    const keys = generateKeyPairSync('ed25519');
    const publicKey = keys.publicKey
      .export({ format: 'der', type: 'spki' })
      .subarray(-32);
    const referenceBody = (scriptByte: number) =>
      new Map<number, unknown>([
        [0, []],
        [
          1,
          [
            new Map<number, unknown>([
              [0, Buffer.alloc(0)],
              [1, 0],
              [
                3,
                new cbor.Tagged(
                  24,
                  encode([0, [0, Buffer.alloc(28, scriptByte)]])
                ),
              ],
            ]),
          ],
        ],
        [2, 0],
      ]);
    const original = transaction(referenceBody(1));
    const signature = sign(
      null,
      Buffer.from(original.transactionId, 'hex'),
      keys.privateKey
    );
    const witnessSet = encodeVKeyWitnessSet([{ publicKey, signature }]);
    const existingWitnesses = new Map<number, unknown>([
      [0, [[publicKey, signature]]],
    ]);

    expect(
      diffVKeyWitnesses(original, original.transactionId, witnessSet)
    ).toEqual(witnessSet);
    const existing = transaction(referenceBody(1), existingWitnesses);
    expect(
      diffVKeyWitnesses(
        existing,
        existing.transactionId,
        encodeVKeyWitnessSet([])
      )
    ).toEqual(encodeVKeyWitnessSet([]));
    const mismatchedExisting = transaction(referenceBody(2), existingWitnesses);
    expect(() =>
      diffVKeyWitnesses(
        mismatchedExisting,
        mismatchedExisting.transactionId,
        encodeVKeyWitnessSet([])
      )
    ).toThrow();
    const mutatedBody = transaction(referenceBody(2));
    expect(() =>
      diffVKeyWitnesses(mutatedBody, mutatedBody.transactionId, witnessSet)
    ).toThrow();
    for (let index = 0; index < witnessSet.length; index += 1)
      expect(() =>
        diffVKeyWitnesses(
          original,
          original.transactionId,
          mutateByte(witnessSet, index)
        )
      ).toThrow();
  });

  it('rejects every mutated CIP-8 COSE_Sign1, COSE_Key, payload, and credential vector', () => {
    const expected = prepareCip8Request(
      cip8Fixture.drepId,
      cip8Fixture.payload,
      {
        networkId: 1,
        drepCredential: cip8Fixture.drepId,
      }
    );
    expect(verifyCip8BackendResponse(expected, cip8Response())).toBeDefined();

    const fields: ReadonlyArray<Readonly<{
      key: 'cose_sign1' | 'cose_key';
      hex: string;
    }>> = [
      { key: 'cose_sign1', hex: cip8Fixture.coseSign1 },
      { key: 'cose_key', hex: cip8Fixture.coseKey },
    ];
    fields.forEach(({ key, hex }) => {
      const source = Buffer.from(hex, 'hex');
      for (let index = 0; index < source.length; index += 1) {
        const candidate = mutateByte(source, index).toString('hex');
        expect(() =>
          verifyCip8BackendResponse(
            expected,
            cip8Response({ [key]: candidate })
          )
        ).toThrow();
      }
    });

    const changedPayload = prepareCip8Request(
      cip8Fixture.drepId,
      mutateByte(Buffer.from(cip8Fixture.payload, 'hex'), 0).toString('hex'),
      { networkId: 1, drepCredential: cip8Fixture.drepId }
    );
    expect(() =>
      verifyCip8BackendResponse(changedPayload, cip8Response())
    ).toThrow();
    expect(() =>
      verifyCip8BackendResponse(
        expected,
        cip8Response({
          credential: mutateByte(
            Buffer.from(cip8Fixture.drepId, 'hex'),
            0
          ).toString('hex'),
        })
      )
    ).toThrow();
  });
});
