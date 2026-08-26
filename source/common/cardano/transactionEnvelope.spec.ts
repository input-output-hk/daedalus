import fs from 'fs';
import path from 'path';
import { blake2b } from 'blakejs';

import { bytesForSpan, CborParseError, parseCborItem } from './cborSlices';
import {
  parseConwayTransactionEnvelope,
  TransactionEnvelopeError,
} from './transactionEnvelope';

type Span = { start: number; end: number };
type Fixture = { id: string; file: string; cborHex: string };
type Expected = {
  id: string;
  file: string;
  expected: {
    body: Span;
    witnessSet: Span;
    isValid: Span;
    auxiliaryData: Span;
    outputs: Span[];
    collateralReturn: Span | null;
    bodyHashBlake2b256: string;
  };
};

const directory = path.join(__dirname, 'fixtures', 'exact-cbor');
const manifest = JSON.parse(
  fs.readFileSync(path.join(directory, 'manifest.json'), 'utf8')
) as { fixtures: Expected[] };
const fixture = (expected: Expected): Fixture =>
  JSON.parse(
    fs.readFileSync(path.join(directory, expected.file), 'utf8')
  ) as Fixture;
const raw = (expected: Expected): Buffer =>
  Buffer.from(fixture(expected).cborHex, 'hex');
const accepted = manifest.fixtures.filter(({ id }) => id.startsWith('conway-'));
const parse = (bytes: Buffer) => parseConwayTransactionEnvelope(bytes);
const reject = (bytes: Buffer) =>
  expect(() => parse(bytes)).toThrow(TransactionEnvelopeError);
const replace = (
  source: Buffer,
  start: number,
  end: number,
  value: Buffer
): Buffer =>
  Buffer.concat([source.subarray(0, start), value, source.subarray(end)]);

const minimal = (auxiliary = Buffer.from([0xf6])) =>
  Buffer.concat([
    Buffer.from('84a300800181a2004001000200a0f4', 'hex'),
    auxiliary,
  ]);

describe('Conway transaction envelope', () => {
  it.each(accepted)(
    'preserves frozen spans and true body hash for $id',
    (expected) => {
      const bytes = raw(expected);
      const envelope = parse(bytes);
      expect(envelope.cbor).toBe(bytes);
      expect(envelope.spans).toEqual({
        body: expected.expected.body,
        witnessSet: expected.expected.witnessSet,
        isValid: expected.expected.isValid,
        auxiliaryData: expected.expected.auxiliaryData,
        outputs: expected.expected.outputs,
        collateralReturn: expected.expected.collateralReturn || undefined,
      });
      expect(envelope.transactionId).toBe(expected.expected.bodyHashBlake2b256);
      expect(
        Buffer.from(
          blake2b(
            bytes.subarray(
              expected.expected.body.start,
              expected.expected.body.end
            ),
            undefined,
            32
          )
        ).toString('hex')
      ).toBe(envelope.transactionId);
      envelope.outputs.forEach((output, index) => {
        const slice = bytesForSpan(envelope.cbor, output.span);
        expect(slice).toEqual(
          bytes.subarray(
            expected.expected.outputs[index].start,
            expected.expected.outputs[index].end
          )
        );
        expect(parseCborItem(slice).span.end).toBe(slice.length);
      });
    }
  );

  it('retains tagged and untagged set representation and accepts an indefinite root', () => {
    const regression = raw(accepted[0]);
    const indefinite = Buffer.concat([
      Buffer.from([0x9f]),
      regression.subarray(1),
      Buffer.from([0xff]),
    ]);
    expect(parse(indefinite).spans.body).toEqual(accepted[0].expected.body);
    expect(parse(raw(accepted[2])).transactionId).toBe(
      accepted[2].expected.bodyHashBlake2b256
    );
    expect(() =>
      parse(Buffer.from('84a400800181a200400100020004d901028100a0f4f6', 'hex'))
    ).not.toThrow();
    expect(() =>
      parse(Buffer.from('84a400800181a200400100020014d901028100a0f4f6', 'hex'))
    ).not.toThrow();
    const wrongTag = Buffer.from(regression);
    const at = wrongTag.indexOf(Buffer.from('d90102', 'hex'));
    wrongTag[at + 2] = 0x03;
    reject(wrongTag);
    reject(Buffer.from('84a300d90102d90102800181a2004001000200a0f4f6', 'hex'));
    reject(Buffer.from('84a400d9010280018002000dd9010280a0f4f6', 'hex'));
  });

  it('rejects encoded and decoded integer duplicate map keys', () => {
    const source = raw(accepted[0]);
    const bodyEnd = accepted[0].expected.body.end;
    const duplicateFee = replace(
      replace(source, 1, 2, Buffer.from([0xa8])),
      bodyEnd,
      bodyEnd,
      Buffer.from([0x02, 0x00])
    );
    reject(duplicateFee);
    const decodedDuplicate = replace(
      replace(source, 1, 2, Buffer.from([0xa8])),
      bodyEnd,
      bodyEnd,
      Buffer.from([0x18, 0x00, 0x80])
    );
    reject(decodedDuplicate);
    expect(() => parseCborItem(Buffer.from('a200000001', 'hex'))).toThrow(
      CborParseError
    );
  });

  it('fails closed on trailing, malformed, and invalid outer data', () => {
    const source = raw(accepted[0]);
    reject(Buffer.concat([source, Buffer.from([0x00])]));
    [
      Buffer.from('84', 'hex'),
      Buffer.from('9f', 'hex'),
      Buffer.from('bf00', 'hex'),
      Buffer.from('5fff', 'hex'),
      Buffer.from('ff', 'hex'),
      Buffer.from('830000', 'hex'),
      Buffer.from('84a000f4f6', 'hex'),
      Buffer.from('83a0a0f4', 'hex'),
    ].forEach(reject);
    expect(() => parseCborItem(Buffer.from('5f5f40ffff', 'hex'))).toThrow(
      CborParseError
    );
  });

  it('enforces tag-24 embedded complete consumption and outer bool/auxiliary fields', () => {
    reject(
      Buffer.from('84a300800181a300400100028201d8184200000200a0f4f6', 'hex')
    );
    const falseEnvelope = minimal();
    const trueEnvelope = Buffer.from(falseEnvelope);
    trueEnvelope[trueEnvelope.length - 2] = 0xf5;
    expect(parse(trueEnvelope).isValid).toBe(true);
    expect(
      parse(minimal(Buffer.from('a0', 'hex'))).auxiliaryData.span
    ).toEqual({ start: falseEnvelope.length - 1, end: falseEnvelope.length });
    expect(
      parse(minimal(Buffer.from('d90103a0', 'hex'))).auxiliaryData.span.end
    ).toBe(minimal(Buffer.from('d90103a0', 'hex')).length);
    reject(Buffer.from('84a300800181a2004001000200a0f7f6', 'hex'));
    const alonzo = manifest.fixtures.find(
      ({ id }) => id === 'alonzo-ledger-golden-auxiliary-data'
    );
    if (!alonzo) throw new Error('missing frozen Alonzo evidence');
    reject(raw(alonzo));
  });

  it('offers bounded pure raw-buffer hooks for deterministic mutation', () => {
    let seed = 0x6d2b79f5;
    const mutate = (source: Buffer) => {
      const result = Buffer.from(source);
      seed = Math.imul(seed ^ (seed >>> 15), 1 | seed);
      if (result.length)
        result[(seed >>> 0) % result.length] ^= (seed >>> 8) & 0xff;
      return result;
    };
    [
      ...accepted.map(raw),
      Buffer.alloc(0),
      Buffer.from([0xff]),
      Buffer.alloc(65_537),
    ].forEach((source) => {
      for (let iteration = 0; iteration < 16; iteration += 1) {
        try {
          const envelope = parse(mutate(source));
          [
            envelope.root.span,
            envelope.body.span,
            envelope.witnessSet.span,
            envelope.spans.isValid,
            envelope.spans.auxiliaryData,
            ...envelope.spans.outputs,
          ].forEach(({ start, end }) => {
            expect(start).toBeGreaterThanOrEqual(0);
            expect(end).toBeLessThanOrEqual(envelope.cbor.length);
            expect(end).toBeGreaterThan(start);
          });
          expect(
            Buffer.from(
              blake2b(
                bytesForSpan(envelope.cbor, envelope.spans.body),
                undefined,
                32
              )
            ).toString('hex')
          ).toBe(envelope.transactionId);
        } catch (error) {
          if (!(error instanceof TransactionEnvelopeError)) throw error;
        }
      }
    });
  });
});
