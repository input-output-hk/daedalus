import { bytesForSpan, parseCborItem } from '../cardano/cborSlices';
import { parseConwayTransactionEnvelope } from '../cardano/transactionEnvelope';
import {
  extractEnvelopeVKeyWitnesses,
  verifyVKeyWitnesses,
} from '../cardano/witnessSet';

const fail = (): never => {
  throw new Error('Signed transaction changed the approved transaction');
};
const immutableWitnesses = (cbor: Buffer): readonly string[] => {
  const root = parseCborItem(cbor);
  if (root.major !== 5 || !root.entries || root.span.end !== cbor.length)
    fail();
  return root.entries
    .filter(({ key }) => key.major !== 0 || key.value !== BigInt(0))
    .map(({ key, value }) =>
      cbor.subarray(key.span.start, value.span.end).toString('hex')
    )
    .sort();
};

export const verifySignedNativeTransaction = (
  unsignedCbor: string,
  signedCbor: string
): string => {
  const unsigned = parseConwayTransactionEnvelope(
    Buffer.from(unsignedCbor, 'hex')
  );
  const signed = parseConwayTransactionEnvelope(Buffer.from(signedCbor, 'hex'));
  for (const field of ['body', 'isValid', 'auxiliaryData'] as const) {
    if (
      !bytesForSpan(unsigned.cbor, unsigned.spans[field]).equals(
        bytesForSpan(signed.cbor, signed.spans[field])
      )
    )
      fail();
  }
  const unsignedWitnesses = bytesForSpan(
    unsigned.cbor,
    unsigned.spans.witnessSet
  );
  const signedWitnesses = bytesForSpan(signed.cbor, signed.spans.witnessSet);
  if (
    immutableWitnesses(unsignedWitnesses).join(',') !==
    immutableWitnesses(signedWitnesses).join(',')
  )
    fail();
  const originalKeys = new Set(
    extractEnvelopeVKeyWitnesses(unsigned).map(({ publicKey }) =>
      publicKey.toString('hex')
    )
  );
  const returned = extractEnvelopeVKeyWitnesses(signed);
  if (
    [...originalKeys].some(
      (publicKey) =>
        !returned.some(
          (witness) => witness.publicKey.toString('hex') === publicKey
        )
    )
  )
    fail();
  verifyVKeyWitnesses(bytesForSpan(signed.cbor, signed.spans.body), returned);
  return signedCbor;
};
