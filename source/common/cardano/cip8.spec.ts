import fixture from '../cip30/contracts/fixtures/cip8-cip95-fixture.json';
import wireFixtures from '../cip30/contracts/fixtures/wire-fixtures.json';
import {
  Cip8BackendResponse,
  createCip8DataSignReview,
  parseCip8DataSignReview,
  serializeCip8,
  verifyCip8BackendResponse,
} from './cip8';
import {
  Cip8AddressNotPKError,
  Cip8Error,
  prepareCip8Request,
} from './cip8Request';
import {
  decodeCoseKey,
  decodeCoseSign1,
  encodeCoseKey,
  encodeCoseProtectedHeader,
  encodeCoseSignatureStructure,
} from './cose';

const response = (overrides: Partial<Cip8BackendResponse> = {}) => ({
  revision: 1 as const,
  credential_kind: 'drep' as const,
  credential: fixture.drepId,
  cose_sign1: fixture.coseSign1,
  cose_key: fixture.coseKey,
  ...overrides,
});

const drepRequest = () =>
  prepareCip8Request(fixture.drepId, fixture.payload, {
    networkId: 1,
    drepCredential: fixture.drepId,
  });

const address = (name: string) => {
  const item = wireFixtures.addresses.find(
    (candidate) => candidate.name === name
  );
  if (!item) throw new Error(`Missing address fixture: ${name}`);
  return item;
};

describe('CIP-8', () => {
  it('serializes the exact task-002 profile and verifies it independently', () => {
    const expected = drepRequest();
    expect(
      encodeCoseProtectedHeader(expected.protectedAddress).toString('hex')
    ).toBe(fixture.protectedHeader);
    expect(
      encodeCoseSignatureStructure(
        Buffer.from(fixture.protectedHeader, 'hex'),
        expected.payload
      ).toString('hex')
    ).toBe(fixture.sigStructure);

    const serialized = serializeCip8(expected, {
      publicKey: Buffer.from(fixture.publicKey, 'hex'),
      signature: Buffer.from(fixture.signature, 'hex'),
    });
    expect(serialized).toEqual({
      signature: fixture.coseSign1,
      key: fixture.coseKey,
    });
    expect(
      decodeCoseKey(Buffer.from(serialized.key, 'hex')).toString('hex')
    ).toBe(fixture.publicKey);
    expect(
      decodeCoseSign1(
        Buffer.from(serialized.signature, 'hex'),
        expected.protectedAddress,
        expected.payload
      ).protectedHeader.toString('hex')
    ).toBe(fixture.protectedHeader);
    expect(verifyCip8BackendResponse(expected, response())).toEqual(serialized);
  });

  it('accepts missing version only through the explicit legacy verifier option', () => {
    const expected = drepRequest();
    const legacy = response({
      cose_sign1: fixture.negativeCases.legacyMissingVersionVerificationOnly,
    });
    expect(() => verifyCip8BackendResponse(expected, legacy)).toThrow(
      Cip8Error
    );
    expect(
      verifyCip8BackendResponse(expected, legacy, {
        allowLegacyMissingVersion: true,
      })
    ).toEqual({ signature: legacy.cose_sign1, key: legacy.cose_key });
    expect(
      serializeCip8(expected, {
        publicKey: Buffer.from(fixture.publicKey, 'hex'),
        signature: Buffer.from(fixture.signature, 'hex'),
      }).signature
    ).toBe(fixture.coseSign1);
  });

  it('normalizes payment, stake, raw DRep, and matching type-6 inputs', () => {
    const matching = address('mainnet-enterprise-matching-drep');
    const nonmatching = address('mainnet-enterprise-nonmatching-drep');
    const base = address('mainnet-base-key-key');
    const reward = address('mainnet-reward-key');
    const options = { networkId: 1 as const, drepCredential: fixture.drepId };

    for (const input of [fixture.drepId, matching.raw, matching.bech32]) {
      const prepared = prepareCip8Request(input, fixture.payload, options);
      expect(prepared.credentialKind).toBe('drep');
      expect(prepared.credential.toString('hex')).toBe(fixture.drepId);
      expect(prepared.protectedAddress.toString('hex')).toBe(fixture.drepId);
      expect(
        parseCip8DataSignReview(createCip8DataSignReview(prepared))
          .credentialKind
      ).toBe('drep');
      expect(verifyCip8BackendResponse(prepared, response())).toEqual({
        signature: fixture.coseSign1,
        key: fixture.coseKey,
      });
    }

    expect(
      prepareCip8Request(nonmatching.raw, '', options).credentialKind
    ).toBe('payment');
    expect(prepareCip8Request(base.bech32, '', options)).toMatchObject({
      credentialKind: 'payment',
      address: base.raw,
    });
    expect(prepareCip8Request(reward.raw, '', options)).toMatchObject({
      credentialKind: 'stake',
      address: reward.raw,
    });
  });

  it('rejects malformed request hex and script credentials before producing a request', () => {
    const options = { networkId: 1 as const, drepCredential: fixture.drepId };
    for (const payload of [
      fixture.negativeCases.oddLengthHex,
      fixture.negativeCases.prefixedHex,
      fixture.negativeCases.malformedHex,
    ]) {
      expect(() =>
        prepareCip8Request(fixture.drepId, payload, options)
      ).toThrow(Cip8Error);
    }
    expect(() =>
      prepareCip8Request(fixture.negativeCases.oddLengthHex, '', options)
    ).toThrow(Cip8Error);
    expect(() =>
      prepareCip8Request(`71${fixture.drepId}`, '', options)
    ).toThrow(Cip8AddressNotPKError);
    expect(() =>
      prepareCip8Request(fixture.drepId, '', { networkId: 1 })
    ).toThrow(Cip8Error);
    expect(() =>
      prepareCip8Request(`00${fixture.drepId.slice(2)}`, '', options)
    ).toThrow(Cip8Error);
  });

  it('shows UTF-8 preview only for exact display-safe payload bytes', () => {
    const payment = address('mainnet-enterprise-matching-drep');
    const safe = prepareCip8Request(
      payment.raw,
      Buffer.from('Hello, Cardano', 'utf8').toString('hex'),
      { networkId: 1 }
    );
    expect(createCip8DataSignReview(safe)).toEqual({
      address: payment.raw,
      credentialKind: 'payment',
      payload: Buffer.from('Hello, Cardano', 'utf8').toString('hex'),
      utf8Preview: 'Hello, Cardano',
    });
    for (const payload of [
      Buffer.from('line\nbreak', 'utf8').toString('hex'),
      'ff',
      Buffer.from('\u202eunsafe', 'utf8').toString('hex'),
    ])
      expect(
        createCip8DataSignReview(
          prepareCip8Request(payment.raw, payload, { networkId: 1 })
        ).utf8Preview
      ).toBeNull();
    expect(() =>
      parseCip8DataSignReview({
        ...createCip8DataSignReview(safe),
        utf8Preview: 'Different bytes',
      })
    ).toThrow('Invalid CIP-8 data');
  });

  it('rejects each malformed or unbound backend result', () => {
    const expected = drepRequest();
    const changedSignature = `${fixture.coseSign1.slice(0, -2)}00`;
    const changedKey = encodeCoseKey(
      Buffer.from(`04${fixture.publicKey.slice(2)}`, 'hex')
    ).toString('hex');
    const invalidResponses: Cip8BackendResponse[] = [
      response({ credential_kind: 'payment' }),
      response({ credential: `00${fixture.drepId.slice(2)}` }),
      response({ cose_sign1: fixture.negativeCases.malformedCbor }),
      response({ cose_sign1: fixture.negativeCases.taggedCoseSign1 }),
      response({ cose_sign1: fixture.negativeCases.changedPayload }),
      response({ cose_sign1: changedSignature }),
      response({ cose_key: fixture.negativeCases.malformedCbor }),
      response({ cose_key: changedKey }),
      response({ cose_key: `0x${fixture.coseKey}` }),
    ];
    invalidResponses.forEach((candidate) => {
      expect(() => verifyCip8BackendResponse(expected, candidate)).toThrow(
        Cip8Error
      );
    });
  });
});
