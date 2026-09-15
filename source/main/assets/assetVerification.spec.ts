/**
 * Policy binding and key binding: the digest that ties a policy field to a
 * subject, the native script decoder, and the registry's own script evaluator.
 *
 * Fixtures are real mainnet registry entries, captured 2026-09-14.
 *
 * @jest-environment node
 */
import crypto from 'crypto';
import blake2b from 'blake2b';
import * as cbor from 'cbor';
import {
  assetKeyHash,
  attestationPayload,
  attestingKeyHashes,
  decodeNativeScript,
  evaluateNativeScript,
  isPropertyAttested,
  nativeScriptPolicyId,
  verifyAttestationSignature,
  verifyPolicyBinding,
  verifyRegistryProperty,
} from './assetVerification';
import type { NativeScript } from './assetVerification';
import type { RegistryProperty } from './assetRegistryClient';

// c76ef54…42544544, the worked example in the plan documents.
const BTED = {
  subject: 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544',
  policyId: 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9',
  policy:
    '820182018282051a0303eb448200581c39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f',
  publicKey: '5817526d712f71e33a31ac3429fb7ce70b3e17e727044d9a2a51493e7894ba48',
  keyHash: '39a1df51147b6de6689a4727846962fb6540c3a3c7859a1a79b9420f',
};

// atLeast 2 of [ timeBefore 600, sig, timeAfter 500 ].
const AT_LEAST = {
  subject:
    '84e7bef00924708ab746b79b94a3e3659244854c1acf9119c288e581436654657374436f696e',
  policy:
    '82018303028382051902588200581cc04cc33b367f233e6ef0f15b05e2225b1974f4980611fb5852f6d01e82041901f4',
  publicKey: '8f26099728b91992ba5a06d8d91152ea6bd9aa1d944334fa96a4541b583c2634',
};

// all of [ timeAfter 75846431, timeBefore 112500909, sig ]. The upper bound is
// a slot long past, which is the case an implementer checking the clock fails.
const BOTH_BOUNDS = {
  subject: 'a90d1702625ee4ebcee3b3649708cbcbb163f50db9663308acc9650e4d414e45',
  policy:
    '820182018382041a0485531f82051a06b4a0ad8200581c95a6c324ffdef13f0eeaa5abe324a08c330d468cf8011bdbdf950e53',
  publicKey: '647fd616cd2587ea4380004ddd1f2977eca7a4274ad91a7a07f6b12cedc0d3eb',
  keyHash: '95a6c324ffdef13f0eeaa5abe324a08c330d468cf8011bdbdf950e53',
};

const signedBy = (...publicKeys: Array<string>) =>
  publicKeys.map((publicKey) => ({ signature: 'ff', publicKey }));

const sig = (keyHash: string): NativeScript => ({ kind: 'sig', keyHash });
const hashes = (...values: Array<string>) => new Set(values);

describe('assetKeyHash', () => {
  it('takes blake2b-224 of a 32-byte public key', () => {
    expect(assetKeyHash(BTED.publicKey)).toBe(BTED.keyHash);
    expect(assetKeyHash(BOTH_BOUNDS.publicKey)).toBe(BOTH_BOUNDS.keyHash);
  });

  it('refuses a key that is not 64 hex characters rather than hashing a prefix', () => {
    expect(assetKeyHash(BTED.publicKey.slice(0, 62))).toBeNull();
    expect(assetKeyHash(`${BTED.publicKey}00`)).toBeNull();
    expect(assetKeyHash(`${BTED.publicKey.slice(0, 62)}zz`)).toBeNull();
    expect(assetKeyHash('')).toBeNull();
  });
});

describe('attestingKeyHashes', () => {
  it('leaves a malformed key out of the set instead of hashing it', () => {
    expect(attestingKeyHashes(signedBy(BTED.publicKey, 'not a key'))).toEqual(
      hashes(BTED.keyHash)
    );
  });

  it('is empty for an empty signature list', () => {
    expect(attestingKeyHashes([])).toEqual(new Set());
  });
});

describe('decodeNativeScript', () => {
  const decodePolicy = (policy: string) =>
    decodeNativeScript(
      cbor.decodeFirstSync(Buffer.from(policy.slice(4), 'hex'))
    );

  it('decodes the shapes the corpus contains', () => {
    expect(decodePolicy(BTED.policy)).toEqual({
      kind: 'all',
      scripts: [{ kind: 'timeBefore', slot: 50588484 }, sig(BTED.keyHash)],
    });
    expect(decodePolicy(AT_LEAST.policy)).toEqual({
      kind: 'atLeast',
      required: 2,
      scripts: [
        { kind: 'timeBefore', slot: 600 },
        sig('c04cc33b367f233e6ef0f15b05e2225b1974f4980611fb5852f6d01e'),
        { kind: 'timeAfter', slot: 500 },
      ],
    });
    expect(decodePolicy(BOTH_BOUNDS.policy)).toEqual({
      kind: 'all',
      scripts: [
        { kind: 'timeAfter', slot: 75846431 },
        { kind: 'timeBefore', slot: 112500909 },
        sig(BOTH_BOUNDS.keyHash),
      ],
    });
  });

  it('decodes an any-of, which the corpus does not contain', () => {
    const encoded = cbor.encode([2, [[0, Buffer.from(BTED.keyHash, 'hex')]]]);
    expect(decodeNativeScript(cbor.decodeFirstSync(encoded))).toEqual({
      kind: 'any',
      scripts: [sig(BTED.keyHash)],
    });
  });

  it('refuses a tag it does not know', () => {
    expect(decodeNativeScript([6, 1])).toBeNull();
  });

  it('refuses a key hash that is not 28 bytes', () => {
    expect(decodeNativeScript([0, Buffer.alloc(27)])).toBeNull();
    expect(decodeNativeScript([0, 'not bytes'])).toBeNull();
  });

  it('refuses a negative or non-integer at-least threshold', () => {
    expect(decodeNativeScript([3, -1, []])).toBeNull();
    expect(decodeNativeScript([3, 1.5, []])).toBeNull();
    expect(decodeNativeScript([3, '2', []])).toBeNull();
  });

  it('refuses a non-integer slot', () => {
    expect(decodeNativeScript([4, 'soon'])).toBeNull();
    expect(decodeNativeScript([5, 1.5])).toBeNull();
  });

  it('refuses a script nested past the depth cap', () => {
    let nested: unknown = [0, Buffer.from(BTED.keyHash, 'hex')];
    for (let index = 0; index < 40; index += 1) nested = [1, [nested]];
    expect(decodeNativeScript(nested)).toBeNull();
  });

  it('refuses a sub-script that does not decode, rather than dropping it', () => {
    expect(
      decodeNativeScript([
        1,
        [
          [0, Buffer.alloc(28)],
          [9, 9],
        ],
      ])
    ).toBeNull();
  });

  it('refuses a value that is not an array', () => {
    expect(decodeNativeScript(1)).toBeNull();
    expect(decodeNativeScript([])).toBeNull();
    expect(decodeNativeScript(null)).toBeNull();
  });
});

describe('evaluateNativeScript', () => {
  const A = 'aa'.repeat(28);
  const B = 'bb'.repeat(28);
  const C = 'cc'.repeat(28);

  it('satisfies a signature requirement only when the key hash is present', () => {
    expect(evaluateNativeScript(sig(A), hashes(A))).toBe(true);
    expect(evaluateNativeScript(sig(A), hashes(B))).toBe(false);
    expect(evaluateNativeScript(sig(A), new Set())).toBe(false);
  });

  it('requires every branch of an all-of', () => {
    const script: NativeScript = { kind: 'all', scripts: [sig(A), sig(B)] };
    expect(evaluateNativeScript(script, hashes(A, B))).toBe(true);
    expect(evaluateNativeScript(script, hashes(A))).toBe(false);
  });

  it('satisfies an empty all-of and refuses an empty any-of', () => {
    expect(evaluateNativeScript({ kind: 'all', scripts: [] }, new Set())).toBe(
      true
    );
    expect(evaluateNativeScript({ kind: 'any', scripts: [] }, new Set())).toBe(
      false
    );
  });

  it('requires one branch of an any-of', () => {
    const script: NativeScript = { kind: 'any', scripts: [sig(A), sig(B)] };
    expect(evaluateNativeScript(script, hashes(B))).toBe(true);
    expect(evaluateNativeScript(script, hashes(C))).toBe(false);
  });

  it('refuses an at-least of two satisfied by one key', () => {
    const script: NativeScript = {
      kind: 'atLeast',
      required: 2,
      scripts: [sig(A), sig(B), sig(C)],
    };
    expect(evaluateNativeScript(script, hashes(A))).toBe(false);
    expect(evaluateNativeScript(script, hashes(A, C))).toBe(true);
  });

  it('refuses an at-least whose threshold exceeds its branches', () => {
    expect(
      evaluateNativeScript(
        { kind: 'atLeast', required: 4, scripts: [sig(A), sig(B), sig(C)] },
        hashes(A, B, C)
      )
    ).toBe(false);
  });

  it('satisfies an at-least of zero with no keys', () => {
    expect(
      evaluateNativeScript(
        { kind: 'atLeast', required: 0, scripts: [sig(A)] },
        new Set()
      )
    ).toBe(true);
  });

  it('refuses a kind it does not know, which the decoder cannot produce', () => {
    // Unreachable through `decodeNativeScript`, which refuses an unknown tag.
    // The guard is what decides which way an unknown kind falls, and it falls
    // towards unsatisfied.
    expect(evaluateNativeScript({ kind: 'unheard-of' } as any, hashes())).toBe(
      false
    );
  });

  it('resolves through nesting', () => {
    const script: NativeScript = {
      kind: 'all',
      scripts: [sig(A), { kind: 'any', scripts: [sig(B), sig(C)] }],
    };
    expect(evaluateNativeScript(script, hashes(A, C))).toBe(true);
    expect(evaluateNativeScript(script, hashes(A))).toBe(false);
  });

  it('satisfies both time-lock forms with no keys at all', () => {
    expect(
      evaluateNativeScript({ kind: 'timeAfter', slot: 500 }, new Set())
    ).toBe(true);
    expect(
      evaluateNativeScript({ kind: 'timeBefore', slot: 600 }, new Set())
    ).toBe(true);
  });
});

describe('time locks and the clock', () => {
  afterEach(() => {
    jest.useRealTimers();
  });

  it.each([
    ['long after the policy expired', new Date('2038-01-01T00:00:00Z')],
    ['long before it was written', new Date('1970-01-02T00:00:00Z')],
  ])(
    'binds a policy whose upper bound is a slot in the past, %s',
    (_label, when) => {
      jest.useFakeTimers().setSystemTime(when);
      const result = verifyPolicyBinding(
        BOTH_BOUNDS.subject,
        BOTH_BOUNDS.policy,
        signedBy(BOTH_BOUNDS.publicKey)
      );
      expect(result).toMatchObject({ bound: true, satisfied: true });
    }
  );
});

describe('verifyPolicyBinding', () => {
  it('binds the worked example and reproduces its policy id', () => {
    const result = verifyPolicyBinding(
      BTED.subject,
      BTED.policy,
      signedBy(BTED.publicKey)
    );
    expect(result).toMatchObject({
      bound: true,
      satisfied: true,
      policyId: BTED.policyId,
    });
  });

  it('binds the same fixture written in upper case', () => {
    expect(
      verifyPolicyBinding(
        BTED.subject.toUpperCase(),
        BTED.policy.toUpperCase(),
        signedBy(BTED.publicKey)
      )
    ).toMatchObject({ bound: true, satisfied: true });
  });

  it('names both digests when the policy belongs to a different subject', () => {
    const result = verifyPolicyBinding(
      BOTH_BOUNDS.subject,
      BTED.policy,
      signedBy(BTED.publicKey)
    );
    expect(result).toEqual({
      bound: false,
      reason: 'digest-mismatch',
      expectedPolicyId: BOTH_BOUNDS.subject.slice(0, 56),
      actualPolicyId: BTED.policyId,
    });
    expect(result).not.toHaveProperty('satisfied');
  });

  it('reports an absent policy as absent rather than as a mismatch', () => {
    expect(verifyPolicyBinding(BTED.subject, null, [])).toEqual({
      bound: false,
      reason: 'absent',
    });
    expect(verifyPolicyBinding(BTED.subject, undefined, [])).toEqual({
      bound: false,
      reason: 'absent',
    });
    expect(verifyPolicyBinding(BTED.subject, '', [])).toEqual({
      bound: false,
      reason: 'absent',
    });
  });

  it('reports a malformed policy as malformed', () => {
    expect(verifyPolicyBinding(BTED.subject, 'zzzz', [])).toMatchObject({
      reason: 'malformed',
    });
    expect(verifyPolicyBinding(BTED.subject, '8201', [])).toMatchObject({
      reason: 'malformed',
    });
    expect(verifyPolicyBinding(BTED.subject, '820', [])).toMatchObject({
      reason: 'malformed',
    });
  });

  it('reports a subject too short to carry a policy id as malformed', () => {
    expect(verifyPolicyBinding('c76ef545', BTED.policy, [])).toEqual({
      bound: false,
      reason: 'malformed',
    });
    expect(verifyPolicyBinding('z'.repeat(64), BTED.policy, [])).toEqual({
      bound: false,
      reason: 'malformed',
    });
  });

  it('reports bytes that hash correctly but are not a script as not-a-script', () => {
    const bytes = cbor.encode(1);
    const subject = `${nativeScriptPolicyId(bytes)}42544544`;
    expect(
      verifyPolicyBinding(subject, `8201${bytes.toString('hex')}`, [])
    ).toEqual({ bound: false, reason: 'not-a-script' });
  });

  it('reports bytes that are not CBOR at all as not-a-script', () => {
    // A lone break byte hashes like anything else and fails at the decoder
    // rather than at the shape check, which is a different path from the case
    // above where the CBOR decodes to something that is not a script.
    const bytes = Buffer.from('ff', 'hex');
    const subject = `${nativeScriptPolicyId(bytes)}42544544`;
    expect(verifyPolicyBinding(subject, '8201ff', [])).toEqual({
      bound: false,
      reason: 'not-a-script',
    });
  });

  it('binds but does not satisfy when the signing key is not required', () => {
    const result = verifyPolicyBinding(
      BTED.subject,
      BTED.policy,
      signedBy(BOTH_BOUNDS.publicKey)
    );
    expect(result).toMatchObject({ bound: true, satisfied: false });
  });

  it('ignores a malformed key alongside a real one and fails on it alone', () => {
    expect(
      verifyPolicyBinding(
        BTED.subject,
        BTED.policy,
        signedBy(BTED.publicKey, 'not a key')
      )
    ).toMatchObject({ satisfied: true });
    expect(
      verifyPolicyBinding(BTED.subject, BTED.policy, signedBy('not a key'))
    ).toMatchObject({ satisfied: false });
  });

  it('satisfies an at-least whose time-lock branches alone meet the threshold', () => {
    // Pinned deliberately. The registry's own evaluator treats both time-lock
    // forms as satisfied, so this script needs no signature at all. One entry
    // in 405 policy-bearing mainnet subjects has this shape. Changing this
    // behaviour would diverge from the registry's definition of a valid
    // attestation, so it fails this test rather than changing quietly.
    expect(
      verifyPolicyBinding(AT_LEAST.subject, AT_LEAST.policy, [])
    ).toMatchObject({ bound: true, satisfied: true });
    expect(
      verifyPolicyBinding(
        AT_LEAST.subject,
        AT_LEAST.policy,
        signedBy(AT_LEAST.publicKey)
      )
    ).toMatchObject({ bound: true, satisfied: true });
  });
});

// The five signed properties of the worked example, captured from the live
// registry on 2026-09-14.
const BTED_PROPERTIES: Record<
  string,
  { value: unknown; sequenceNumber: number; signature: string }
> = {
  name: {
    value: 'BitEd Token',
    sequenceNumber: 0,
    signature:
      '9a5e7bb00e1b4e2c9d4d40d894ba6f019400ca2f4138f65d5d8bd7ef9c5a9143f837428c29c40af9757da139e37d8eced1973e9d029700c9d46073ab22476e09',
  },
  ticker: {
    value: 'BTED',
    sequenceNumber: 0,
    signature:
      '68a722e7aa51d7d36baae9cbaadbf7c632f41596e2ed4030a4bacd58643a81d6ed8cb6f79b585e63c58e9df6b2f88f5a9d66248ad0eb624c86709e0abe40cd0a',
  },
  url: {
    value: 'https://bit-ed.org/',
    sequenceNumber: 0,
    signature:
      '778698a32b1a8dbc9f4cf74dd26f5f6ca159d4460a2b687927573104ccfe4bde3267060cc1e4ceb696405d074fd97b69d6961b946ad8dcb6b467a6cf4fe34708',
  },
  description: {
    value:
      'The BitEd Token (BTED) is a governance token built on the Cardano Blockchain. BitEd an organization seeking to improve education for children globally. Charitable projects receiving donations will be voted on by token holders. BitEd will make a by-weekly donation and use the tokens to maximize donations to selected projects. No one project will receive multiple donations.',
    sequenceNumber: 0,
    signature:
      '2b478f17b2336f49d936d4556b1d854c635fe5afc55f9e279bbeb938d78ebe3985a62596050efad75f1ca4cfd3c681e2a2bfc4aa6984793cc8486ec855cb9b04',
  },
  decimals: {
    value: 0,
    sequenceNumber: 0,
    signature:
      '622bf53a1ba2891cf75c6b44394cad40597213a854de0d2dbcc0d1cd31767776afdcb069b744bf54dec4e3f3486d1ba5872dae07ead7288900acc5b684b0b10b',
  },
};

const propertyOf = (
  name: string,
  overrides: Partial<RegistryProperty> = {}
): RegistryProperty => {
  const source = BTED_PROPERTIES[name];
  return {
    value: source.value,
    sequenceNumber: source.sequenceNumber,
    signatures: [{ signature: source.signature, publicKey: BTED.publicKey }],
    ...overrides,
  };
};

const hash32 = (bytes: Uint8Array): Buffer => {
  const input = new Uint8Array(bytes.length);
  input.set(bytes);
  return Buffer.from(blake2b(32).update(input).digest());
};

// The ed25519 group order. Adding it to S leaves a signature that is
// arithmetically equivalent and non-canonical.
const GROUP_ORDER = BigInt(
  '7237005577332262213973186563042994240857116359379907606001950938285454250989'
);

const withScalarPlusOrder = (signature: string): string | null => {
  const bytes = Buffer.from(signature, 'hex');
  let scalar = BigInt(0);
  for (let index = 31; index >= 0; index -= 1) {
    scalar = (scalar << BigInt(8)) | BigInt(bytes[32 + index]);
  }
  const shifted = scalar + GROUP_ORDER;
  if (shifted >= BigInt(1) << BigInt(256)) return null;
  const out = Buffer.alloc(32);
  let remaining = shifted;
  for (let index = 0; index < 32; index += 1) {
    out[index] = Number(remaining & BigInt(0xff));
    remaining >>= BigInt(8);
  }
  return Buffer.concat([bytes.subarray(0, 32), out]).toString('hex');
};

describe('attestationPayload', () => {
  it.each(Object.keys(BTED_PROPERTIES))(
    'produces a payload the live %s signature verifies',
    (name) => {
      const { value, sequenceNumber, signature } = BTED_PROPERTIES[name];
      const payload = attestationPayload(
        BTED.subject,
        name,
        value,
        sequenceNumber
      );
      expect(payload).toHaveLength(32);
      expect(
        verifyAttestationSignature(payload, signature, BTED.publicKey)
      ).toBe(true);
    }
  );

  it('hashes the subject and the property name as CBOR text, not as raw UTF-8', () => {
    const real = attestationPayload(BTED.subject, 'ticker', 'BTED', 0);
    const rawSubject = hash32(
      Buffer.concat([
        hash32(Buffer.from(BTED.subject, 'utf8')),
        hash32(cbor.encode('ticker')),
        hash32(cbor.encode('BTED')),
        hash32(cbor.encode(0)),
      ])
    );
    const rawName = hash32(
      Buffer.concat([
        hash32(cbor.encode(BTED.subject)),
        hash32(Buffer.from('ticker', 'utf8')),
        hash32(cbor.encode('BTED')),
        hash32(cbor.encode(0)),
      ])
    );
    expect(real.equals(rawSubject)).toBe(false);
    expect(real.equals(rawName)).toBe(false);
  });

  it('signs a logo over the decoded bytes rather than the base64 text', () => {
    const png = Buffer.from('89504e470d0a1a0a0000000d49484452', 'hex');
    const base64 = png.toString('base64');
    const decodedForm = hash32(
      Buffer.concat([
        hash32(cbor.encode(BTED.subject)),
        hash32(cbor.encode('logo')),
        hash32(cbor.encode(png)),
        hash32(cbor.encode(0)),
      ])
    );
    const textForm = hash32(
      Buffer.concat([
        hash32(cbor.encode(BTED.subject)),
        hash32(cbor.encode('logo')),
        hash32(cbor.encode(base64)),
        hash32(cbor.encode(0)),
      ])
    );
    const payload = attestationPayload(BTED.subject, 'logo', base64, 0);
    expect(payload.equals(decodedForm)).toBe(true);
    expect(payload.equals(textForm)).toBe(false);
  });

  it('encodes every other property from the value as it arrives', () => {
    const payload = attestationPayload(BTED.subject, 'ticker', 'BTED', 0);
    expect(
      payload.equals(
        hash32(
          Buffer.concat([
            hash32(cbor.encode(BTED.subject)),
            hash32(cbor.encode('ticker')),
            hash32(cbor.encode('BTED')),
            hash32(cbor.encode(0)),
          ])
        )
      )
    ).toBe(true);
  });

  it('refuses a logo value that is not a string', () => {
    expect(attestationPayload(BTED.subject, 'logo', 42, 0)).toBeNull();
  });

  it('refuses a value CBOR cannot encode rather than throwing', () => {
    // The registry publishes JSON, so nothing it sends can be a function; the
    // guard is for a caller handing over something the encoder refuses.
    expect(
      attestationPayload(BTED.subject, 'ticker', () => 'BTED', 0)
    ).toBeNull();
  });

  it('refuses a subject or a property name that is not a string', () => {
    expect(attestationPayload(null as any, 'ticker', 'BTED', 0)).toBeNull();
    expect(attestationPayload(BTED.subject, null as any, 'BTED', 0)).toBeNull();
  });

  it('refuses a sequence number that is not an integer', () => {
    expect(attestationPayload(BTED.subject, 'ticker', 'BTED', 1.5)).toBeNull();
    expect(
      attestationPayload(BTED.subject, 'ticker', 'BTED', Number.NaN)
    ).toBeNull();
  });
});

describe('verifyAttestationSignature', () => {
  const payloadFor = (name: string) =>
    attestationPayload(
      BTED.subject,
      name,
      BTED_PROPERTIES[name].value,
      BTED_PROPERTIES[name].sequenceNumber
    );

  it('rejects a signature whose scalar has had the group order added to it', () => {
    const real = BTED_PROPERTIES.decimals.signature;
    const malleable = withScalarPlusOrder(real);
    // If S + L had overflowed, the case would compare a signature with itself
    // and pass without asserting anything.
    expect(malleable).not.toBeNull();
    expect(malleable).not.toBe(real);
    expect(
      verifyAttestationSignature(payloadFor('decimals'), real, BTED.publicKey)
    ).toBe(true);
    expect(
      verifyAttestationSignature(
        payloadFor('decimals'),
        malleable,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects a tampered value', () => {
    const payload = attestationPayload(BTED.subject, 'name', 'BitEd Tokeo', 0);
    expect(
      verifyAttestationSignature(
        payload,
        BTED_PROPERTIES.name.signature,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects a tampered sequence number', () => {
    const payload = attestationPayload(BTED.subject, 'name', 'BitEd Token', 1);
    expect(
      verifyAttestationSignature(
        payload,
        BTED_PROPERTIES.name.signature,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects a signature valid for a different property of the same subject', () => {
    expect(
      verifyAttestationSignature(
        payloadFor('name'),
        BTED_PROPERTIES.ticker.signature,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects a signature valid for the same property of a different subject', () => {
    const payload = attestationPayload(
      BOTH_BOUNDS.subject,
      'name',
      'BitEd Token',
      0
    );
    expect(
      verifyAttestationSignature(
        payload,
        BTED_PROPERTIES.name.signature,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects a payload that is not bytes rather than throwing', () => {
    // `attestationPayload` returns null for an input it cannot encode, and a
    // caller that passes that on reaches the verifier with nothing to verify.
    expect(
      verifyAttestationSignature(
        null as any,
        BTED_PROPERTIES.name.signature,
        BTED.publicKey
      )
    ).toBe(false);
  });

  it('rejects malformed inputs without throwing', () => {
    const payload = payloadFor('name');
    const real = BTED_PROPERTIES.name.signature;
    expect(
      verifyAttestationSignature(payload, real.slice(0, 126), BTED.publicKey)
    ).toBe(false);
    expect(
      verifyAttestationSignature(
        payload,
        `${real.slice(0, 126)}zz`,
        BTED.publicKey
      )
    ).toBe(false);
    expect(
      verifyAttestationSignature(payload, real, BTED.publicKey.slice(0, 62))
    ).toBe(false);
    expect(
      verifyAttestationSignature(payload, 'ff'.repeat(64), BTED.publicKey)
    ).toBe(false);
    expect(verifyAttestationSignature(payload, real, 'ff'.repeat(32))).toBe(
      false
    );
  });
});

describe('isPropertyAttested', () => {
  it('attests a property whose signature verifies', () => {
    expect(isPropertyAttested(BTED.subject, 'name', propertyOf('name'))).toBe(
      true
    );
  });

  it('attests when one of two signatures verifies', () => {
    expect(
      isPropertyAttested(
        BTED.subject,
        'name',
        propertyOf('name', {
          signatures: [
            { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
            {
              signature: BTED_PROPERTIES.name.signature,
              publicKey: BTED.publicKey,
            },
          ],
        })
      )
    ).toBe(true);
  });

  it('does not attest when only a junk signature is present', () => {
    expect(
      isPropertyAttested(
        BTED.subject,
        'name',
        propertyOf('name', {
          signatures: [
            { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
          ],
        })
      )
    ).toBe(false);
  });

  it('does not attest a property with no signatures', () => {
    expect(
      isPropertyAttested(
        BTED.subject,
        'name',
        propertyOf('name', { signatures: [] })
      )
    ).toBe(false);
  });
});

describe('verifyRegistryProperty', () => {
  it('returns all three steps true for the worked example', () => {
    expect(
      verifyRegistryProperty(
        BTED.subject,
        BTED.policy,
        'decimals',
        propertyOf('decimals')
      )
    ).toEqual({
      bound: true,
      satisfied: true,
      attested: true,
      verified: true,
    });
  });

  it('reports an unbound policy without claiming the signature went unchecked', () => {
    expect(
      verifyRegistryProperty(BTED.subject, null, 'name', propertyOf('name'))
    ).toEqual({
      bound: false,
      satisfied: false,
      attested: true,
      verified: false,
    });
  });

  // The middle class the plan documents, a genuine signature from a key set the
  // policy does not satisfy, cannot be taken from the registry: it would need a
  // second policy hashing to the same policy id. It is built here instead, with
  // a freshly generated key that actually signs the payload.
  const synthetic = (requiredKeyHash?: string) => {
    const { publicKey, privateKey } = crypto.generateKeyPairSync('ed25519');
    const rawPublicKey = publicKey
      .export({ format: 'der', type: 'spki' })
      .subarray(-32);
    const signerKeyHash = assetKeyHash(rawPublicKey.toString('hex'));
    const scriptBytes = cbor.encode([
      0,
      Buffer.from(requiredKeyHash ?? signerKeyHash, 'hex'),
    ]);
    const subject = `${nativeScriptPolicyId(scriptBytes)}42544544`;
    const payload = attestationPayload(subject, 'ticker', 'TEST', 0);
    return {
      subject,
      policy: `8201${scriptBytes.toString('hex')}`,
      property: {
        value: 'TEST',
        sequenceNumber: 0,
        signatures: [
          {
            signature: crypto.sign(null, payload, privateKey).toString('hex'),
            publicKey: rawPublicKey.toString('hex'),
          },
        ],
      },
    };
  };

  it('verifies a freshly signed property whose key the script requires', () => {
    const { subject, policy, property } = synthetic();
    expect(verifyRegistryProperty(subject, policy, 'ticker', property)).toEqual(
      {
        bound: true,
        satisfied: true,
        attested: true,
        verified: true,
      }
    );
  });

  it('refuses a genuine signature from a key the script does not require', () => {
    const { subject, policy, property } = synthetic('bb'.repeat(28));
    expect(verifyRegistryProperty(subject, policy, 'ticker', property)).toEqual(
      {
        bound: true,
        satisfied: false,
        attested: true,
        verified: false,
      }
    );
  });

  it('reports a tampered signature as bound and satisfied but not attested', () => {
    expect(
      verifyRegistryProperty(
        BTED.subject,
        BTED.policy,
        'name',
        propertyOf('name', {
          signatures: [
            { signature: 'ff'.repeat(64), publicKey: BTED.publicKey },
          ],
        })
      )
    ).toEqual({
      bound: true,
      satisfied: true,
      attested: false,
      verified: false,
    });
  });
});
