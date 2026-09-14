/**
 * Policy binding and key binding: the digest that ties a policy field to a
 * subject, the native script decoder, and the registry's own script evaluator.
 *
 * Fixtures are real mainnet registry entries, captured 2026-09-14.
 *
 * @jest-environment node
 */
import * as cbor from 'cbor';
import {
  assetKeyHash,
  attestingKeyHashes,
  decodeNativeScript,
  evaluateNativeScript,
  nativeScriptPolicyId,
  verifyPolicyBinding,
} from './assetVerification';
import type { NativeScript } from './assetVerification';

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
