import fixture from '../../../common/cip30/contracts/fixtures/cip8-cip95-fixture.json';
import wireFixtures from '../../../common/cip30/contracts/fixtures/wire-fixtures.json';
import type {
  ContextOwnership,
  DappNetwork,
} from '../../../common/cardano/transactionContext';

import { prepareHardwareMessage } from './hardwareWalletMessage';

const harden = (value: number): number => value + 0x80000000;
const paymentPath = [harden(1852), harden(1815), harden(0), 0, 0];
const stakePath = [harden(1852), harden(1815), harden(0), 2, 0];
const drepPath = [harden(1852), harden(1815), harden(0), 3, 0];
const network = (networkId: 0 | 1): DappNetwork => ({
  networkId,
  networkMagic: networkId ? 764824073 : 1,
  genesisHash: '00'.repeat(32),
});
const owned = (
  credentialKind: ContextOwnership['credentialKind'],
  credential: string,
  derivationPath: readonly number[]
): ContextOwnership => ({
  credentialKind,
  credential,
  ownership: 'owned_key',
  derivationPath,
  proofKinds: [],
});
const address = (name: string) => {
  const value = wireFixtures.addresses.find(
    (candidate) => candidate.name === name
  );
  if (!value) throw new Error(`Missing address fixture ${name}`);
  return value.raw;
};

const paymentCredential = fixture.drepId;
const stakeCredential =
  '00112233445566778899aabbccddeeff00112233445566778899aabb';
const ownership = [
  owned('payment', paymentCredential, paymentPath),
  owned('stake', stakeCredential, stakePath),
  owned('drep', fixture.drepId, drepPath),
];

describe('prepareHardwareMessage', () => {
  it('binds payment and stake addresses to their exact paths', () => {
    expect(
      prepareHardwareMessage(
        address('mainnet-base-key-key'),
        fixture.payload,
        network(1),
        ownership
      )
    ).toMatchObject({
      credentialKind: 'payment',
      path: paymentPath,
      address: {
        kind: 'address',
        addressType: 0,
        paymentPath,
        stakePath,
      },
    });
    expect(
      prepareHardwareMessage(
        address('mainnet-reward-key'),
        fixture.payload,
        network(1),
        ownership
      )
    ).toMatchObject({
      credentialKind: 'stake',
      path: stakePath,
      address: { kind: 'address', addressType: 14, stakePath },
    });
    expect(
      prepareHardwareMessage(
        address('testnet-pointer-key'),
        fixture.payload,
        network(0),
        [owned('payment', paymentCredential, paymentPath)]
      )
    ).toMatchObject({
      address: {
        kind: 'address',
        addressType: 4,
        paymentPath,
        pointer: { blockIndex: 0, txIndex: 0, certificateIndex: 0 },
      },
    });
  });

  it('normalizes both DRep inputs to the same key-hash request', () => {
    const direct = prepareHardwareMessage(
      fixture.drepId,
      fixture.payload,
      network(1),
      ownership,
      fixture.drepId
    );
    const type6 = prepareHardwareMessage(
      fixture.matchingEnterpriseAddress,
      fixture.payload,
      network(1),
      ownership,
      fixture.drepId
    );
    expect(direct).toMatchObject({
      credentialKind: 'drep',
      credential: fixture.drepId,
      protectedAddress: fixture.drepId,
      path: drepPath,
      address: { kind: 'key_hash', value: fixture.drepId },
    });
    expect(type6).toEqual(direct);
  });

  it('preserves the payload limit and rejects untrusted paths before devices', () => {
    const boundary = prepareHardwareMessage(
      address('mainnet-enterprise-matching-drep'),
      '00'.repeat(65_536),
      network(1),
      [owned('payment', paymentCredential, paymentPath)]
    );
    expect(boundary.payload).toHaveLength(131_072);
    expect(() =>
      prepareHardwareMessage(
        address('mainnet-enterprise-matching-drep'),
        '00'.repeat(65_537),
        network(1),
        [owned('payment', paymentCredential, paymentPath)]
      )
    ).toThrow('Invalid CIP-8 data signature');
    expect(() =>
      prepareHardwareMessage(
        address('mainnet-enterprise-matching-drep'),
        fixture.payload,
        network(1),
        [owned('payment', paymentCredential, [...paymentPath, 1])]
      )
    ).toThrow('Invalid hardware message request');
  });
});
