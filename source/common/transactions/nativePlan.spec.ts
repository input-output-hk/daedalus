/** @jest-environment node */
import cbor from 'cbor';

import {
  createNativePlanReviewDisplay,
  nativeApprovalBindingDigest,
  nativeTransactionPlanDigest,
  parseNativePreparedApproval,
  parseNativeTransactionPlan,
} from './nativePlan';

const encode = (value: unknown): string =>
  cbor
    .encodeOne(value, { canonical: true, collapseBigIntegers: true })
    .toString('hex');
const address = Buffer.from(`60${'11'.repeat(28)}`, 'hex');
const policy = Buffer.alloc(28, 0x22);
const transactionId = Buffer.alloc(32, 0x33);
const genesisHash = '44'.repeat(32);
const inputCoin = BigInt('9007199254740993');
const fee = BigInt(200000);
const value = (coin: bigint) => [
  coin,
  [[policy, Buffer.from('aa', 'hex'), BigInt(2)]],
];
const item = (outputCoin = inputCoin - fee) => [
  [[transactionId, 0, address, value(inputCoin)]],
  [[address, value(outputCoin), true]],
  fee,
  [],
  [],
  null,
  BigInt(0),
  BigInt(0),
  [],
  null,
  null,
];
const plan = (transaction = item()) =>
  encode([
    'daedalus-native-review',
    1,
    'wallet-1',
    [0, 42, Buffer.from(genesisHash, 'hex')],
    'payment',
    Buffer.alloc(32, 0x55),
    [null, BigInt(12345)],
    [transaction],
    [BigInt(0), []],
  ]);

describe('native transaction plan', () => {
  it('round-trips exact amounts and derives the wallet delta', () => {
    const planCbor = plan();
    const parsed = parseNativeTransactionPlan(planCbor, {
      walletId: 'wallet-1',
      network: { networkId: 0, networkMagic: 42, genesisHash },
      action: 'payment',
    });
    expect(parsed.items[0].inputs[0].value.coin.toString()).toBe(
      inputCoin.toString()
    );
    expect(createNativePlanReviewDisplay(parsed).walletChange).toMatchObject({
      coin: `-${fee.toString()}`,
      assets: [],
    });
    expect(nativeTransactionPlanDigest(planCbor)).toMatch(/^[0-9a-f]{64}$/u);
  });

  it('rejects changed economics before presentation', () => {
    expect(() =>
      parseNativeTransactionPlan(plan(item(inputCoin - fee - BigInt(1))))
    ).toThrow('coin is not conserved');
  });

  it('binds the exact plan, wallet, network, action, and authorization', () => {
    const planCbor = plan();
    const prepared = parseNativePreparedApproval({
      walletId: 'wallet-1',
      network: { networkId: 0, networkMagic: 42, genesisHash },
      action: 'payment',
      authorization: 'software',
      items: [
        {
          kind: 'native-plan',
          planCbor,
          planDigest: nativeTransactionPlanDigest(planCbor),
        },
      ],
      context: { walletName: 'Savings', networkName: 'Preview' },
    });
    const digest = nativeApprovalBindingDigest(prepared);
    expect(digest).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      nativeApprovalBindingDigest({ ...prepared, authorization: 'ledger' })
    ).not.toBe(digest);
  });
});
