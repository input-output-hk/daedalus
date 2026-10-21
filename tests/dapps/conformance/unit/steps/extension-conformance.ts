import { createHash } from 'crypto';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { Given, Then, When } from 'cucumber';
import { expect } from 'chai';
import manifest from '../../../../../source/common/cip30/contracts/contract-manifest.json';
import cip8Fixture from '../../../../../source/common/cip30/contracts/fixtures/cip8-cip95-fixture.json';
import cip103Fixtures from '../../../../../source/common/cip30/contracts/fixtures/cip103-fixtures.json';
import observations from '../../fixtures/client-adapters.json';
import { captureCip103Submission, invokeCip95Adapter } from '../../adapters';
import { reconstructPublicRejection } from '../../../../../source/common/cip30/errors';
import type { DappCip30Rejection } from '../../../../../source/common/cip30/errors';
import {
  parseDappCip30GatewayRequest,
  parseDappCip30ResultEnvelope,
} from '../../../../../source/common/cip30/schemas';
import { DAPP_CIP30_METHODS } from '../../../../../source/common/cip30/wire';
import type { DappCip30Method } from '../../../../../source/common/cip30/wire';

// Independent normative examples, not generated from the manifest under test.
const hash = 'ab'.repeat(32);
const signature = { signature: 'a0', key: 'a0' };
const wireCases: Array<{
  method: DappCip30Method;
  args: unknown[];
  result: unknown;
  minimum: number;
  errors: DappCip30Rejection['type'][];
}> = [
  {
    method: 'provider.isEnabled',
    args: [],
    result: false,
    minimum: 0,
    errors: [],
  },
  {
    method: 'provider.enable',
    args: [{ extensions: [{ cip: 95 }] }],
    result: { extensions: [{ cip: 95 }] },
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.getExtensions',
    args: [],
    result: [{ cip: 95 }],
    minimum: 0,
    errors: [],
  },
  { method: 'api.getNetworkId', args: [], result: 0, minimum: 0, errors: [] },
  {
    method: 'api.getUtxos',
    args: ['a0', { page: 0, limit: 100 }],
    result: null,
    minimum: 0,
    errors: ['paginate-error'],
  },
  {
    method: 'api.getCollateral',
    args: [{ amount: 'a0' }],
    result: null,
    minimum: 0,
    errors: [],
  },
  { method: 'api.getBalance', args: [], result: 'a0', minimum: 0, errors: [] },
  {
    method: 'api.getUsedAddresses',
    args: [{ page: 0, limit: 100 }],
    result: ['00'],
    minimum: 0,
    errors: ['paginate-error'],
  },
  {
    method: 'api.getUnusedAddresses',
    args: [],
    result: ['00'],
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.getChangeAddress',
    args: [],
    result: '00',
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.getRewardAddresses',
    args: [],
    result: ['00'],
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.signTx',
    args: ['a0', true],
    result: 'a0',
    minimum: 1,
    errors: ['tx-sign-error'],
  },
  {
    method: 'api.signData',
    args: ['00', ''],
    result: signature,
    minimum: 2,
    errors: ['data-sign-error'],
  },
  {
    method: 'api.submitTx',
    args: ['a0'],
    result: hash,
    minimum: 1,
    errors: ['tx-send-error'],
  },
  {
    method: 'api.cip95.getPubDRepKey',
    args: [],
    result: hash,
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.cip95.getRegisteredPubStakeKeys',
    args: [],
    result: [hash],
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.cip95.getUnregisteredPubStakeKeys',
    args: [],
    result: [hash],
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.cip95.signData',
    args: ['00', ''],
    result: signature,
    minimum: 2,
    errors: ['data-sign-error'],
  },
  {
    method: 'api.cip103.signTxs',
    args: [[{ cbor: 'a0', partialSign: true }]],
    result: ['a0'],
    minimum: 1,
    errors: ['tx-sign-error'],
  },
  {
    method: 'api.cip103.submitTxs',
    args: [['a0']],
    result: [hash],
    minimum: 1,
    errors: ['tx-send-error', 'cip103-submit-error'],
  },
  {
    method: 'api.cip104.getAccountPub',
    args: [],
    result: `5840${'ab'.repeat(64)}`,
    minimum: 0,
    errors: [],
  },
  {
    method: 'api.cip142.getNetworkMagic',
    args: [],
    result: 4294967295,
    minimum: 0,
    errors: [],
  },
];

const rejectionCases: DappCip30Rejection[] = [
  { type: 'api-error', value: { code: -3, info: 'Refused' } },
  { type: 'paginate-error', value: { maxSize: 3 } },
  { type: 'tx-sign-error', value: { code: 2, info: 'Declined' } },
  { type: 'data-sign-error', value: { code: 3, info: 'Declined' } },
  { type: 'tx-send-error', value: { code: 1, info: 'Refused' } },
  { type: 'cip103-submit-error', value: [hash, { code: 2, info: 'Failed' }] },
];

const expectInvalidRequest = (request: unknown): void => {
  try {
    parseDappCip30GatewayRequest(request);
  } catch (rejection) {
    expect(rejection).to.deep.equal({ code: -1, info: 'Invalid request' });
    return;
  }
  expect.fail('Expected APIError.InvalidRequest');
};

const CIP95_METHODS = [
  'api.cip95.getPubDRepKey',
  'api.cip95.getRegisteredPubStakeKeys',
  'api.cip95.getUnregisteredPubStakeKeys',
  'api.cip95.signData',
];

const fixturePaths = {
  // The manifest hash in observations identifies the historical contract only.
  // These unchanged byte fixtures are still the evidence used by the adapters.
  'cip8-cip95-fixture.json':
    'source/common/cip30/contracts/fixtures/cip8-cip95-fixture.json',
  'cip103-fixtures.json':
    'source/common/cip30/contracts/fixtures/cip103-fixtures.json',
};

const digest = (path: string): string =>
  createHash('sha256')
    .update(readFileSync(resolve(path)))
    .digest('hex');

Given('the recorded CIP client observations', async function () {
  this.context.observations = observations;
});

Then(
  'current public methods and wire behavior satisfy the normative contract',
  async function () {
    const paths = wireCases.map(({ method }) => method);
    expect(new Set(manifest.methods.map(({ path }) => path))).to.deep.equal(
      new Set(paths)
    );
    expect(new Set(DAPP_CIP30_METHODS)).to.deep.equal(new Set(paths));
    expect(manifest.providerPath).to.equal('window.cardano.daedalus');

    for (const { method, args, result, minimum, errors } of wireCases) {
      for (let length = minimum; length <= args.length; length += 1) {
        const request = { method, args: args.slice(0, length) };
        expect(parseDappCip30GatewayRequest(request)).to.deep.equal(request);
      }
      if (minimum > 0) {
        expectInvalidRequest({ method, args: args.slice(0, minimum - 1) });
      }
      expectInvalidRequest({ method, args: [...args, undefined] });

      const fulfilled = { status: 'fulfilled', value: result };
      expect(parseDappCip30ResultEnvelope(method, fulfilled)).to.deep.equal(
        fulfilled
      );
      expect(() =>
        parseDappCip30ResultEnvelope(method, { status: 'fulfilled', value: {} })
      ).to.throw('Invalid CIP-30 result value');

      for (const rejection of rejectionCases) {
        const envelope = { status: 'rejected', rejection };
        if (rejection.type === 'api-error' || errors.includes(rejection.type)) {
          expect(parseDappCip30ResultEnvelope(method, envelope)).to.deep.equal(
            envelope
          );
          expect(reconstructPublicRejection(rejection)).to.deep.equal(
            rejection.value
          );
          expect(reconstructPublicRejection(rejection)).not.to.be.instanceOf(
            Error
          );
        } else {
          expect(() => parseDappCip30ResultEnvelope(method, envelope)).to.throw(
            'Invalid CIP-30 rejection for method'
          );
        }
      }
    }
  }
);

Then('current enable and collateral semantics are enforced', async function () {
  for (const args of [[], [undefined]]) {
    expect(
      parseDappCip30GatewayRequest({ method: 'api.getCollateral', args })
    ).to.deep.equal({ method: 'api.getCollateral', args });
  }
  for (const value of [{}, { extensions: [{ cip: 95 }], extra: true }]) {
    expect(() =>
      parseDappCip30ResultEnvelope('provider.enable', {
        status: 'fulfilled',
        value,
      })
    ).to.throw('Invalid CIP-30 result value');
  }
  for (const request of [
    { method: 'api.getCollateral', args: [{}] },
    { method: 'api.getUtxos', args: [undefined, { page: 0, limit: 101 }] },
    { method: 'api.cip103.signTxs', args: [[]] },
    { method: 'api.cip103.submitTxs', args: [Array(51).fill('a0')] },
    { method: 'api.signData', args: ['00', '0x00'] },
  ]) {
    expectInvalidRequest(request);
  }
});

Then(
  'recorded adapter fixtures retain their historical byte hashes',
  async function () {
    for (const [name, path] of Object.entries(fixturePaths)) {
      expect(digest(path)).to.equal(observations.fixtureHashes[name]);
    }
  }
);

Then(
  'recorded standard revisions retain their historical provenance',
  async function () {
    expect(observations.observedAt).to.equal('2026-08-31');
    expect(
      observations.standards.map(({ name, revision }) => [name, revision])
    ).to.deep.equal([
      ['CIP-30', '86b89208d3b2aabb5dcc5b778dfbe09096b4e114'],
      ['CIP-95', '20c819b25abee6551a3ef51778b975e7463e1269'],
      ['CIP-103', '86b89208d3b2aabb5dcc5b778dfbe09096b4e114'],
      ['CIP-104', '86b89208d3b2aabb5dcc5b778dfbe09096b4e114'],
      ['CIP-142', '86b89208d3b2aabb5dcc5b778dfbe09096b4e114'],
    ]);
    expect(observations.upstreamDeltas).to.deep.equal([]);
  }
);

Then('unavailable live wallets are not reported as passing', async function () {
  expect(observations.liveWallets).to.have.length(5);
  expect(
    observations.liveWallets.every(
      ({ availability }) => availability === 'unavailable'
    )
  ).to.equal(true);
});

Given('the source-verified Cardano JS SDK CIP-95 adapter', async function () {
  const adapter = observations.adapters.find(
    ({ id }) => id === 'cardano-js-sdk-cip95'
  );
  expect(adapter?.availability).to.equal('source-verified');
  this.context.calls = [];
});

When('the adapter invokes the CIP-95 namespace', async function () {
  const calls: string[] = this.context.calls;
  const api = {
    cip95: {
      getPubDRepKey: async () => calls.push(CIP95_METHODS[0]),
      getRegisteredPubStakeKeys: async () => calls.push(CIP95_METHODS[1]),
      getUnregisteredPubStakeKeys: async () => calls.push(CIP95_METHODS[2]),
      signData: async (address: string) => {
        calls.push(CIP95_METHODS[3]);
        expect([
          cip8Fixture.drepId,
          cip8Fixture.matchingEnterpriseAddress,
        ]).to.include(address);
        return { signature: cip8Fixture.coseSign1, key: cip8Fixture.coseKey };
      },
    },
  };

  [this.context.rawDRepResult, this.context.type6Result] =
    await invokeCip95Adapter(
      api,
      cip8Fixture.drepId,
      cip8Fixture.matchingEnterpriseAddress,
      cip8Fixture.payload
    );
});

Then('it uses the frozen CIP-95 public method names', async function () {
  expect(this.context.calls).to.deep.equal([
    ...CIP95_METHODS,
    CIP95_METHODS[3],
  ]);
});

Then(
  'raw and matching type-6 DRep inputs produce the frozen normalized COSE',
  async function () {
    expect(this.context.rawDRepResult).to.deep.equal(this.context.type6Result);
    expect(this.context.rawDRepResult).to.deep.equal({
      signature: cip8Fixture.coseSign1,
      key: cip8Fixture.coseKey,
    });
    expect(cip8Fixture.normalizedProtectedAddress).to.equal(cip8Fixture.drepId);
  }
);

Given('the source-verified CIP-103 client adapter', async function () {
  const adapter = observations.adapters.find(
    ({ id }) => id === 'cip103-reference-client'
  );
  expect(adapter?.availability).to.equal('source-verified');
});

When('the adapter receives a mixed submission failure', async function () {
  const mixedRejection = JSON.parse(
    JSON.stringify(cip103Fixtures.submission.mixedPostAttemptRejection)
  );
  const result = await captureCip103Submission(
    async (_transactions: string[]) => {
      throw mixedRejection;
    },
    cip103Fixtures.transactions.duplicateIdentity
  );
  this.context.rejection = result.rejected;
});

Then('it catches the aligned mixed array directly', async function () {
  expect(this.context.rejection).to.deep.equal(
    cip103Fixtures.submission.mixedPostAttemptRejection
  );
  expect(this.context.rejection).not.to.be.instanceOf(Error);
});

Given('the current extension registry', async function () {
  this.context.extensions = manifest.extensions.descriptors;
});

Then('CIP-104 remains proposed and policy-gated', async function () {
  const cip104 = this.context.extensions.find(({ cip }) => cip === 104);
  const method = manifest.methods.find(
    ({ path }) => path === 'api.cip104.getAccountPub'
  );
  expect(cip104.status).to.equal('proposed-policy-gated');
  expect(method?.availability).to.equal('policy-gated');
  expect(method?.positiveFixture?.cbor).to.equal(
    `5840${method?.positiveFixture?.raw}`
  );
});

Then('CIP-142 remains proposed and policy-gated', async function () {
  const cip142 = this.context.extensions.find(({ cip }) => cip === 142);
  expect(cip142.status).to.equal('proposed-policy-gated');
  expect(
    manifest.methods.find(({ path }) => path === 'api.cip142.getNetworkMagic')
      ?.availability
  ).to.equal('policy-gated');
});
