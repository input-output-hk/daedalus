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

const CIP95_METHODS = [
  'api.cip95.getPubDRepKey',
  'api.cip95.getRegisteredPubStakeKeys',
  'api.cip95.getUnregisteredPubStakeKeys',
  'api.cip95.signData',
];

const fixturePaths = {
  'contract-manifest.json':
    'source/common/cip30/contracts/contract-manifest.json',
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
  'every public provider method matches the frozen contract',
  async function () {
    const paths = manifest.methods.map(({ path }) => path);
    expect(new Set(paths).size).to.equal(paths.length);
    expect(paths).to.include.members([
      ...CIP95_METHODS,
      'api.cip103.signTxs',
      'api.cip103.submitTxs',
    ]);
    expect(manifest.providerPath).to.equal('window.cardano.daedalus');

    for (const [name, expected] of Object.entries(observations.fixtureHashes)) {
      expect(digest(fixturePaths[name])).to.equal(expected);
    }
  }
);

Then(
  'current standard revisions match the frozen provenance',
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

  [
    this.context.rawDRepResult,
    this.context.type6Result,
  ] = await invokeCip95Adapter(
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

Given('the frozen extension registry', async function () {
  this.context.extensions = manifest.extensions.descriptors;
});

Then('CIP-104 remains terminal-disabled and omitted', async function () {
  const cip104 = this.context.extensions.find(({ cip }) => cip === 104);
  expect(cip104.status).to.equal('proposed-disabled');
  expect(
    manifest.methods.find(({ path }) => path === 'api.cip104.getAccountPub')
      ?.availability
  ).to.equal('terminal-disabled');
  expect(manifest.extensions.negotiation.policyDisabledMetadata).to.contain(
    'Omit'
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
