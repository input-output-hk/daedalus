import { generateKeyPairSync, sign as signBytes } from 'crypto';
import type { KeyObject } from 'crypto';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { blake2b } from 'blakejs';
import { After, Given, Then, When } from 'cucumber';
import { expect } from 'chai';

import { bytesForSpan } from '../../../../source/common/cardano/cborSlices';
import {
  prepareCip8Request,
  serializeCip8,
  verifyCip8BackendResponse,
} from '../../../../source/common/cardano/cip8';
import {
  encodeCoseProtectedHeader,
  encodeCoseSignatureStructure,
} from '../../../../source/common/cardano/cose';
import semanticFixture from '../../../../source/common/cardano/fixtures/exact-cbor/semantic-conway-v1.json';
import { parseConwayTransactionEnvelope } from '../../../../source/common/cardano/transactionEnvelope';
import {
  encodeVKeyWitnessSet,
  verifyVKeyWitnesses,
} from '../../../../source/common/cardano/witnessSet';
import { CollateralPreferenceStore } from '../../../../source/main/cip30/CollateralPreferenceStore';
import { GrantRepository } from '../../../../source/main/cip30/GrantRepository';

const walletId = 'wallet-a';
const origin = 'https://dapp.example';
const genesis = '11'.repeat(32);
type KeyMaterial = Readonly<{
  keys: Readonly<{ privateKey: KeyObject; publicKey: KeyObject }>;
  publicKey: Buffer;
}>;

const networkMagic = 764824073;

const keyMaterial = () => {
  const keys = generateKeyPairSync('ed25519');
  const publicDer = keys.publicKey.export({
    format: 'der',
    type: 'spki',
  }) as Buffer;
  return { keys, publicKey: publicDer.subarray(-32) };
};

const signCip8 = (
  address: string,
  payload: string,
  material: KeyMaterial,
  drepCredential?: string
) => {
  const expected = prepareCip8Request(address, payload, {
    networkId: 0,
    drepCredential,
  });
  const signature = signBytes(
    null,
    encodeCoseSignatureStructure(
      encodeCoseProtectedHeader(expected.protectedAddress),
      expected.payload
    ),
    material.keys.privateKey
  );
  const result = serializeCip8(expected, {
    publicKey: material.publicKey,
    signature,
  });
  return verifyCip8BackendResponse(expected, {
    revision: 1,
    credential_kind: expected.credentialKind,
    credential: expected.credential.toString('hex'),
    cose_sign1: result.signature,
    cose_key: result.key,
  });
};

After(function () {
  if (this.context.softwareConnectorDirectory)
    fs.rmSync(this.context.softwareConnectorDirectory, {
      recursive: true,
      force: true,
    });
});

Given(/^an eligible software wallet and dApp request$/, function () {
  const directory = fs.mkdtempSync(
    path.join(os.tmpdir(), 'daedalus-software-connector-')
  );
  this.context.softwareConnectorDirectory = directory;
  this.context.grants = new GrantRepository(
    path.join(directory, 'grants.json')
  );
  this.context.preferences = new CollateralPreferenceStore(
    path.join(directory, 'collateral.json')
  );
  this.context.approvals = [];
  this.context.transaction = parseConwayTransactionEnvelope(
    Buffer.from(semanticFixture.cborHex, 'hex')
  );
});

Given(
  /^an eligible software wallet without sufficient collateral$/,
  function () {
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), 'daedalus-collateral-journey-')
    );
    this.context.softwareConnectorDirectory = directory;
    this.context.preferences = new CollateralPreferenceStore(
      path.join(directory, 'collateral.json')
    );
    expect(this.context.preferences.get(walletId, genesis)).to.equal(undefined);
  }
);

When(/^the user connects the dApp$/, function () {
  this.context.grants.put({
    origin,
    walletId,
    networkGenesis: genesis,
    networkMagic,
    readScopes: ['connection', 'read'],
    enabledExtensionScopes: [],
    launch: { kind: 'diagnostics' },
    grantedAt: '2026-08-28T00:00:00.000Z',
  });
  this.context.approvals.push('connection');
});

When(/^the user connects the dApp with CIP-95$/, function () {
  this.context.grants.put({
    origin,
    walletId,
    networkGenesis: genesis,
    networkMagic,
    readScopes: ['connection', 'read'],
    enabledExtensionScopes: [95],
    launch: { kind: 'diagnostics' },
    grantedAt: '2026-08-28T00:00:00.000Z',
  });
  this.context.approvals.push('connection');
});

Then(/^the dApp can read the selected wallet network$/, function () {
  const grant = this.context.grants.find({
    origin,
    walletId,
    networkGenesis: genesis,
    launch: { kind: 'diagnostics' },
    scopes: ['connection', 'read'],
    extensions: [],
  });
  expect(grant).to.include({ walletId, networkMagic });
});

When(/^the user approves exact data and transaction signing$/, function () {
  const dataKeys = keyMaterial();
  const credential = Buffer.from(
    blake2b(dataKeys.publicKey, undefined, 28)
  ).toString('hex');
  this.context.dataSignature = signCip8(
    `60${credential}`,
    Buffer.from('Hello, Cardano', 'utf8').toString('hex'),
    dataKeys
  );

  const transactionKeys = keyMaterial();
  const body = bytesForSpan(
    this.context.transaction.cbor,
    this.context.transaction.spans.body
  );
  const bodyHash = Buffer.from(blake2b(body, undefined, 32));
  const signature = signBytes(null, bodyHash, transactionKeys.keys.privateKey);
  verifyVKeyWitnesses(body, [
    { publicKey: transactionKeys.publicKey, signature },
  ]);
  this.context.witnessSet = encodeVKeyWitnessSet([
    { publicKey: transactionKeys.publicKey, signature },
  ]).toString('hex');
  this.context.approvals.push('data-sign', 'transaction-sign');
});

Then(
  /^the connector returns locally verified CIP-8 data and transaction witnesses$/,
  function () {
    expect(this.context.dataSignature).to.have.keys('key', 'signature');
    expect(this.context.witnessSet).to.match(/^a100/u);
    expect(this.context.approvals).to.deep.equal([
      'connection',
      'data-sign',
      'transaction-sign',
    ]);
  }
);

When(
  /^the user separately approves exact transaction submission$/,
  function () {
    this.context.approvals.push('transaction-submit');
    this.context.submittedTransactionId = this.context.transaction.transactionId;
  }
);

Then(/^the connector returns the locally derived transaction id$/, function () {
  expect(this.context.approvals).to.deep.equal([
    'connection',
    'data-sign',
    'transaction-sign',
    'transaction-submit',
  ]);
  expect(this.context.submittedTransactionId).to.equal(
    this.context.transaction.transactionId
  );
});

When(/^separately approves governance key disclosure$/, function () {
  const grant = this.context.grants.find({
    origin,
    walletId,
    networkGenesis: genesis,
    launch: { kind: 'diagnostics' },
    scopes: ['connection', 'read'],
    extensions: [95],
  });
  expect(grant?.enabledExtensionScopes).to.deep.equal([95]);
  this.context.approvals.push('governance-key-disclosure');
});

Then(
  /^raw and type-6 DRep signing inputs produce the same verified identity$/,
  function () {
    const material = keyMaterial();
    const credential = Buffer.from(
      blake2b(material.publicKey, undefined, 28)
    ).toString('hex');
    const payload = Buffer.from('Governance', 'utf8').toString('hex');
    const raw = signCip8(credential, payload, material, credential);
    const type6 = signCip8(`60${credential}`, payload, material, credential);
    expect(type6).to.deep.equal(raw);
    expect(this.context.approvals).to.deep.equal([
      'connection',
      'governance-key-disclosure',
    ]);
  }
);

When(/^the user explicitly starts collateral preparation$/, function () {
  this.context.collateralPreparation = {
    state: 'preparing',
    route: `/wallets/${walletId}/send`,
  };
});

Then(/^the normal confirmed Send flow is required$/, function () {
  expect(this.context.collateralPreparation).to.deep.equal({
    state: 'preparing',
    route: `/wallets/${walletId}/send`,
  });
  expect(this.context.preferences.get(walletId, genesis)).to.equal(undefined);
});

When(/^the submitted preparation output is confirmed$/, function () {
  const transactionId = '22'.repeat(32);
  this.context.preferences.put({
    walletId,
    networkGenesis: genesis,
    targetLovelace: '5000000',
    preferredInputs: [{ transactionId, index: 0 }],
    generation: 1,
  });
  this.context.confirmedCollateral = { transactionId, index: 0 };
});

Then(/^the confirmed output becomes the preferred collateral$/, function () {
  expect(
    this.context.preferences.get(walletId, genesis)?.preferredInputs
  ).to.deep.equal([this.context.confirmedCollateral]);
});
