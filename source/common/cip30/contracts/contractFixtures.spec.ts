import { createPublicKey, verify } from 'crypto';
import { MessageChannel } from 'worker_threads';

import Ajv from 'ajv';
import { bech32 } from 'bech32';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import manifest from './contract-manifest.json';
import cip8Fixture from './fixtures/cip8-cip95-fixture.json';
import cip103Fixtures from './fixtures/cip103-fixtures.json';
import schemaCases from './fixtures/schema-cases.json';
import wireFixtures from './fixtures/wire-fixtures.json';
import commonSchema from './schemas/common.schema.json';
import errorsSchema from './schemas/errors.schema.json';
import envelopeSchema from './schemas/envelope.schema.json';

const schemaByFile = {
  'common.json': commonSchema,
  'errors.json': errorsSchema,
  'envelope.json': envelopeSchema,
};

const schemaIdByFile = {
  'common.json': commonSchema.$id,
  'errors.json': errorsSchema.$id,
  'envelope.json': envelopeSchema.$id,
};

const toAbsoluteSchemaRef = (schemaRef: string): string => {
  const [file, fragment] = schemaRef.split('#');
  return `${schemaIdByFile[file]}#${fragment || ''}`;
};

const cloneThroughMessageChannel = <T>(value: T): Promise<T> =>
  new Promise((resolve) => {
    const { port1, port2 } = new MessageChannel();
    port2.once('message', (message) => {
      port1.close();
      port2.close();
      resolve(message);
    });
    port1.postMessage(value);
  });

const isCanonicalCoin = (hex: string): boolean => {
  const bytes = Buffer.from(hex, 'hex');
  if (bytes.toString('hex') !== hex || bytes.length === 0) return false;

  const additionalInfo = bytes[0] & 0x1f;
  if (bytes[0] >> 5 !== 0) return false;
  if (additionalInfo < 24) return bytes.length === 1;
  if (additionalInfo === 24)
    return bytes.length === 2 && bytes.readUInt8(1) >= 24;
  if (additionalInfo === 25)
    return bytes.length === 3 && bytes.readUInt16BE(1) > 0xff;
  if (additionalInfo === 26)
    return bytes.length === 5 && bytes.readUInt32BE(1) > 0xffff;
  if (additionalInfo === 27)
    return bytes.length === 9 && bytes.readUInt32BE(1) > 0;
  return false;
};

const isPlainDataObject = (value: unknown): boolean => {
  if (value === null || typeof value !== 'object' || Array.isArray(value))
    return true;
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) return false;
  for (const key in value) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) return false;
  }
  return Object.values(Object.getOwnPropertyDescriptors(value)).every(
    (descriptor) => !descriptor.get && !descriptor.set
  );
};

type InvocationArgument = {
  required: boolean;
  undefinedMeansOmitted: boolean;
};

type InvocationMethod = {
  minArgs: number;
  maxArgs: number;
  args: InvocationArgument[];
};

type ExtensionDescriptor = {
  cip: number;
  status: string;
  dependencies: number[];
  incompatibleWith: number[];
};

type TestApi = {
  active: boolean;
  generation: number;
  getExtensions: () => Array<{ cip: number }>;
  signTx: string;
  [key: string]: unknown;
};

const invocationIsValid = (
  method: InvocationMethod,
  args: unknown[]
): boolean => {
  if (args.length < method.minArgs || args.length > method.maxArgs)
    return false;
  return method.args.every((argument, index) => {
    const value = args[index];
    if (value === undefined)
      return !argument.required && argument.undefinedMeansOmitted === true;
    if (
      ['function', 'symbol', 'bigint'].includes(typeof value) ||
      (typeof value === 'number' && !Number.isFinite(value))
    )
      return false;
    return isPlainDataObject(value);
  });
};

const negotiate = (
  requested: unknown,
  available: number[],
  descriptors: ExtensionDescriptor[] = manifest.extensions.descriptors
): number[] => {
  if (!Array.isArray(requested)) throw new Error('APIError.InvalidRequest');
  const extensions = requested as Array<{ cip: number }>;
  extensions.forEach((extension) => {
    if (
      !isPlainDataObject(extension) ||
      extension === null ||
      Object.keys(extension).length !== 1 ||
      !Number.isSafeInteger(extension.cip) ||
      extension.cip <= 0
    )
      throw new Error('APIError.InvalidRequest');
  });

  const requestedCips = new Set(extensions.map(({ cip }) => cip));
  const enabled: number[] = [];
  descriptors.forEach((descriptor) => {
    if (
      !requestedCips.has(descriptor.cip) ||
      !available.includes(descriptor.cip)
    )
      return;
    if (descriptor.status === 'proposed-disabled') return;
    if (!descriptor.dependencies.every((cip: number) => enabled.includes(cip)))
      return;
    if (
      descriptor.incompatibleWith.some((cip: number) => enabled.includes(cip))
    )
      return;
    enabled.push(descriptor.cip);
  });
  return enabled;
};

const makeApi = (enabled: number[], generation: number) => {
  const api: TestApi = {
    active: true,
    generation,
    getExtensions: () => enabled.map((cip) => ({ cip })),
    signTx: enabled.includes(95) ? 'cip95-signTx' : 'base-signTx',
  };
  enabled.forEach((cip) => {
    api[`cip${cip}`] =
      cip === 103 ? { effectiveSigner: api.signTx, signTxs: true } : {};
  });
  return api;
};

describe('frozen CIP-30 contracts', () => {
  const ajv = new Ajv({ allErrors: true, schemaId: 'auto' });

  beforeAll(() => {
    Object.values(schemaByFile).forEach((schema) => {
      ajv.addSchema(schema);
    });
  });

  it('covers every public path and resolves every declared schema', () => {
    const expectedPaths = [
      'provider.isEnabled',
      'provider.enable',
      'api.getExtensions',
      'api.getNetworkId',
      'api.getUtxos',
      'api.getCollateral',
      'api.getBalance',
      'api.getUsedAddresses',
      'api.getUnusedAddresses',
      'api.getChangeAddress',
      'api.getRewardAddresses',
      'api.signTx',
      'api.signData',
      'api.submitTx',
      'api.cip95.getPubDRepKey',
      'api.cip95.getRegisteredPubStakeKeys',
      'api.cip95.getUnregisteredPubStakeKeys',
      'api.cip95.signData',
      'api.cip103.signTxs',
      'api.cip103.submitTxs',
      'api.cip104.getAccountPub',
      'api.cip142.getNetworkMagic',
    ];

    expect(manifest.methods.map(({ path }) => path)).toEqual(expectedPaths);
    expect(manifest.extensions.explicitlyAbsent).toEqual([8, 106, 141]);
    expect(manifest.extensions.registryOrder).toEqual([95, 103, 104, 142]);

    manifest.methods.forEach(({ rejections }) => {
      rejections.forEach((rejection) => {
        const schemaRef = manifest.rejectionSchemas[rejection];
        expect(schemaRef).toBeDefined();
        expect(ajv.getSchema(toAbsoluteSchemaRef(schemaRef))).toBeDefined();
      });
    });

    manifest.methods.forEach((method) => {
      expect(Boolean(method.successSchema || method.successSchemaRef)).toBe(
        true
      );
      method.args.forEach((argument) => {
        expect(Boolean(argument.schema || argument.schemaRef)).toBe(true);
      });
    });
    const optionalArguments = manifest.methods.reduce(
      (result, method) => [
        ...result,
        ...method.args.filter((argument) => !argument.required),
      ],
      []
    );
    expect(
      optionalArguments.every((argument) =>
        Object.prototype.hasOwnProperty.call(argument, 'default')
      )
    ).toBe(true);

    expect(Object.keys(wireFixtures).sort()).toEqual(
      [
        'addressNegatives',
        'addresses',
        'baseApi',
        'coin',
        'emptyWitnessSet',
        'limits',
        'negotiation',
        'networkMagic',
      ].sort()
    );
    expect(Object.keys(cip103Fixtures).sort()).toEqual(
      ['dependencyCases', 'signing', 'submission', 'transactions'].sort()
    );

    const refs = JSON.stringify(manifest).match(/[a-z-]+\.json#[^" ]+/g) || [];
    refs.forEach((schemaRef) => {
      expect(ajv.getSchema(toAbsoluteSchemaRef(schemaRef))).toBeDefined();
    });
  });

  it('meta-validates schemas and validates all JSON fixture cases', () => {
    Object.values(schemaByFile).forEach((schema) => {
      expect(ajv.validateSchema(schema)).toBe(true);
    });

    schemaCases.cases.forEach(({ schema, valid, value }) => {
      const validate = ajv.getSchema(toAbsoluteSchemaRef(schema));
      expect(validate).toBeDefined();
      if (!validate) throw new Error(`Missing schema: ${schema}`);
      expect(validate(value)).toBe(valid);
    });

    const hash = 'a'.repeat(64);
    const address = wireFixtures.addresses[0].raw;
    const definitionSamples: Record<
      string,
      Record<string, [unknown, unknown]>
    > = {
      'common.json': {
        hexBytes: ['', '0x00'],
        nonEmptyHexBytes: ['a0', ''],
        hash32: [hash, 'aa'],
        publicKey32: [cip8Fixture.publicKey, 'aa'],
        drepId: [cip8Fixture.drepId, 'aa'],
        addressInput: [address, '0x00'],
        addressResult: [address, 'addr1invalid'],
        addressResults: [[address], ['addr1invalid']],
        utxoResults: [null, ['addr1invalid']],
        witnessSets: [['a0'], []],
        hashes: [[hash], []],
        publicKeys: [[cip8Fixture.publicKey], ['aa']],
        extension: [{ cip: 95 }, { cip: 95, extra: true }],
        extensions: [[{ cip: 95 }], [{ cip: 0 }]],
        enableOptions: [{ extensions: [{ cip: 95 }] }, { extra: true }],
        paginate: [
          { page: 0, limit: 100 },
          { page: 0, limit: 101 },
        ],
        getCollateralParams: [{ amount: '00' }, {}],
        transactionSignatureRequest: [{ cbor: 'a0' }, { cbor: '' }],
        transactionSignatureRequests: [[{ cbor: 'a0' }], []],
        transactions: [['a0'], []],
        dataSignature: [
          { signature: 'a0', key: 'a0' },
          { signature: '', key: 'a0' },
        ],
        networkId: [1, 2],
        networkMagic: [4294967295, 4294967296],
        nonEmptyString: ['Daedalus', ''],
      },
      'errors.json': {
        apiError: [
          { code: -1, info: 'Invalid' },
          { code: 0, info: 'Invalid' },
        ],
        paginateError: [{ maxSize: 0 }, { maxSize: -1 }],
        txSignError: [
          { code: 3, info: 'Deprecated' },
          { code: 4, info: '' },
        ],
        dataSignError: [
          { code: 2, info: 'Script' },
          { code: 4, info: '' },
        ],
        txSendError: [
          { code: 2, info: 'Failure' },
          { code: 3, info: '' },
        ],
        cip103SubmitError: [[hash, { code: 2, info: 'Failure' }], []],
      },
      'envelope.json': {
        fulfilled: [
          { status: 'fulfilled', value: 'a0' },
          { status: 'fulfilled' },
        ],
        rejection: [
          { type: 'api-error', value: { code: -1, info: 'Invalid' } },
          { type: 'api-error', value: { code: 0, info: 'Invalid' } },
        ],
        rejected: [
          {
            status: 'rejected',
            rejection: {
              type: 'api-error',
              value: { code: -1, info: 'Invalid' },
            },
          },
          { status: 'rejected' },
        ],
        resultEnvelope: [
          { status: 'fulfilled', value: 'a0' },
          { status: 'unknown', value: 'a0' },
        ],
      },
    };

    Object.entries(schemaByFile).forEach(([file, schema]) => {
      expect(Object.keys(definitionSamples[file]).sort()).toEqual(
        Object.keys(schema.definitions).sort()
      );
      Object.entries(definitionSamples[file]).forEach(
        ([definition, [valid, invalid]]) => {
          const validate = ajv.getSchema(
            toAbsoluteSchemaRef(`${file}#/definitions/${definition}`)
          );
          if (!validate) throw new Error(`Missing definition: ${definition}`);
          expect(validate(valid)).toBe(true);
          expect(validate(invalid)).toBe(false);
        }
      );
    });

    const inlineSchemas: unknown[] = [];
    manifest.providerProperties.forEach((property) => {
      if (property.schema) inlineSchemas.push(property.schema);
    });
    manifest.methods.forEach((method) => {
      if (method.successSchema) inlineSchemas.push(method.successSchema);
      method.args.forEach((argument) => {
        if (argument.schema) inlineSchemas.push(argument.schema);
      });
    });
    inlineSchemas.forEach((schema) =>
      expect(() => ajv.compile(schema as object)).not.toThrow()
    );

    const contractReferences = JSON.stringify([
      manifest,
      schemaCases,
      ...Object.values(schemaByFile),
    ]);
    Object.values(schemaByFile).forEach((schema) => {
      Object.keys(schema.definitions).forEach((definition) => {
        expect(contractReferences).toContain(`/definitions/${definition}`);
      });
    });
  });

  it('executes deterministic negotiation, metadata, and API replacement rules', () => {
    wireFixtures.negotiation.forEach(({ requested, available, enabled }) => {
      expect(
        negotiate(
          requested.map((cip) => ({ cip })),
          available
        )
      ).toEqual(enabled);
    });

    expect(() => negotiate([{ cip: 95, extra: true }], [95])).toThrow(
      'APIError.InvalidRequest'
    );
    expect(() => negotiate([{ cip: NaN }], [95])).toThrow(
      'APIError.InvalidRequest'
    );
    expect(() => negotiate({ cip: 95 }, [95])).toThrow(
      'APIError.InvalidRequest'
    );
    const inheritedExtension = Object.create({ extra: true });
    inheritedExtension.cip = 95;
    expect(() => negotiate([inheritedExtension], [95])).toThrow(
      'APIError.InvalidRequest'
    );
    expect(negotiate([{ cip: 9999 }], [95, 103])).toEqual([]);
    expect(negotiate([{ cip: 104 }, { cip: 142 }], [104, 142])).toEqual([142]);

    const syntheticDescriptors = [
      { cip: 1, status: 'active', dependencies: [], incompatibleWith: [] },
      { cip: 2, status: 'active', dependencies: [1], incompatibleWith: [] },
      { cip: 3, status: 'active', dependencies: [], incompatibleWith: [1] },
    ];
    expect(
      negotiate(
        [{ cip: 1 }, { cip: 2 }, { cip: 3 }],
        [1, 2, 3],
        syntheticDescriptors
      )
    ).toEqual([1, 2]);
    expect(negotiate([{ cip: 2 }], [2], syntheticDescriptors)).toEqual([]);

    const metadata = negotiate(
      manifest.extensions.registryOrder.map((cip) => ({ cip })),
      [95, 103, 104, 142]
    );
    expect(metadata).toEqual([95, 103, 142]);

    const firstApi = makeApi([95, 103], 1);
    expect(firstApi.signTx).toBe('cip95-signTx');
    expect(
      (firstApi.cip103 as { effectiveSigner: string }).effectiveSigner
    ).toBe('cip95-signTx');
    expect(Object.prototype.hasOwnProperty.call(firstApi, 'cip104')).toBe(
      false
    );
    const secondApi = makeApi([103], 2);
    firstApi.active = false;
    expect(firstApi.active).toBe(false);
    expect(secondApi.generation).toBe(2);
    expect(secondApi.signTx).toBe('base-signTx');
    expect(
      (secondApi.cip103 as { effectiveSigner: string }).effectiveSigner
    ).toBe('base-signTx');
    expect(secondApi.getExtensions()).toEqual([{ cip: 103 }]);
    expect(Object.prototype.hasOwnProperty.call(secondApi, 'cip95')).toBe(
      false
    );
  });

  it('independently checks error, consent, and exact limit contracts', () => {
    const actualRejections = manifest.methods.reduce(
      (result, { path, rejections }) => ({ ...result, [path]: rejections }),
      {}
    );
    expect(actualRejections).toEqual(schemaCases.expectedRejections);
    expect(manifest.consentExpiry).toEqual(schemaCases.expectedConsentExpiry);
    expect(wireFixtures.limits).toEqual({
      decodedBytesAccepted: 65536,
      decodedBytesRejected: 65537,
      batchAccepted: 50,
      batchRejected: 51,
      pageLimitAccepted: 100,
      pageLimitRejected: 101,
      consentInactivityMilliseconds: 300000,
    });
    expect(wireFixtures.networkMagic).toEqual({
      mainnet: 764824073,
      preprod: 1,
      preview: 2,
      custom: 42,
      minimum: 0,
      maximum: 4294967295,
    });

    const acceptedHex = '00'.repeat(65536);
    const rejectedHex = '00'.repeat(65537);
    const decodedSizeIsAccepted = (hex: string) =>
      /^(?:[0-9a-f]{2})*$/.test(hex) &&
      Buffer.from(hex, 'hex').length <=
        manifest.limits.decodedBytesPerTransactionOrPayload.maximum;
    expect(decodedSizeIsAccepted(acceptedHex)).toBe(true);
    expect(decodedSizeIsAccepted(rejectedHex)).toBe(false);
    expect(decodedSizeIsAccepted(`0x${acceptedHex}`)).toBe(false);

    schemaCases.sizeLimitedPaths.forEach((path) => {
      const limitedMethod = manifest.methods.find(
        (method) => method.path === path
      );
      expect(limitedMethod).toBeDefined();
      const limitArgument =
        limitedMethod &&
        limitedMethod.args.find(
          (argument) =>
            argument.decodedByteMaximum === 65536 ||
            argument.decodedByteMaximumPerItem === 65536
        );
      expect(limitArgument).toBeDefined();
    });

    const paginate = ajv.getSchema(
      toAbsoluteSchemaRef('common.json#/definitions/paginate')
    );
    const signTxs = ajv.getSchema(
      toAbsoluteSchemaRef(
        'common.json#/definitions/transactionSignatureRequests'
      )
    );
    const submitTxs = ajv.getSchema(
      toAbsoluteSchemaRef('common.json#/definitions/transactions')
    );
    const networkMagic = ajv.getSchema(
      toAbsoluteSchemaRef('common.json#/definitions/networkMagic')
    );
    if (!paginate || !signTxs || !submitTxs || !networkMagic)
      throw new Error('Missing limit schema');

    expect(paginate({ page: 0, limit: 1 })).toBe(true);
    expect(paginate({ page: Number.MAX_SAFE_INTEGER, limit: 100 })).toBe(true);
    [0, 101, -1, 1.5, NaN, Infinity].forEach((limit) =>
      expect(paginate({ page: 0, limit })).toBe(false)
    );
    [-1, 1.5, Number.MAX_SAFE_INTEGER + 1, NaN, Infinity].forEach((page) =>
      expect(paginate({ page, limit: 1 })).toBe(false)
    );

    expect(signTxs([{ cbor: 'a0' }])).toBe(true);
    expect(signTxs(Array.from({ length: 50 }, () => ({ cbor: 'a0' })))).toBe(
      true
    );
    expect(signTxs([])).toBe(false);
    expect(signTxs(Array.from({ length: 51 }, () => ({ cbor: 'a0' })))).toBe(
      false
    );
    expect(submitTxs(['a0'])).toBe(true);
    expect(submitTxs(Array.from({ length: 50 }, () => 'a0'))).toBe(true);
    expect(submitTxs([])).toBe(false);
    expect(submitTxs(Array.from({ length: 51 }, () => 'a0'))).toBe(false);

    [0, 1, 2, 764824073, 4294967295].forEach((magic) =>
      expect(networkMagic(magic)).toBe(true)
    );
    [-1, 1.5, 4294967296, NaN, Infinity].forEach((magic) =>
      expect(networkMagic(magic)).toBe(false)
    );

    const signTxsMethod = manifest.methods.find(
      ({ path }) => path === 'api.cip103.signTxs'
    );
    expect(signTxsMethod && signTxsMethod.failureInfo).toContain(
      'Transaction at index <n> failed'
    );
    expect(schemaCases.expectedRejections['api.cip103.submitTxs']).toEqual([
      'apiError',
      'txSendError',
      'cip103SubmitError',
    ]);
  });

  it('round-trips every Bech32 fixture to its exact raw address bytes', () => {
    wireFixtures.addresses.forEach(
      ({ bech32: encoded, hrp, raw, type, networkId }) => {
        const decoded = bech32.decode(encoded, 1000);
        expect(decoded.prefix).toBe(hrp);
        expect(
          Buffer.from(bech32.fromWords(decoded.words)).toString('hex')
        ).toBe(raw);
        expect(Buffer.from(raw, 'hex')[0] >> 4).toBe(type);
        expect(Buffer.from(raw, 'hex')[0] & 0x0f).toBe(networkId);
      }
    );

    wireFixtures.addressNegatives
      .filter(
        ({ name }) => name === 'wrong-hrp' || name === 'wrong-route-network'
      )
      .forEach(({ name, value }) => {
        const decoded = bech32.decode(value, 1000);
        const raw = Buffer.from(bech32.fromWords(decoded.words));
        const addressType = raw[0] >> 4;
        const networkId = raw[0] & 0x0f;
        const validHrp =
          addressType >= 14
            ? decoded.prefix === (networkId === 1 ? 'stake' : 'stake_test')
            : decoded.prefix === (networkId === 1 ? 'addr' : 'addr_test');
        expect(validHrp).toBe(false);
        expect(name).toMatch(/wrong-(hrp|route-network)/);
      });

    const matchingDrep = wireFixtures.addresses.find(
      ({ name }) => name === 'mainnet-enterprise-matching-drep'
    );
    const ownedNonmatching = wireFixtures.addresses.find(
      ({ name }) => name === 'mainnet-enterprise-nonmatching-drep'
    );
    const unownedNonmatching = wireFixtures.addresses.find(
      ({ name }) => name === 'mainnet-enterprise-unowned-payment'
    );
    expect(matchingDrep && matchingDrep.protectedAddress).toBe(
      cip8Fixture.drepId
    );
    expect(ownedNonmatching && ownedNonmatching.expectedOutcome).toBe(
      'payment-success'
    );
    expect(ownedNonmatching && ownedNonmatching.protectedAddress).toBe(
      ownedNonmatching && ownedNonmatching.raw
    );
    expect(unownedNonmatching && unownedNonmatching.expectedOutcome).toBe(
      'DataSignError.ProofGeneration'
    );
    wireFixtures.addresses
      .filter(({ classification }) => classification === 'address-not-pk')
      .forEach(({ expectedOutcome }) =>
        expect(expectedOutcome).toBe('DataSignError.AddressNotPK')
      );
    wireFixtures.addressNegatives.forEach(({ error }) =>
      expect(error).toBe('APIError.InvalidRequest')
    );
    const malformedCase = wireFixtures.addressNegatives.find(
      ({ name }) => name === 'malformed-shelley-length'
    );
    if (!malformedCase) throw new Error('Missing malformed-address fixture');
    const malformedLength = Buffer.from(malformedCase.value, 'hex');
    expect(malformedLength).toHaveLength(2);
    const futureCase = wireFixtures.addressNegatives.find(
      ({ name }) => name === 'future-address-type'
    );
    if (!futureCase) throw new Error('Missing future-address fixture');
    const futureType = Buffer.from(futureCase.value, 'hex')[0] >> 4;
    expect(futureType).toBe(8);

    const addressResult = ajv.getSchema(
      toAbsoluteSchemaRef('common.json#/definitions/addressResult')
    );
    if (!addressResult) throw new Error('Missing address-result schema');
    expect(addressResult(wireFixtures.addresses[0].raw)).toBe(true);
    expect(addressResult(wireFixtures.addresses[0].bech32)).toBe(false);
  });

  it('locks canonical Coin boundaries and empty partial-sign success', async () => {
    const decoded = wireFixtures.coin.positive.map((hex) =>
      cbor.decodeFirstSync(hex)
    );
    expect(decoded.map(String)).toEqual([
      '0',
      '23',
      '24',
      '18446744073709551615',
    ]);
    expect(wireFixtures.coin.positive.every(isCanonicalCoin)).toBe(true);
    expect(wireFixtures.coin.negative.every(isCanonicalCoin)).toBe(false);
    expect(cbor.decodeFirstSync(wireFixtures.emptyWitnessSet)).toEqual({});
    const signTxMethod = manifest.methods.find(
      ({ path }) => path === 'api.signTx'
    );
    expect(signTxMethod).toBeDefined();
    expect(signTxMethod && signTxMethod.partialSignNoApplicableKey).toContain(
      'a0'
    );

    expect(cbor.decodeFirstSync(wireFixtures.baseApi.minimalValue)).toBe(0);
    const utxo = cbor.decodeFirstSync(
      wireFixtures.baseApi.transactionUnspentOutput
    );
    expect(utxo).toHaveLength(2);
    expect(utxo[0][0]).toHaveLength(32);
    expect(utxo[0][1]).toBe(0);
    const utxoAddress = wireFixtures.addresses.find(
      ({ name }) => name === 'mainnet-enterprise-matching-drep'
    );
    if (!utxoAddress) throw new Error('Missing UTxO address fixture');
    expect(utxo[1][0].toString('hex')).toBe(utxoAddress.raw);
    expect(utxo[1][1]).toBe(0);
    expect(wireFixtures.baseApi.getUtxos.insufficient).toBeNull();
    expect(wireFixtures.baseApi.getCollateral.insufficient).toBeNull();
    expect(wireFixtures.baseApi.getCollateral.sideEffect).toBe('none');
    expect(wireFixtures.baseApi.getCollateral.daedalusAdaCap).toBeNull();
    expect(
      manifest.methods.find(({ path }) => path === 'api.getCollateral')
        ?.semantics
    ).toMatch(/^Deprecated\./);
  });

  it('freezes CIP-103 identity, dependencies, signing, and submission results', () => {
    expect(cip103Fixtures.transactions.duplicateIdentity[0]).toBe(
      cip103Fixtures.transactions.duplicateIdentity[1]
    );
    expect(cip103Fixtures.dependencyCases.map(({ name }) => name)).toEqual([
      'same-request-parent',
      'reference-input',
      'forward-reference',
      'self-reference',
      'unresolved-reference',
      'conflicting-spend',
    ]);
    expect(cip103Fixtures.signing.firstFailure.error.info).toBe(
      'Transaction at index 1 failed'
    );
    expect(cip103Fixtures.signing.firstFailure.releasedWitnesses).toBeNull();
    expect(cip103Fixtures.signing.allSuccessWitnesses).toEqual(['a0', 'a0']);
    expect(cip103Fixtures.submission.preAttemptRefusal).toEqual({
      code: 1,
      info: 'Refused',
    });
    expect(cip103Fixtures.submission.mixedPostAttemptRejection).toHaveLength(2);
    expect(cip103Fixtures.submission.allSuccessHashes).toHaveLength(2);
  });

  it('verifies exact CIP-8 bytes and CIP-95 DRep normalization', () => {
    const acceptsProducedCip8Profile = (hex: string): boolean => {
      if (!hex.startsWith('84')) return false;
      try {
        const decoded = cbor.decodeFirstSync(hex);
        return (
          Array.isArray(decoded) &&
          decoded.length === 4 &&
          decoded[1].hashed === false &&
          decoded[1].version === 1
        );
      } catch (_error) {
        return false;
      }
    };

    const protectedHeader = cbor.decodeFirstSync(cip8Fixture.protectedHeader);
    const coseSign1 = cbor.decodeFirstSync(cip8Fixture.coseSign1);
    const coseKey = cbor.decodeFirstSync(cip8Fixture.coseKey);

    expect(acceptsProducedCip8Profile(cip8Fixture.coseSign1)).toBe(true);
    expect(coseSign1[0].toString('hex')).toBe(cip8Fixture.protectedHeader);
    expect(protectedHeader.get(1)).toBe(-8);
    expect(protectedHeader.get('address').toString('hex')).toBe(
      cip8Fixture.drepId
    );
    expect(coseSign1[1].hashed).toBe(false);
    expect(coseSign1[1].version).toBe(1);
    expect(Object.keys(coseSign1[1]).sort()).toEqual(['hashed', 'version']);
    expect(coseSign1[2].toString('hex')).toBe(cip8Fixture.payload);
    expect(coseSign1[3].toString('hex')).toBe(cip8Fixture.signature);
    expect(protectedHeader.has(4)).toBe(false);
    expect(coseKey.get(1)).toBe(1);
    expect(coseKey.get(3)).toBe(-8);
    expect(coseKey.get(-1)).toBe(6);
    expect(coseKey.get(-2).toString('hex')).toBe(cip8Fixture.publicKey);
    expect(coseKey.has(2)).toBe(false);

    const publicKeyDer = Buffer.concat([
      Buffer.from('302a300506032b6570032100', 'hex'),
      Buffer.from(cip8Fixture.publicKey, 'hex'),
    ]);
    const publicKey = createPublicKey({
      key: publicKeyDer,
      format: 'der',
      type: 'spki',
    });
    const reconstructedSigStructure = cbor.encodeCanonical([
      'Signature1',
      coseSign1[0],
      Buffer.alloc(0),
      coseSign1[2],
    ]);
    expect(reconstructedSigStructure.toString('hex')).toBe(
      cip8Fixture.sigStructure
    );
    expect(
      verify(null, reconstructedSigStructure, publicKey, coseSign1[3])
    ).toBe(true);
    expect(cip8Fixture.matchingEnterpriseAddress.slice(2)).toBe(
      cip8Fixture.drepId
    );
    const computedDrepId = Buffer.from(
      blake2b(Buffer.from(cip8Fixture.publicKey, 'hex'), undefined, 28)
    ).toString('hex');
    expect(computedDrepId).toBe(cip8Fixture.drepId);
    expect(cip8Fixture.normalizedProtectedAddress).toBe(cip8Fixture.drepId);

    expect(cip8Fixture.negativeCases.oddLengthHex).not.toMatch(
      /^(?:[0-9a-f]{2})*$/
    );
    expect(cip8Fixture.negativeCases.prefixedHex).not.toMatch(
      /^(?:[0-9a-f]{2})*$/
    );
    expect(cip8Fixture.negativeCases.malformedHex).not.toMatch(
      /^(?:[0-9a-f]{2})*$/
    );
    expect(
      acceptsProducedCip8Profile(cip8Fixture.negativeCases.taggedCoseSign1)
    ).toBe(false);
    expect(() =>
      cbor.decodeFirstSync(cip8Fixture.negativeCases.malformedCbor)
    ).toThrow();

    const changed = cbor.decodeFirstSync(
      cip8Fixture.negativeCases.changedPayload
    );
    const changedSigStructure = cbor.encodeCanonical([
      'Signature1',
      changed[0],
      Buffer.alloc(0),
      changed[2],
    ]);
    expect(verify(null, changedSigStructure, publicKey, changed[3])).toBe(
      false
    );

    const legacy = cbor.decodeFirstSync(
      cip8Fixture.negativeCases.legacyMissingVersionVerificationOnly
    );
    expect(legacy[1].version).toBeUndefined();
    const legacySigStructure = cbor.encodeCanonical([
      'Signature1',
      legacy[0],
      Buffer.alloc(0),
      legacy[2],
    ]);
    expect(verify(null, legacySigStructure, publicKey, legacy[3])).toBe(true);
    expect(
      acceptsProducedCip8Profile(
        cip8Fixture.negativeCases.legacyMissingVersionVerificationOnly
      )
    ).toBe(false);
  });

  it('preserves every data-only result envelope through structured clone', async () => {
    const validateEnvelope = ajv.getSchema(
      toAbsoluteSchemaRef('envelope.json#/definitions/resultEnvelope')
    );
    if (!validateEnvelope) throw new Error('Missing result-envelope schema');

    for (const envelope of schemaCases.envelopes) {
      expect(validateEnvelope(envelope)).toBe(true);
      const cloned = await cloneThroughMessageChannel(envelope);
      expect(cloned).toEqual(envelope);
      expect(cloned).not.toBeInstanceOf(Error);
      expect(Object.keys(cloned)).toEqual(Object.keys(envelope));
    }
  });

  it('executes JavaScript-only invocation and default rules', () => {
    const expectedNames = [
      'optional-explicit-undefined',
      'required-explicit-undefined',
      'extra-trailing-undefined',
      'nan',
      'positive-infinity',
      'function',
      'symbol',
      'bigint-number-argument',
      'inherited-enumerable-field',
      'accessor-field',
    ];
    expect(schemaCases.javascriptOnlyCases.map(({ name }) => name)).toEqual(
      expectedNames
    );

    const method = (path: string) =>
      manifest.methods.find((candidate) => candidate.path === path);
    const enable = method('provider.enable');
    const isEnabled = method('provider.isEnabled');
    const signTx = method('api.signTx');
    expect(enable && invocationIsValid(enable, [])).toBe(true);
    expect(enable && invocationIsValid(enable, [undefined])).toBe(true);
    expect(enable && invocationIsValid(enable, [{ extensions: [] }])).toBe(
      true
    );
    const enableOptions = ajv.getSchema(
      toAbsoluteSchemaRef('common.json#/definitions/enableOptions')
    );
    if (!enableOptions) throw new Error('Missing enable-options schema');
    expect(enableOptions({ extensions: [] })).toBe(true);
    expect(enableOptions({ extensions: [], extra: true })).toBe(false);
    expect(enable && invocationIsValid(enable, [undefined, undefined])).toBe(
      false
    );
    expect(isEnabled && invocationIsValid(isEnabled, [undefined])).toBe(false);
    expect(signTx && invocationIsValid(signTx, [])).toBe(false);
    expect(signTx && invocationIsValid(signTx, [undefined])).toBe(false);
    expect(signTx && invocationIsValid(signTx, ['a0'])).toBe(true);
    expect(signTx && invocationIsValid(signTx, ['a0', undefined])).toBe(true);
    expect(signTx && invocationIsValid(signTx, ['a0', false, undefined])).toBe(
      false
    );
    expect(signTx && invocationIsValid(signTx, [() => undefined])).toBe(false);
    expect(signTx && invocationIsValid(signTx, [Symbol('tx')])).toBe(false);
    const bigintValue = ((global as unknown) as {
      BigInt: CallableFunction;
    }).BigInt(1);
    expect(signTx && invocationIsValid(signTx, [bigintValue])).toBe(false);

    const inherited = Object.create({ extensions: [] });
    inherited.own = true;
    expect(enable && invocationIsValid(enable, [inherited])).toBe(false);
    const accessor = {};
    Object.defineProperty(accessor, 'extensions', { get: () => [] });
    expect(enable && invocationIsValid(enable, [accessor])).toBe(false);

    expect(enable && enable.args[0].default).toEqual({});
    expect(signTx && signTx.args[1].default).toBe(false);
  });
});
