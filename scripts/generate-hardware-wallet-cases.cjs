#!/usr/bin/env node

const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const prettier = require('prettier');
const cbor = require('cbor');

const [
  manifestPath,
  outputPath,
  evidenceSchemaPath,
  evidenceExamplesPath,
  inputRecipesPath,
] = process.argv.slice(2);
if (!manifestPath || !outputPath) {
  throw new Error(
    'Usage: generate-hardware-wallet-cases.cjs <manifest> <cases> [evidence-schema] [evidence-examples] [input-recipes]'
  );
}

const sha256 = (value) =>
  crypto.createHash('sha256').update(value).digest('hex');
const readJson = (filePath) => JSON.parse(fs.readFileSync(filePath, 'utf8'));
const formatJson = (value) => prettier.format(JSON.stringify(value), { parser: 'json' });
const manifest = readJson(manifestPath);
const root = path.resolve(path.dirname(manifestPath), '../../../../..');
const manifestDigest = sha256(fs.readFileSync(manifestPath));
const artifacts = new Map(manifest.artifacts.map((item) => [item.id, item]));
const staticRows = new Map(
  manifest.staticRows.map((item) => [`${item.artifactId}:${item.operation}`, item])
);
const coverage = new Map(
  manifest.staticFieldCoverage.map((item) => [item.artifactId, item])
);
const txFixtureSource = readJson(
  path.join(root, manifest.fixtureBindings.signTx.path)
);
const dataFixtureSource = readJson(
  path.join(root, manifest.fixtureBindings.signData.path)
);
const addressFixtureSource = readJson(
  path.join(root, manifest.fixtureBindings.signDataAddresses.path)
);
const parseLedger7MessageData = require(path.join(
  root,
  'node_modules/@cardano-foundation/ledgerjs-hw-app-cardano/dist/parsing/messageData.js'
)).parseMessageData;
const slug = (value) =>
  String(value)
    .replace(/([a-z])([A-Z])/g, '$1-$2')
    .replace(/_/g, '-')
    .toLowerCase();
const hexDigest = (value) => sha256(Buffer.from(value, 'hex'));
const canonicalize = (value) => {
  if (Array.isArray(value)) return value.map(canonicalize);
  if (value && typeof value === 'object') {
    return Object.keys(value)
      .sort()
      .reduce((result, key) => ({ ...result, [key]: canonicalize(value[key]) }), {});
  }
  return value;
};
const canonicalJsonRecipe = (kind, value, extra = {}) => {
  const canonicalJson = JSON.stringify(canonicalize(value));
  const recipe = {
    kind,
    canonicalJson,
    decodedByteLength: Buffer.byteLength(canonicalJson),
    ...extra,
  };
  return {
    ...recipe,
    inputSha256: sha256(Buffer.from(canonicalJson)),
    recipeSha256: sha256(JSON.stringify(recipe)),
  };
};
const repeatedByteRecipe = (decodedByteLength) => {
  const patternHex = 'a5';
  const bytes = Buffer.alloc(decodedByteLength, 0xa5);
  const recipe = {
    kind: 'repeated-byte-pattern',
    patternHex,
    repetitions: decodedByteLength,
    decodedByteLength,
  };
  return { ...recipe, inputSha256: sha256(bytes), recipeSha256: sha256(JSON.stringify(recipe)) };
};
const validateTransactionEnvelope = (bytes, requireCanonicalRoundTrip = false) => {
  const decoded = cbor.decodeAllSync(bytes);
  if (decoded.length !== 1 || !Array.isArray(decoded[0]) || decoded[0].length !== 4) {
    throw new Error('Transaction recipe is not one complete four-item envelope');
  }
  if (!(decoded[0][0] instanceof Map) || !(decoded[0][1] instanceof Map)) {
    throw new Error('Transaction recipe body and witness set must be maps');
  }
  if (requireCanonicalRoundTrip && !cbor.encodeCanonical(decoded[0]).equals(bytes)) {
    throw new Error('Generated transaction recipe is not canonical round-trip stable');
  }
  const validateMetadatum = (value) => {
    if (typeof value === 'string' && Buffer.byteLength(value) > 64) {
      throw new Error('Transaction metadata text exceeds 64 bytes');
    }
    if (Buffer.isBuffer(value) && value.length > 64) {
      throw new Error('Transaction metadata bytes exceed 64 bytes');
    }
    if (Array.isArray(value)) value.forEach(validateMetadatum);
    if (value instanceof Map) {
      for (const [key, item] of value) {
        validateMetadatum(key);
        validateMetadatum(item);
      }
    }
  };
  if (decoded[0][3] instanceof Map) {
    for (const value of decoded[0][3].values()) validateMetadatum(value);
  }
  return decoded[0];
};
const transactionInputRecipe = (cborHex, assertion, generated = false) => {
  const bytes = Buffer.from(cborHex, 'hex');
  validateTransactionEnvelope(bytes, generated);
  const recipeIdentity = {
    kind: 'transaction-cbor',
    cborHex,
    decodedByteLength: bytes.length,
    assertion,
    structuralValidation: {
      decoder: 'cbor.decodeAllSync',
      completeItemCount: 1,
      rootArrayItems: 4,
      bodyAndWitnessSetAreMaps: true,
      canonicalRoundTrip: generated,
      metadataTextAndBytesMaximum: 64,
    },
  };
  return {
    ...recipeIdentity,
    inputSha256: sha256(bytes),
    recipeSha256: sha256(JSON.stringify(canonicalize(recipeIdentity))),
  };
};
const transactionBoundaryRecipe = (targetLength) => {
  const source = validateTransactionEnvelope(Buffer.from(txFixtureSource.cborHex, 'hex'));
  for (let prefixEmptyItems = 0; prefixEmptyItems <= 32; prefixEmptyItems += 1) {
    let low = 0;
    let high = targetLength;
    while (low <= high) {
      const paddingLength = Math.floor((low + high) / 2);
      const chunks = Array(prefixEmptyItems).fill('');
      let remaining = paddingLength;
      while (remaining >= 23) {
        chunks.push('x'.repeat(23));
        remaining -= 23;
      }
      chunks.push('x'.repeat(remaining));
      const candidate = [...source];
      candidate[3] = new Map([[0, chunks]]);
      const bytes = cbor.encodeCanonical(candidate);
      if (bytes.length === targetLength) {
        return transactionInputRecipe(
          bytes.toString('hex'),
          {
            family: 'decoded-request-size',
            exactByteLength: targetLength,
            auxiliaryData: {
              form: 'raw-metadata-map-list',
              label: 0,
              textByte: '78',
              textRepetitions: paddingLength,
              maximumTextBytesPerItem: 23,
              prefixEmptyItems,
            },
          },
          true
        );
      }
      if (bytes.length < targetLength) low = paddingLength + 1;
      else high = paddingLength - 1;
    }
  }
  throw new Error(`Cannot construct valid ${targetLength}-byte transaction envelope`);
};
const boundaryTransactions = new Map([
  [65536, transactionBoundaryRecipe(65536)],
  [65537, transactionBoundaryRecipe(65537)],
]);
const boundaryFixtureDocument = {
  schemaVersion: 1,
  transactions: [...boundaryTransactions.entries()].map(([length, recipe]) => ({
    id: `valid-conway-${length}-bytes`,
    cborHex: recipe.cborHex,
    decodedByteLength: recipe.decodedByteLength,
    inputSha256: recipe.inputSha256,
    assertion: recipe.assertion,
    structuralValidation: recipe.structuralValidation,
  })),
};
const boundaryFixtureBytes = Buffer.from(formatJson(boundaryFixtureDocument));
const txBoundaries = new Map(
  [...boundaryTransactions.entries()].map(([length, recipe]) => {
    const identity = {
      kind: 'authoritative-transaction-fixture',
      fixturePath: 'hardware-wallet-tests/capability-matrix/input-recipes.json',
      fixtureSha256: sha256(boundaryFixtureBytes),
      fixtureEntry: `valid-conway-${length}-bytes`,
      decodedByteLength: recipe.decodedByteLength,
      inputSha256: recipe.inputSha256,
      assertion: recipe.assertion,
      structuralValidation: recipe.structuralValidation,
    };
    return [
      length,
      { ...identity, recipeSha256: sha256(JSON.stringify(canonicalize(identity))) },
    ];
  })
);
const cases = [];

if (hexDigest(txFixtureSource.cborHex) !== txFixtureSource.provenance.fixtureSha256) {
  throw new Error('task-004 cborHex does not match provenance.fixtureSha256');
}
if (txFixtureSource.id !== 'conway-regression-collateral') {
  throw new Error('Unexpected task-004 fixture id');
}
if (dataFixtureSource.name !== 'deterministic-ed25519-drep-normalization') {
  throw new Error('Unexpected task-002 fixture name');
}

const txFixture = {
  fixtureId: `task-004:${txFixtureSource.id}`,
  path: manifest.fixtureBindings.signTx.path,
  fileSha256: manifest.fixtureBindings.signTx.sha256,
  selectedInput: {
    kind: 'transaction-cbor',
    sha256: hexDigest(txFixtureSource.cborHex),
  },
  payloadSha256: null,
  identitySha256: null,
  publicKeySha256: null,
  signatureSha256: null,
  provenanceFixtureSha256: txFixtureSource.provenance.fixtureSha256,
};
const dataFixture = (message) => {
  if (message.source === 'wire-address') {
    const address = addressFixtureSource.addresses.find(
      (item) => item.name === message.addressName
    );
    if (!address) throw new Error(`Missing wire address ${message.addressName}`);
    return {
      fixtureId: `task-002-wire:${address.name}`,
      path: manifest.fixtureBindings.signDataAddresses.path,
      fileSha256: manifest.fixtureBindings.signDataAddresses.sha256,
      selectedInput: {
        kind: address.type === 14 ? 'reward-address' : 'base-address',
        sha256: hexDigest(address.raw),
      },
      payloadSha256: hexDigest(dataFixtureSource.payload),
      identitySha256: hexDigest(address.raw),
      publicKeySha256: null,
      signatureSha256: null,
      provenanceFixtureSha256: null,
    };
  }
  return {
    fixtureId: `task-002:${dataFixtureSource.name}`,
    path: manifest.fixtureBindings.signData.path,
    fileSha256: manifest.fixtureBindings.signData.sha256,
    selectedInput: {
      kind:
        message.inputField === 'drepId' ? 'drep-key-hash' : 'enterprise-address',
      sha256: hexDigest(dataFixtureSource[message.inputField]),
    },
    payloadSha256: hexDigest(dataFixtureSource.payload),
    identitySha256: hexDigest(dataFixtureSource[message.identityField]),
    publicKeySha256: hexDigest(dataFixtureSource.publicKey),
    signatureSha256: hexDigest(dataFixtureSource.signature),
    provenanceFixtureSha256: null,
  };
};

const errorOutcome = (errorCode) => {
  if (errorCode === 'APIError.InvalidRequest') return 'invalid-request';
  if (errorCode === 'APIError.InternalError') return 'internal-error';
  if (errorCode.endsWith('.UserDeclined')) return 'user-declined';
  if (errorCode.endsWith('.ProofGeneration')) return 'proof-generation';
  if (errorCode === 'discard-no-second-settlement') {
    return 'discarded-late-result';
  }
  throw new Error(`No evidence outcome for ${errorCode}`);
};
const transactionRejection = {
  outcome: 'pre-device-reject',
  errorCode: 'TxSignError.ProofGeneration',
  preDevice: true,
};
const dataExpectation = (support) =>
  ({
    outcome: support === 'representable' ? 'pre-device-reject' : 'proof-generation',
    errorCode: 'DataSignError.ProofGeneration',
    preDevice: true,
  });
const messageRequestBranch = (artifact, message) =>
  artifact.vendor === 'ledger' && message.id === 'drep-type6'
    ? 'key-hash'
    : message.requestBranch;
const requestFields = (item, message, requestBranch) => {
  const branch = item.messageRequest.branches.find(
    (entry) => entry.id === requestBranch
  );
  return {
    required: [...item.messageRequest.required, ...(branch?.required || [])],
    optional: item.messageRequest.optional,
    forbidden: branch?.forbidden || [],
  };
};
const executionLayer = (operation, category, expected, recipe, physical) => {
  if (recipe.kind === 'static-source-assertion') return 'source-probe';
  if (category === 'synthetic-golden') return 'synthetic-verifier';
  if (physical) return 'physical-device';
  if (category === 'error') return 'adapter-mock';
  if (expected.preDevice) return 'pre-device-validator';
  return operation === 'signData' ? 'physical-device' : 'adapter-mock';
};

const syntheticGoldenRecipe = () =>
  canonicalJsonRecipe('synthetic-golden', {
    operation: 'local-cose-verification',
    derivationPath: null,
    pathKeyAssociationClaim: false,
    identityHex: dataFixtureSource.drepId,
    identitySha256: hexDigest(dataFixtureSource.drepId),
    payloadHex: dataFixtureSource.payload,
    payloadSha256: hexDigest(dataFixtureSource.payload),
    publicKeyHex: dataFixtureSource.publicKey,
    publicKeySha256: hexDigest(dataFixtureSource.publicKey),
    signatureHex: dataFixtureSource.signature,
    signatureSha256: hexDigest(dataFixtureSource.signature),
    coseSign1Hex: dataFixtureSource.coseSign1,
    coseKeyHex: dataFixtureSource.coseKey,
  });

const messageRecipe = (message, artifact) => {
  const paths = {
    'payment-address': [2147485500, 2147485463, 2147483648, 0, 0],
    'stake-address': [2147485500, 2147485463, 2147483648, 2, 0],
    'drep-direct': [2147485500, 2147485463, 2147483648, 3, 0],
    'drep-type6': [2147485500, 2147485463, 2147483648, 3, 0],
  };
  const derivationPath = paths[message.id];
  const stakingPath = [2147485500, 2147485463, 2147483648, 2, 0];
  const requestBranch = messageRequestBranch(artifact, message);
  const role = message.id.startsWith('drep')
    ? 'drep'
    : message.id === 'stake-address'
    ? 'stake'
    : 'payment';
  const trezorAddressParameters =
    message.id === 'payment-address'
      ? { addressType: 0, path: derivationPath, stakingPath }
      : message.id === 'stake-address'
      ? { addressType: 14, stakingPath: derivationPath }
      : message.id === 'drep-type6'
      ? { addressType: 6, path: derivationPath }
      : null;
  const ledgerAddress =
    message.id === 'payment-address'
      ? {
          type: 0,
          params: { spendingPath: derivationPath, stakingPath },
        }
      : message.id === 'stake-address'
      ? { type: 14, params: { stakingPath: derivationPath } }
      : null;
  const requestTemplate =
    artifact.vendor === 'trezor'
      ? {
          method: 'cardanoSignMessage',
          path: derivationPath,
          payload: dataFixtureSource.payload,
          preferHexDisplay: false,
          networkId: 1,
          protocolMagic: 764824073,
          ...(trezorAddressParameters
            ? { addressParameters: trezorAddressParameters }
            : {}),
        }
      : {
          method: 'signMessage',
          signingPath: derivationPath,
          messageHex: dataFixtureSource.payload,
          hashPayload: false,
          preferHexDisplay: false,
          addressFieldType: requestBranch === 'address' ? 'address' : 'key_hash',
          ...(requestBranch === 'address'
            ? {
                address: ledgerAddress,
                network: { networkId: 1, protocolMagic: 764824073 },
              }
            : {}),
        };
  if (artifact.vendor === 'ledger') parseLedger7MessageData(requestTemplate);
  return canonicalJsonRecipe(
    'physical-message-request',
    {
      operation: 'signData',
      messageMode: message.id,
      request: {
        method: requestTemplate.method,
        credentialKind: requestBranch,
        hasBoundCredential: requestBranch === 'address',
        hashPayload: false,
      },
      credentialBinding: {
        role,
        predicate:
          message.id === 'drep-type6'
            ? 'type-6-payment-key-hash-equals-drep-key-hash'
            : message.id === 'drep-direct'
            ? 'direct-drep-key-hash-owned-by-role'
            : 'full-address-credential-owned-by-role',
      },
      payloadSha256: hexDigest(dataFixtureSource.payload),
      vendorParserValidation:
        artifact.vendor === 'ledger'
          ? 'ledger-message-parser-pass'
          : 'trezor-schema-derived-template',
    },
    {
      boundOutputsExcludedFromInputDigest: [
        'identityBytes',
        'publicKeyBytes',
        'signatureBytes',
      ],
    }
  );
};

const sourceAssertionRecipe = (artifact, category, subject) => {
  const probePath = path.join(path.dirname(manifestPath), artifact.probe);
  const probe = readJson(probePath);
  const sourceFiles = probe.evidence.sourceFiles;
  const preferred =
    category === 'certificate'
      ? sourceFiles.find((item) => /certificate/i.test(item.path))
      : category === 'nested-constraint' && /output|datum|reference|token/.test(subject.name)
      ? sourceFiles.find((item) => /output|token/i.test(item.path))
      : sourceFiles.find((item) => /public\.d\.ts|cardano\/index\.d\.ts/.test(item.path));
  const source = preferred || sourceFiles[0];
  return canonicalJsonRecipe('static-source-assertion', {
    assertionKind: category,
    inventoryEntry: subject,
    probeResult: {
      path: path.relative(root, probePath).split(path.sep).join('/'),
      sha256: sha256(fs.readFileSync(probePath)),
    },
    source: { packagePath: source.path, sha256: source.sha256 },
    execution: 'source-inventory-comparison-only',
    transactionInput: false,
    hardwareExecution: false,
  });
};

const transactionRecipe = (artifact, category, subject) => {
  const rootEnvelope = validateTransactionEnvelope(Buffer.from(txFixtureSource.cborHex, 'hex'));
  const fixtureBodyKeys = new Set(rootEnvelope[0].keys());
  const demonstrated =
    (category === 'body-field' && fixtureBodyKeys.has(subject.key)) ||
    (category === 'exact-body-family' &&
      ['root-envelope', 'set-tag-258', 'legacy-conway-forms'].includes(subject.name)) ||
    (category === 'nested-constraint' &&
      ((subject.name === 'output-format' && ['alonzo-array', 'legacy-array'].includes(subject.alternative)) ||
        (subject.name === 'output-destination' && subject.alternative === 'raw-address') ||
        (subject.name === 'datum' && subject.alternative === 'absent') ||
        (subject.name === 'reference-script' && subject.alternative === 'absent')));
  return demonstrated
    ? transactionInputRecipe(txFixtureSource.cborHex, {
        authoritativeFixtureId: txFixture.fixtureId,
        demonstratedClaim: subject,
      })
    : sourceAssertionRecipe(artifact, category, subject);
};

const physicalTransactionRecipe = (subject) =>
  canonicalJsonRecipe('physical-transaction-request', {
    operation: 'signTx',
    contextFixture: 'task-607-ordinary-ledger-context-v1',
    execution:
      subject.name === 'ordered-batch' ? 'ordered-batch' : 'single-transaction',
    ...(subject.name === 'ordered-batch'
      ? {
          itemKinds: ['ready', 'canonical-empty-witness-set', 'ready'],
          signedIndices: [0, 2],
          release: 'all-after-success',
          refusalIndices: [0, 1, 2],
        }
      : {}),
    verification: 'immutable-body-hash-and-witnesses',
  });
const caseRecipe = (artifact, operation, category, subject, message) => {
  if (category === 'synthetic-golden') return syntheticGoldenRecipe();
  if (category === 'physical-transaction') return physicalTransactionRecipe(subject);
  if (category === 'limit') {
    if (operation === 'signTx') {
      return txBoundaries.get(Number(subject.name.replace('decoded-bytes-', '')));
    }
    return repeatedByteRecipe(Number(subject.name.replace('decoded-bytes-', '')));
  }
  if (operation === 'signData' && message) return messageRecipe(message, artifact);
  if (operation === 'signTx' && ['body-field', 'certificate', 'exact-body-family', 'nested-constraint'].includes(category)) {
    return transactionRecipe(artifact, category, subject);
  }
  return canonicalJsonRecipe('deterministic-harness-input', {
    operation,
    executableHarness: category === 'error' ? 'adapter-fault-injection' : 'pre-device-capability-gate',
    subject,
    input: operation === 'signTx' ? { cborHex: txFixtureSource.cborHex } : { payloadHex: dataFixtureSource.payload },
  });
};

const physicalModelRowId = 'ledger-8-library-nano-x-app7';
const observedPhysicalRuntimeVersion = [7, 3, 0];
const positiveMessageProof = [
  'identityVerified',
  'publicKeyAssociated',
  'signatureVerified',
  'localCoseVerified',
  'returnedPublicKeyDigest',
  'returnedSignatureDigest',
];
const positiveTransactionProof = [
  'bodyHashVerified',
  'publicKeyAssociated',
  'signatureVerified',
  'returnedBodyHashDigest',
  'returnedPublicKeyDigest',
  'returnedSignatureDigest',
  'witnessCount',
];
const certificationTarget = (model, operation, category, state, inputRecipe) => {
  if (
    model.id !== physicalModelRowId ||
    state.libraryRepresentability !== 'representable' ||
    !(
      (operation === 'signData' && category === 'message-mode') ||
      (operation === 'signTx' && category === 'physical-transaction')
    )
  ) {
    return null;
  }
  const prerequisiteState = {
    libraryRepresentability: 'representable',
    deterministicProbe: 'pass',
    emulatorEvidence: state.emulatorEvidence,
    adapterImplementation: 'pass',
    physicalCertification: 'pass-from-this-reviewed-case',
    productEnablement: 'disabled-until-later-release-policy',
    reviewerDisposition: 'approved',
  };
  const transaction = operation === 'signTx';
  return {
    outcome: 'pass',
    errorCode: 'none',
    physicalExecution: true,
    inputRecipeSha256: inputRecipe.recipeSha256,
    prerequisiteState,
    prerequisiteStateSha256: sha256(JSON.stringify(canonicalize(prerequisiteState))),
    requiredProof: transaction ? positiveTransactionProof : positiveMessageProof,
    forbiddenProof: transaction
      ? [
          'identityVerified',
          'localCoseVerified',
          'vendorCosePassedThrough',
        ]
      : [
          'bodyHashVerified',
          'returnedBodyHashDigest',
          'witnessCount',
          'vendorCosePassedThrough',
        ],
  };
};

const operationState = (model, operation) => {
  const artifactState = staticRows.get(`${model.artifactId}:${operation}`);
  const dimensions = model.operationDimensions[operation];
  if (!artifactState) throw new Error(`Missing artifact operation row for ${model.id}:${operation}`);
  const intersect = (left, right, positive) =>
    left === positive && right === positive ? positive : left !== positive ? left : right;
  const state = {
    libraryRepresentability:
      artifactState.libraryRepresentability === 'representable'
        ? model.operationSupport[operation]
        : artifactState.libraryRepresentability,
    deterministicProbe:
      artifactState.deterministicProbe === 'pass'
        ? dimensions.deterministicProbe
        : artifactState.deterministicProbe,
    emulatorEvidence: intersect(
      artifactState.emulatorEvidence,
      dimensions.emulatorEvidence,
      'pass'
    ),
    physicalCertification: intersect(
      artifactState.physicalCertification,
      dimensions.physicalCertification,
      'pass'
    ),
    adapterImplementation: intersect(
      artifactState.adapterImplementation,
      dimensions.adapterImplementation,
      'pass'
    ),
    productEnablement: intersect(
      artifactState.productEnablement,
      dimensions.productEnablement,
      'enabled'
    ),
  };
  return {
    ...state,
    productUsable:
      state.libraryRepresentability === 'representable' &&
      state.deterministicProbe === 'pass' &&
      state.emulatorEvidence === 'pass' &&
      state.physicalCertification === 'pass' &&
      state.adapterImplementation === 'pass' &&
      state.productEnablement === 'enabled',
  };
};

const add = ({ model, operation, category, subject, expected, message }) => {
  const artifact = artifacts.get(model.artifactId);
  const item = coverage.get(model.artifactId);
  const support = model.operationSupport[operation];
  const fixture = operation === 'signTx' ? txFixture : dataFixture(message);
  const state = operationState(model, operation);
  const inputRecipe = caseRecipe(artifact, operation, category, subject, message);
  const effectiveExpected =
    inputRecipe.kind === 'static-source-assertion'
      ? { outcome: 'static-source-assertion', errorCode: 'none', preDevice: true }
      : expected;
  const target = certificationTarget(model, operation, category, state, inputRecipe);
  const layer = executionLayer(
    operation,
    category,
    effectiveExpected,
    inputRecipe,
    Boolean(target)
  );
  const dataPass = operation === 'signData' && effectiveExpected.outcome === 'pass';
  const transactionPass =
    operation === 'signTx' && effectiveExpected.outcome === 'pass';
  const idParts = [
    model.id,
    operation.toLowerCase(),
    category,
    subject.name,
    subject.alternative,
    subject.predicate,
    subject.support,
  ].filter(Boolean);
  cases.push({
    id: idParts.map(slug).join('-'),
    matrixRevision: manifest.revision,
    capabilityRowId: model.id,
    artifactBinding: {
      id: artifact.id,
      vendor: artifact.vendor,
      package: artifact.package,
      version: artifact.version,
      integrity: artifact.integrity,
      lockSha256: artifact.lockSha256,
      runtimeGraphSha256:
        artifact.vendor === 'trezor' ? manifest.runtimeProvenance.runtimeGraphSha256 : null,
      configIdentitySha256:
        artifact.vendor === 'trezor' ? manifest.runtimeProvenance.configIdentitySha256 : null,
    },
    operation,
    category,
    subject,
    modelBinding: {
      model: model.model,
      versionKind: model.versionKind,
      certificationVersion: model.operationGates[operation].minimumVersion,
      support,
      state,
    },
    fixtureBinding: {
      ...fixture,
      selectedInput: {
        kind:
          inputRecipe.kind === 'static-source-assertion'
            ? 'source-assertion'
            : inputRecipe.kind === 'physical-transaction-request'
            ? 'transaction-context'
            : inputRecipe.kind === 'synthetic-golden'
            ? 'synthetic-golden'
            : fixture.selectedInput.kind,
        sha256: inputRecipe.inputSha256,
      },
      ...(operation === 'signData' && category !== 'synthetic-golden'
        ? {
            identitySha256: null,
            publicKeySha256: null,
            signatureSha256: null,
          }
        : {}),
    },
    inputRecipe,
    signingBinding: {
      requestBranch:
        operation === 'signTx'
          ? 'transaction'
          : messageRequestBranch(artifact, message),
      signingMode:
        operation === 'signTx'
          ? category === 'physical-transaction'
            ? 'exact-body'
            : 'exact-body-reject'
          : 'unhashed-message',
      displayMode: effectiveExpected.preDevice
        ? 'none-pre-device-rejection'
        : 'device-confirmation-required',
      executionLayer: layer,
      requestFields:
        operation === 'signTx'
          ? inputRecipe.kind === 'static-source-assertion'
            ? {
                required: ['probeResult.path', 'source.packagePath', 'inventoryEntry'],
                optional: [],
                forbidden: ['cborHex'],
              }
            : inputRecipe.kind === 'physical-transaction-request'
            ? {
                required: ['contextFixture', 'execution', 'verification'],
                optional: ['itemKinds', 'signedIndices', 'release', 'refusalIndices'],
                forbidden: ['cborHex'],
              }
            : { required: ['cborHex'], optional: [], forbidden: [] }
          : requestFields(item, message, messageRequestBranch(artifact, message)),
    },
    expected: effectiveExpected,
    proofBinding: {
      vendorResponse: operation === 'signTx' ? item.transactionProof : item.messageProof,
      required: transactionPass
        ? positiveTransactionProof
        : dataPass
        ? positiveMessageProof
        : [],
      forbidden: transactionPass
        ? [
            'identityVerified',
            'localCoseVerified',
            'vendorCosePassedThrough',
          ]
        : dataPass
        ? ['vendorCosePassedThrough']
        : [
            'bodyHashVerified',
            'identityVerified',
            'publicKeyAssociated',
            'signatureVerified',
            'localCoseVerified',
            'returnedBodyHashDigest',
            'returnedPublicKeyDigest',
            'returnedSignatureDigest',
            'witnessCount',
            'vendorCosePassedThrough',
          ],
    },
    physicalExecution: layer === 'physical-device',
    certificationTarget: target,
    evidenceOwner:
      inputRecipe.kind === 'static-source-assertion'
        ? artifact.vendor === 'ledger'
          ? 'task-602'
          : 'task-603'
        : 'task-607',
  });
};

for (const model of manifest.modelRows) {
  const item = coverage.get(model.artifactId);
  const defaultMessage = manifest.messageRows[0];
  if (model.operationSupport.signTx === 'representable') {
    for (const key of manifest.conwayInventory.bodyKeys) {
      const field = manifest.bodyFieldMap.find((entry) => entry.key === key);
      add({
        model,
        operation: 'signTx',
        category: 'body-field',
        subject: { kind: 'body-field', name: field.name, key },
        expected: transactionRejection,
      });
    }
    for (const tag of manifest.conwayInventory.certificateTags) {
      add({
        model,
        operation: 'signTx',
        category: 'certificate',
        subject: { kind: 'certificate', name: `tag-${tag}`, tag },
        expected: transactionRejection,
      });
    }
    for (const family of manifest.exactBodyFamilies) {
      add({
        model,
        operation: 'signTx',
        category: 'exact-body-family',
        subject: { kind: 'exact-body-family', name: family },
        expected: transactionRejection,
      });
    }
    for (const constraint of item.nestedInventory.constraints.filter(
      (entry) => !entry.appMajors || entry.appMajors.includes(model.versionMajor)
    )) {
      for (const alternative of constraint.alternatives || [null]) {
        add({
          model,
          operation: 'signTx',
          category: 'nested-constraint',
          subject: {
            kind: 'nested-constraint',
            name: constraint.id,
            alternative,
            minimum: constraint.minimum ?? null,
            maximum: constraint.maximum ?? null,
            ordering: constraint.ordering ?? null,
          },
          expected: transactionRejection,
        });
      }
    }
    if (model.id === physicalModelRowId) {
      for (const name of ['single-transaction', 'ordered-batch']) {
        add({
          model,
          operation: 'signTx',
          category: 'physical-transaction',
          subject: { kind: 'physical-transaction', name },
          expected: { outcome: 'pass', errorCode: 'none', preDevice: false },
        });
      }
    }
  }

  for (const operation of ['signTx', 'signData']) {
    const support = model.operationSupport[operation];
    add({
      model,
      operation,
      category: 'model-version',
      subject: { kind: 'model-version', name: 'operation-support', support },
      message: defaultMessage,
      expected:
        operation === 'signTx' ? transactionRejection : dataExpectation(support),
    });
  }

  if (model.operationSupport.signData === 'representable') {
    for (const message of manifest.messageRows) {
      add({
        model,
        operation: 'signData',
        category: 'message-mode',
        subject: { kind: 'message-mode', name: message.id },
        message,
        expected:
          model.id === physicalModelRowId
            ? { outcome: 'pass', errorCode: 'none', preDevice: false }
            : dataExpectation('representable'),
      });
    }
    add({
      model,
      operation: 'signData',
      category: 'synthetic-golden',
      subject: { kind: 'synthetic-golden', name: 'task-002-drep-vector' },
      message: manifest.messageRows.find((item) => item.id === 'drep-direct'),
      expected: { outcome: 'pass', errorCode: 'none', preDevice: false },
    });
  }

  for (const operation of ['signTx', 'signData']) {
    if (model.operationSupport[operation] !== 'representable') continue;
    for (const error of manifest.errorContract) {
      add({
        model,
        operation,
        category: 'error',
        subject: { kind: 'error', name: 'error-contract', predicate: error.predicate },
        message: defaultMessage,
        expected: {
          outcome: errorOutcome(error[operation]),
          errorCode: error[operation],
          preDevice: [
            'APIError.InvalidRequest',
            'TxSignError.ProofGeneration',
            'DataSignError.ProofGeneration',
          ].includes(error[operation]),
        },
      });
    }
    const accepted =
      operation === 'signTx' ? transactionRejection : dataExpectation('representable');
    for (const [name, expected] of [
      ['decoded-bytes-65536', accepted],
      [
        'decoded-bytes-65537',
        { outcome: 'invalid-request', errorCode: 'APIError.InvalidRequest', preDevice: true },
      ],
    ]) {
      add({
        model,
        operation,
        category: 'limit',
        subject: { kind: 'limit', name },
        message: defaultMessage,
        expected,
      });
    }
  }
}

cases.sort((left, right) => left.id.localeCompare(right.id));
const output = { schemaVersion: 4, matrixRevision: manifest.revision, cases };
fs.writeFileSync(outputPath, formatJson(output));
if (inputRecipesPath) fs.writeFileSync(inputRecipesPath, boundaryFixtureBytes);

const artifactDigest = (artifact) =>
  artifact.tarballSha256 || `sha1:${artifact.sha1}`;
const proofConstraint = (field, required) => {
  if (field === 'witnessCount') {
    return required ? { type: 'integer', minimum: 1 } : { const: 0 };
  }
  if (field.startsWith('returned')) {
    return required ? { $ref: '#/definitions/digest' } : { type: 'null' };
  }
  if (field === 'vendorCosePassedThrough') return { const: false };
  return { const: required };
};
const runtimeVersionGate = (testCase) => {
  const model = manifest.modelRows.find(
    (item) => item.id === testCase.capabilityRowId
  );
  const minimum = testCase.modelBinding.certificationVersion;
  if (!model || !minimum) throw new Error(`Missing runtime gate for ${testCase.id}`);
  return {
    oneOf: [
      {
        type: 'array',
        minItems: 3,
        maxItems: 3,
        items: [
          { const: model.versionMajor },
          { const: minimum[1] },
          { type: 'integer', minimum: minimum[2], maximum: 65535 },
        ],
      },
      {
        type: 'array',
        minItems: 3,
        maxItems: 3,
        items: [
          { const: model.versionMajor },
          { type: 'integer', minimum: minimum[1] + 1, maximum: 65535 },
          { type: 'integer', minimum: 0, maximum: 65535 },
        ],
      },
    ],
  };
};
const evidenceBinding = (testCase) => {
  const artifact = artifacts.get(testCase.artifactBinding.id);
  const proofProperties = (required, forbidden) => {
    const properties = {};
    for (const field of required) properties[field] = proofConstraint(field, true);
    for (const field of forbidden) properties[field] = proofConstraint(field, false);
    return properties;
  };
  const sharedProperties = {
      caseId: { const: testCase.id },
      capabilityRowId: { const: testCase.capabilityRowId },
      artifactId: { const: testCase.artifactBinding.id },
      artifactDigest: { const: artifactDigest(artifact) },
      productionLockDigest: { const: testCase.artifactBinding.lockSha256 },
      configDigest: { const: manifestDigest },
      runtimeGraphSha256: {
        const: testCase.artifactBinding.runtimeGraphSha256,
      },
      configIdentitySha256: {
        const: testCase.artifactBinding.configIdentitySha256,
      },
      library: {
        properties: {
          name: { const: testCase.artifactBinding.package },
          version: { const: testCase.artifactBinding.version },
          integrity: { const: testCase.artifactBinding.integrity },
          lockDigest: { const: testCase.artifactBinding.lockSha256 },
        },
      },
      vendor: { const: testCase.artifactBinding.vendor },
      model: { const: testCase.modelBinding.model },
      versionKind: { const: testCase.modelBinding.versionKind },
      minimumVersion: { const: testCase.modelBinding.certificationVersion },
      operation: { const: testCase.operation },
      inputDigest: { const: testCase.fixtureBinding.selectedInput.sha256 },
      inputRecipeSha256: { const: testCase.inputRecipe.recipeSha256 },
      reviewDisposition: { const: 'approved' },
  };
  const required = [
      'caseId',
      'capabilityRowId',
      'artifactId',
      'artifactDigest',
      'productionLockDigest',
      'configDigest',
      'runtimeGraphSha256',
      'configIdentitySha256',
      'library',
      'vendor',
      'model',
      'versionKind',
      'minimumVersion',
      'version',
      'executionKind',
      'operation',
      'inputDigest',
      'inputRecipeSha256',
      'outcome',
      'errorCode',
      'reviewDisposition',
      'prerequisiteAttestation',
  ];
  const current = {
    properties: {
      ...sharedProperties,
      version: { const: null },
      executionKind: { const: 'mock' },
      outcome: { const: testCase.expected.outcome },
      errorCode: { const: testCase.expected.errorCode },
      prerequisiteAttestation: { type: 'null' },
      proof: {
        properties: proofProperties(
          testCase.proofBinding.required,
          testCase.proofBinding.forbidden
        ),
      },
    },
    required,
  };
  if (!testCase.certificationTarget) return [current];
  const target = testCase.certificationTarget;
  const prerequisiteAttestation = {
    libraryRepresentability: 'representable',
    deterministicProbe: 'pass',
    emulatorEvidence: testCase.modelBinding.state.emulatorEvidence,
    adapterImplementation: 'pass',
    physicalCertification: 'pass',
    productEnablement: 'disabled',
    reviewerApproved: true,
    prerequisiteStateSha256: target.prerequisiteStateSha256,
  };
  return [
    current,
    {
        properties: {
          ...sharedProperties,
          version: runtimeVersionGate(testCase),
          executionKind: { const: 'physical' },
          adapterCommit: {
            type: 'string',
            pattern: '^(?!0{40}$)[0-9a-f]{40}$',
          },
          outcome: { const: target.outcome },
          errorCode: { const: target.errorCode },
          prerequisiteAttestation: { const: prerequisiteAttestation },
          proof: {
            properties: proofProperties(
              target.requiredProof,
              target.forbiddenProof
            ),
          },
        },
        required,
    },
  ];
};

if (evidenceSchemaPath) {
  const schema = readJson(evidenceSchemaPath);
  schema.definitions.version = {
    type: 'array',
    minItems: 3,
    maxItems: 3,
    items: { type: 'integer', minimum: 0, maximum: 65535 },
  };
  schema.properties.caseId = { type: 'string', minLength: 1 };
  schema.required = [...new Set([...schema.required, 'minimumVersion', 'executionKind'])];
  schema.properties.minimumVersion = {
    oneOf: [{ $ref: '#/definitions/version' }, { type: 'null' }],
  };
  schema.properties.version = {
    oneOf: [{ $ref: '#/definitions/version' }, { type: 'null' }],
  };
  schema.properties.executionKind = { enum: ['mock', 'physical'] };
  const bindings = cases
    .filter((testCase) => testCase.evidenceOwner === 'task-607')
    .flatMap(evidenceBinding);
  const groups = [];
  while (bindings.length > 0) groups.push({ oneOf: bindings.splice(0, 50) });
  schema.allOf[0] = { oneOf: groups };
  fs.writeFileSync(evidenceSchemaPath, formatJson(schema));
}

if (evidenceExamplesPath) {
  if (!evidenceSchemaPath) throw new Error('Evidence examples require evidence schema');
  const passCase = cases.find(
    (item) =>
      item.capabilityRowId === physicalModelRowId &&
      item.operation === 'signData' &&
      item.category === 'message-mode' &&
      item.subject.name === 'drep-direct'
  );
  const mockCase = cases.find(
    (item) =>
      item.capabilityRowId === physicalModelRowId &&
      item.operation === 'signData' &&
      item.category === 'limit' &&
      item.subject.name === 'decoded-bytes-65537'
  );
  const artifact = artifacts.get(passCase.artifactBinding.id);
  const valid = {
    matrixRevision: manifest.revision,
    caseId: mockCase.id,
    capabilityRowId: mockCase.capabilityRowId,
    artifactId: artifact.id,
    artifactDigest: artifactDigest(artifact),
    productionLockDigest: artifact.lockSha256,
    configDigest: manifestDigest,
    runtimeGraphSha256: mockCase.artifactBinding.runtimeGraphSha256,
    configIdentitySha256: mockCase.artifactBinding.configIdentitySha256,
    library: {
      name: artifact.package,
      version: artifact.version,
      integrity: artifact.integrity,
      lockDigest: artifact.lockSha256,
    },
    adapterCommit: '0000000000000000000000000000000000000000',
    vendor: artifact.vendor,
    model: mockCase.modelBinding.model,
    versionKind: mockCase.modelBinding.versionKind,
    minimumVersion: mockCase.modelBinding.certificationVersion,
    version: null,
    executionKind: 'mock',
    transport: artifact.vendor === 'ledger' ? 'hid' : 'bridge',
    operation: mockCase.operation,
    inputDigest: mockCase.fixtureBinding.selectedInput.sha256,
    inputRecipeSha256: mockCase.inputRecipe.recipeSha256,
    outcome: mockCase.expected.outcome,
    errorCode: mockCase.expected.errorCode,
    prerequisiteAttestation: null,
    proof: {
      bodyHashVerified: false,
      identityVerified: false,
      publicKeyAssociated: false,
      signatureVerified: false,
      localCoseVerified: false,
      vendorCosePassedThrough: false,
      returnedBodyHashDigest: null,
      returnedPublicKeyDigest: null,
      returnedSignatureDigest: null,
      witnessCount: 0,
    },
    operatorId: 'operator-generated',
    reviewerId: 'reviewer-generated',
    reviewDisposition: 'approved',
    executedAt: '2026-08-14T12:00:00Z',
  };
  const promotedValid = {
    ...valid,
    caseId: passCase.id,
    capabilityRowId: passCase.capabilityRowId,
    minimumVersion: passCase.modelBinding.certificationVersion,
    inputDigest: passCase.fixtureBinding.selectedInput.sha256,
    inputRecipeSha256: passCase.inputRecipe.recipeSha256,
    version: observedPhysicalRuntimeVersion,
    executionKind: 'physical',
    adapterCommit: '1111111111111111111111111111111111111111',
    outcome: passCase.certificationTarget.outcome,
    errorCode: passCase.certificationTarget.errorCode,
    prerequisiteAttestation: {
      libraryRepresentability: 'representable',
      deterministicProbe: 'pass',
      emulatorEvidence: passCase.modelBinding.state.emulatorEvidence,
      adapterImplementation: 'pass',
      physicalCertification: 'pass',
      productEnablement: 'disabled',
      reviewerApproved: true,
      prerequisiteStateSha256:
        passCase.certificationTarget.prerequisiteStateSha256,
    },
    proof: {
      bodyHashVerified: false,
      identityVerified: true,
      publicKeyAssociated: true,
      signatureVerified: true,
      localCoseVerified: true,
      vendorCosePassedThrough: false,
      returnedBodyHashDigest: null,
      returnedPublicKeyDigest: sha256('device-derived-public-key'),
      returnedSignatureDigest: sha256('device-derived-signature'),
      witnessCount: 0,
    },
  };
  const invalid = [
    ['wrong-vendor', 'vendor', artifact.vendor === 'ledger' ? 'trezor' : 'ledger'],
    ['wrong-operation', 'operation', 'signTx'],
    ['wrong-artifact', 'artifactId', artifact.id === 'ledger-7.1.4' ? 'trezor-connect-9.7.2' : 'ledger-7.1.4'],
    ['wrong-model', 'model', artifact.vendor === 'ledger' ? 'trezor-t2t1' : 'ledger-nano-x'],
    ['wrong-version', 'version', [99, 0, 0]],
    ['wrong-model-row', 'capabilityRowId', 'not-the-bound-row'],
    ['reviewer-rejected-evidence', 'reviewDisposition', 'rejected'],
    ['wrong-error', 'errorCode', 'none'],
    ['unknown-case', 'caseId', 'unfrozen-case'],
    ['positive-proof-on-pre-device-rejection', 'signatureVerified', true, 'proof'],
    ['returned-digest-on-pre-device-rejection', 'returnedSignatureDigest', sha256('forbidden-returned-signature'), 'proof'],
    ['witness-on-pre-device-rejection', 'witnessCount', 1, 'proof'],
    ['vendor-cose-proof', 'vendorCosePassedThrough', true, 'proof'],
  ].map(([name, property, value, target]) => ({ name, property, value, ...(target ? { target } : {}) }));
  const promotedInvalid = [
    ['zero-adapter-commit', 'adapterCommit', '0000000000000000000000000000000000000000'],
    ['missing-prerequisites', 'prerequisiteAttestation', null],
    ['reviewer-rejected-promotion', 'reviewDisposition', 'rejected'],
    ['mock-cannot-attest-physical-pass', 'executionKind', 'mock'],
    ['missing-promoted-proof', 'signatureVerified', false, 'proof'],
  ].map(([name, property, value, target]) => ({ name, property, value, ...(target ? { target } : {}) }));
  fs.writeFileSync(
    evidenceExamplesPath,
    formatJson({ valid, promotedValid, invalid, promotedInvalid })
  );
}
