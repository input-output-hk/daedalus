#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { createHash } = require('crypto');
const prettier = require('prettier');

const args = Object.fromEntries(
  process.argv.slice(2).map((argument) => {
    const separator = argument.indexOf('=');
    if (!argument.startsWith('--') || separator === -1) {
      throw new Error(`Invalid argument: ${argument}`);
    }
    return [argument.slice(2, separator), argument.slice(separator + 1)];
  })
);

const allowedArguments = new Set([
  'vendor',
  'root',
  'output',
  'label',
  'lock',
  'runtime-output',
]);
const rejectedArgument = Object.keys(args).find((name) => !allowedArguments.has(name));
if (rejectedArgument) throw new Error(`Unsupported argument: --${rejectedArgument}`);
if (!args.vendor || !args.root || !args.output || !args.label || !args.lock) {
  throw new Error(
    'Required: --vendor, --root, --label, --output, and --lock'
  );
}

const projectRoot = path.resolve(__dirname, '..');
const packageRoot = path.resolve(args.root);
const lockPath = path.resolve(args.lock);
const read = (relativePath) =>
  fs.readFileSync(path.join(packageRoot, relativePath), 'utf8');
const exists = (relativePath) => fs.existsSync(path.join(packageRoot, relativePath));
const digest = (filePath) =>
  createHash('sha256').update(fs.readFileSync(filePath)).digest('hex');
const source = (relativePath) => ({
  path: relativePath,
  sha256: digest(path.join(packageRoot, relativePath)),
});
const relativeToProject = (filePath) =>
  path.relative(projectRoot, filePath).split(path.sep).join('/');
const verifyNpmCacheIntegrity = (integrity) => {
  const [algorithm, encodedDigest] = integrity.split('-');
  const expected = Buffer.from(encodedDigest, 'base64').toString('hex');
  const cachePath = path.join(
    process.env.npm_config_cache || path.join(require('os').homedir(), '.npm'),
    '_cacache/content-v2',
    algorithm,
    expected.slice(0, 2),
    expected.slice(2, 4),
    expected.slice(4)
  );
  if (!fs.existsSync(cachePath)) {
    throw new Error('Candidate tarball is absent from the npm content-addressed cache');
  }
  const content = fs.readFileSync(cachePath);
  if (createHash(algorithm).update(content).digest('hex') !== expected) {
    throw new Error('Candidate tarball does not match lock SRI');
  }
  return {
    sha1: createHash('sha1').update(content).digest('hex'),
    sha256: createHash('sha256').update(content).digest('hex'),
  };
};
const packageTree = (root) => {
  const files = [];
  const visit = (directory) => {
    for (const entry of fs
      .readdirSync(directory, { withFileTypes: true })
      .sort((left, right) => left.name.localeCompare(right.name))) {
      const absolutePath = path.join(directory, entry.name);
      if (entry.isDirectory()) visit(absolutePath);
      else if (entry.isFile()) {
        files.push({
          path: path.relative(root, absolutePath).split(path.sep).join('/'),
          sha256: digest(absolutePath),
        });
      } else if (entry.isSymbolicLink()) {
        files.push({
          path: path.relative(root, absolutePath).split(path.sep).join('/'),
          sha256: createHash('sha256')
            .update(`symlink:${fs.readlinkSync(absolutePath)}`)
            .digest('hex'),
        });
      }
    }
  };
  visit(root);
  return {
    algorithm: 'sha256(path\\0content-sha256\\n)',
    fileCount: files.length,
    sha256: createHash('sha256')
      .update(files.map((file) => `${file.path}\0${file.sha256}\n`).join(''))
      .digest('hex'),
  };
};
const parseYarnEntries = (lockText) =>
  lockText
    .split(/\n(?=\S)/)
    .filter((block) => block.includes('\n'))
    .map((block) => {
      const header = block.slice(0, block.indexOf('\n'));
      const selectors = Array.from(header.matchAll(/(?:^|, )"?([^",]+)"?(?=, |:$)/g), ([, value]) => value);
      const value = (field) => {
        const match = block.match(new RegExp(`^  ${field} (?:"([^"]+)"|(\\S+))$`, 'm'));
        return match && (match[1] || match[2]);
      };
      return {
        selectors,
        version: value('version'),
        resolved: value('resolved'),
        integrity: value('integrity'),
      };
    });
const parseYarnIdentity = (
  lockText,
  name,
  version,
  requested = version,
  allowMissingIntegrity = false
) => {
  const selector = `${name}@${requested}`;
  const candidates = parseYarnEntries(lockText).filter((entry) =>
    entry.selectors.includes(selector)
  );
  if (candidates.length !== 1) {
    throw new Error(`Expected one ${selector} yarn.lock entry, found ${candidates.length}`);
  }
  const entry = candidates[0];
  const value = (field) => {
    return entry[field];
  };
  const resolved = value('resolved');
  const integrity = value('integrity');
  if (
    value('version') !== version ||
    !resolved ||
    (!integrity && !allowMissingIntegrity)
  ) {
    throw new Error(`Incomplete ${name}@${version} yarn.lock identity`);
  }
  const fragment = resolved.match(/#([0-9a-f]{40})$/);
  return {
    selectors: entry.selectors,
    resolved,
    sha1: fragment ? fragment[1] : null,
    integrity,
  };
};
const readYarnCacheMetadata = (name, version, lockIdentity) => {
  if (!lockIdentity.sha1) {
    throw new Error(`Cannot bind Yarn cache without SHA-1 for ${name}@${version}`);
  }
  const metadataPath = path.join(
    process.env.YARN_CACHE_FOLDER || path.join(require('os').homedir(), '.cache/yarn/v6'),
    `npm-${name.replace(/\//g, '-')}-${version}-${lockIdentity.sha1}-integrity`,
    'node_modules',
    name,
    '.yarn-metadata.json'
  );
  const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
  if (
    metadata.manifest.version !== version ||
    metadata.remote.resolved !== lockIdentity.resolved ||
    (lockIdentity.integrity && metadata.remote.integrity !== lockIdentity.integrity)
  ) {
    throw new Error(`Installed Yarn cache metadata does not match ${name}@${version}`);
  }
  if (!metadata.remote.integrity) {
    throw new Error(`Yarn cache metadata has no SRI for ${name}@${version}`);
  }
  return metadata;
};
const verifyYarnCacheMetadata = (name, version, lockIdentity) => {
  readYarnCacheMetadata(name, version, lockIdentity);
};
const parseNpmIdentity = (lock, name, version) => {
  if (lock.lockfileVersion !== 3) throw new Error('Candidate lockfileVersion must be 3');
  const entry = lock.packages && lock.packages[`node_modules/${name}`];
  if (!entry || entry.version !== version || !entry.resolved || !entry.integrity) {
    throw new Error(`Incomplete ${name}@${version} package-lock identity`);
  }
  return { resolved: entry.resolved, sha1: null, integrity: entry.integrity };
};
const verifyLock = (packageJson) => {
  const lockText = fs.readFileSync(lockPath, 'utf8');
  let lockType;
  let lockIdentity;
  if (path.basename(lockPath) === 'yarn.lock') {
    lockType = 'yarn-v1';
    lockIdentity = parseYarnIdentity(lockText, packageJson.name, packageJson.version);
    verifyYarnCacheMetadata(packageJson.name, packageJson.version, lockIdentity);
  } else {
    lockType = 'npm-v3';
    const committedLock = JSON.parse(lockText);
    lockIdentity = parseNpmIdentity(
      committedLock,
      packageJson.name,
      packageJson.version
    );
    const installedLockPath = path.join(packageRoot, '..', '..', '.package-lock.json');
    const installedIdentity = parseNpmIdentity(
      JSON.parse(fs.readFileSync(installedLockPath, 'utf8')),
      packageJson.name,
      packageJson.version
    );
    if (JSON.stringify(installedIdentity) !== JSON.stringify(lockIdentity)) {
      throw new Error('Installed candidate identity does not match committed package-lock');
    }
    const tarball = verifyNpmCacheIntegrity(lockIdentity.integrity);
    lockIdentity.sha1 = tarball.sha1;
    lockIdentity.tarballSha256 = tarball.sha256;
  }
  if (!/^sha(1|256|384|512)-[A-Za-z0-9+/]+={0,2}$/.test(lockIdentity.integrity)) {
    throw new Error('Lock integrity is not valid SRI');
  }
  return {
    ...lockIdentity,
    lockType,
    lockPath: relativeToProject(lockPath),
    lockSha256: digest(lockPath),
    verification: {
      version: 'match',
      integrity:
        lockType === 'npm-v3'
          ? 'verified-cached-tarball-before-load'
          : 'yarn-cache-metadata-and-installed-tree-bound-before-load',
    },
  };
};
const typeFields = (types, name) => {
  const match = types.match(new RegExp(`export type ${name} = \\{([\\s\\S]*?)\\n\\};`));
  if (!match) return [];
  return Array.from(match[1].matchAll(/^\s+(\w+)\??:/gm), ([, field]) => field);
};
const typeFieldsUntil = (types, name, nextName) => {
  const start = types.indexOf(`export type ${name} =`);
  const end = types.indexOf(`export type ${nextName} =`, start);
  if (start === -1 || end === -1) return [];
  return Array.from(
    new Set(
      Array.from(
        types.slice(start, end).matchAll(/^\s+(\w+)\??:/gm),
        ([, field]) => field
      )
    )
  );
};
const enumValues = (types, name) => {
  const match = types.match(
    new RegExp(`export declare (?:const )?enum ${name} \\{([\\s\\S]*?)\\n\\}`)
  );
  if (!match) return [];
  return Array.from(match[1].matchAll(/^\s+\w+ = (\d+),?$/gm), ([, value]) =>
    Number(value)
  );
};
const enumEntries = (types, name) => {
  const match = types.match(
    new RegExp(`export declare (?:const )?enum ${name} \\{([\\s\\S]*?)\\n\\}`)
  );
  if (!match) return [];
  return Array.from(
    match[1].matchAll(/^\s+(\w+) = (?:(\d+)|"([^"]+)"),?$/gm),
    ([, key, number, string]) => ({
      key,
      value: number === undefined ? string : Number(number),
    })
  );
};
const words = (value) =>
  value
    .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
    .toLowerCase()
    .replace(/_/g, '-')
    .replace('multiple-host-name', 'multi-host')
    .replace('single-host-ip-addr', 'single-host-ip');
const schemaRequired = (schema) => [...(schema.required || [])].sort();
const schemaOptional = (schema) =>
  Object.keys(schema.properties || {})
    .filter((key) => !(schema.required || []).includes(key))
    .sort();
const schemaStringConstants = (schema) =>
  (schema.anyOf || [])
    .map((item) => item.const)
    .filter((item) => typeof item === 'string' && !/^\d+$/.test(item));
const versionTuple = (version) =>
  version === '0' ? null : version.split('.').map(Number);
const loadPublicExports = () => {
  try {
    return {
      status: 'loaded',
      names: Object.keys(require(packageRoot)).sort(),
    };
  } catch (error) {
    return {
      status: 'blocked-native-runtime',
      errorName: error instanceof Error ? error.name : 'UnknownError',
    };
  }
};
const packageJson = JSON.parse(read('package.json'));
const lockIdentity = verifyLock(packageJson);
const identity = {
  version: packageJson.version,
  resolved: lockIdentity.resolved,
  sha1: lockIdentity.sha1,
  integrity: lockIdentity.integrity,
  tarballSha256: lockIdentity.tarballSha256 || null,
  lockType: lockIdentity.lockType,
  lockPath: lockIdentity.lockPath,
  lockSha256: lockIdentity.lockSha256,
  lockVerification: lockIdentity.verification,
  installedPackageTree: packageTree(packageRoot),
};

let evidence;
if (args.vendor === 'ledger') {
  const types = read('dist/types/public.d.ts');
  const compatibilityPath = exists('dist/validation/deviceCapabilities.js')
    ? 'dist/validation/deviceCapabilities.js'
    : 'dist/interactions/getVersion.js';
  const compatibility = read(compatibilityPath);
  const ledgerSourcePaths = [
    'package.json',
    'dist/types/public.d.ts',
    compatibilityPath,
    'dist/parsing/constants.js',
    'dist/parsing/transaction.js',
    'dist/parsing/output.js',
    'dist/parsing/certificate.js',
    'dist/interactions/signMessage.js',
    'dist/interactions/serialization/txOutput.js',
    'dist/interactions/serialization/txOther.js',
    'dist/validation/requestCompatibility.js',
  ].filter(exists);
  const constants = require(path.resolve(packageRoot, 'dist/parsing/constants.js'));
  const { getCompatibility } = require(path.resolve(
    packageRoot,
    compatibilityPath
  ));
  const compatibilityFor = (major, minor) =>
    getCompatibility({ major, minor, patch: 0, flags: { isAppXS: false } });
  const appMajors = packageJson.version.startsWith('8.') ? [7, 8] : [7];
  const modelOperationGates = appMajors.map((major) => ({
    appMajor: major,
    signTx: {
      support: 'representable',
      minimumVersion: [major, 0, 0],
    },
    signData: {
      support: 'representable',
      minimumVersion:
        major === 7 && !compatibilityFor(7, 0).supportsMessageSigning
          ? [7, 1, 0]
          : [major, 0, 0],
    },
  }));
  const enumAlternatives = (name) => enumEntries(types, name).map((item) => words(item.key));
  const voterNames = enumAlternatives('VoterType');
  const nestedInventory = {
    constraints: [
      { id: 'output-format', alternatives: enumAlternatives('TxOutputFormat').map((item) => item.replace('array-legacy', 'alonzo-array').replace('map-babbage', 'babbage-map')) },
      { id: 'output-destination', alternatives: enumAlternatives('TxOutputDestinationType').map((item) => item.replace('third-party', 'raw-address')) },
      { id: 'datum', alternatives: ['absent', ...enumAlternatives('DatumType')] },
      { id: 'reference-script', alternatives: ['absent', 'present'] },
      { id: 'token-policy', minimum: 0, maximum: constants.ASSET_GROUPS_MAX, ordering: 'canonical-policy-id' },
      { id: 'tokens-per-policy', minimum: 1, maximum: constants.TOKENS_IN_GROUP_MAX, ordering: 'canonical-asset-name' },
      { id: 'drep', alternatives: enumAlternatives('DRepParamsType').map((item) => item === 'key-path' ? 'path' : item) },
      { id: 'required-signer', alternatives: enumAlternatives('TxRequiredSignerType').map((item) => item.replace('hash', 'key-hash')) },
      { id: 'voter', alternatives: voterNames },
      ...appMajors.flatMap((major) => [
        { id: 'voters', minimum: 0, maximum: compatibilityFor(major, major === 7 ? 1 : 0).supportsMultipleVoters ? 4294967295 : 1, ...(appMajors.length > 1 ? { appMajors: [major] } : {}) },
        { id: 'votes-per-voter', minimum: 1, maximum: compatibilityFor(major, major === 7 ? 1 : 0).supportsMultipleVotesPerVoter ? 4294967295 : 1, ...(appMajors.length > 1 ? { appMajors: [major] } : {}) },
      ]),
      { id: 'pool-owner', alternatives: enumAlternatives('PoolOwnerType'), minimum: 0, maximum: constants.POOL_REGISTRATION_OWNERS_MAX },
      { id: 'pool-relay', alternatives: enumAlternatives('RelayType').map((item) => item.replace('single-host-hostname', 'single-host-name')), minimum: 0, maximum: constants.POOL_REGISTRATION_RELAYS_MAX },
    ],
  };
  const publicExports = loadPublicExports();
  const deepImports = {
    address: exists('dist/utils/address.js'),
    internalTypes: exists('dist/types/internal.js'),
  };
  if (!deepImports.address || !deepImports.internalTypes) {
    throw new Error('Required Daedalus Ledger deep import is missing');
  }
  require(path.resolve(packageRoot, 'dist/utils/address.js'));
  require(path.resolve(packageRoot, 'dist/types/internal.js'));
  evidence = {
    publicEntry: packageJson.main,
    typesEntry: packageJson.types,
    compatibilityPath,
    publicExports,
    deepImports,
    sourceFiles: ledgerSourcePaths.map(source),
    normalizedContract: {
      modelOperationGates,
      nestedInventory,
      messageRequest: {
        required: ['messageHex', 'signingPath', 'hashPayload', 'addressFieldType'],
        optional: ['preferHexDisplay'],
        branches: [
          { id: 'address', required: ['address', 'network'], forbidden: [] },
          { id: 'key-hash', required: [], forbidden: ['address', 'network'] },
        ],
      },
      transactionProof: {
        required: typeFields(types, 'SignedTransactionData'),
        witness: {
          required: typeFields(types, 'Witness'),
          minimum: 1,
          keyAssociation: 'path-public-key',
        },
        auxiliarySupplement: {
          nullable: true,
          requiredWhenPresent: typeFields(types, 'TxAuxiliaryDataSupplement'),
        },
      },
      messageProof: {
        required: typeFields(types, 'SignedMessageData'),
        keyAssociation: 'signing-path-public-key-address-field',
        signature: 'ed25519',
        localCoseRequired: true,
      },
    },
    inventory: {
      transactionFields: typeFields(types, 'Transaction'),
      certificateTags: enumValues(types, 'CertificateType'),
      messageFields: typeFieldsUntil(types, 'MessageData', 'CIP36Vote'),
      signedTransactionFields: typeFields(types, 'SignedTransactionData'),
      signedMessageFields: typeFields(types, 'SignedMessageData'),
      outputAlternatives: ['TxOutputAlonzo', 'TxOutputBabbage'],
      credentialAlternatives: enumValues(types, 'CredentialParamsType'),
      voterAlternatives: enumValues(types, 'VoterType'),
    },
    capabilities: {
      signTransaction: types.includes('SignTransactionRequest'),
      signMessage: types.includes('MessageData'),
      returnedTransactionHash: types.includes('txHashHex: string'),
      alonzoOutput: types.includes('TxOutputAlonzo'),
      babbageOutput: types.includes('TxOutputBabbage'),
      inlineDatum: types.includes('DatumType.INLINE'),
      referenceScript: types.includes('referenceScriptHex'),
      conwayCertificates: types.includes('DRepRegistrationParams'),
      votingProcedures: types.includes('votingProcedures?'),
      proposalProcedures: types.includes('proposalProcedures?'),
      treasury: types.includes('treasury?'),
      donation: types.includes('donation?'),
      multipleVoters: compatibility.includes('supportsMultipleVoters: true'),
      multipleVotesPerVoter: compatibility.includes(
        'supportsMultipleVotesPerVoter: true'
      ),
      appMajor8: compatibility.includes('isV8App'),
      unrestrictedTransaction: types.includes('UNRESTRICTED_TRANSACTION'),
    },
  };
} else if (args.vendor === 'trezor') {
  const types = read('lib/types/api/cardano/index.d.ts');
  const factory = read('lib/factory.js');
  const message = read('lib/api/cardano/api/cardanoSignMessage.js');
  const cardanoSchemas = require(path.resolve(packageRoot, 'lib/types/api/cardano/index.js'));
  const { config } = require(path.resolve(packageRoot, 'lib/data/config.js'));
  const { DEFAULT_FIRMWARE_RANGE } = require(path.resolve(packageRoot, 'lib/core/AbstractMethod.js'));
  const { getFirmwareRange } = require(path.resolve(packageRoot, 'lib/api/common/paramsValidator.js'));
  const { getMiscNetwork } = require(path.resolve(packageRoot, 'lib/data/coinInfo.js'));
  const modelNames = Object.keys(DEFAULT_FIRMWARE_RANGE).filter((model) => model !== 'UNKNOWN');
  const operationGate = (method, model) => {
    const explicitRule = config.supportedFirmware.find((rule) => rule.methods && rule.methods.includes(method));
    const explicitMinimum = explicitRule && explicitRule.min && explicitRule.min[model];
    const computed = getFirmwareRange(method, getMiscNetwork('Cardano'), DEFAULT_FIRMWARE_RANGE)[model].min;
    if (explicitMinimum === '0') return { support: 'not_representable', minimumVersion: null };
    if (method === 'cardanoSignMessage' && !explicitMinimum) {
      return { support: 'unresolved', minimumVersion: null };
    }
    return { support: 'representable', minimumVersion: versionTuple(computed) };
  };
  const modelOperationGates = modelNames.map((model) => ({
    model: `trezor-${model.toLowerCase()}`,
    signTx: operationGate('cardanoSignTransaction', model),
    signData: operationGate('cardanoSignMessage', model),
  }));
  const outputSchema = cardanoSchemas.CardanoOutput.allOf[1].properties;
  const addressSchema = cardanoSchemas.CardanoAddressParameters.properties;
  const poolSchema = cardanoSchemas.CardanoPoolParameters.properties;
  const maximum = 4294967295;
  const nestedInventory = {
    constraints: [
      { id: 'output-format', alternatives: schemaStringConstants(outputSchema.format).map(words).map((item) => item.replace('array-legacy', 'legacy-array').replace('map-babbage', 'babbage-map')) },
      { id: 'output-destination', alternatives: cardanoSchemas.CardanoOutput.allOf[0].anyOf.map((item) => Object.keys(item.properties)[0]).map((item) => item === 'address' ? 'raw-address' : 'device-owned-address-parameters') },
      { id: 'address', alternatives: Object.keys(addressSchema).filter((key) => key !== 'addressType').map((key) => key === 'path' ? 'payment-path' : words(key)) },
      { id: 'datum', alternatives: ['absent', ...(outputSchema.datumHash ? ['hash'] : []), ...(outputSchema.inlineDatum ? ['inline'] : [])] },
      { id: 'reference-script', alternatives: ['absent', ...(outputSchema.referenceScript ? ['present'] : [])] },
      { id: 'token-policy', minimum: 0, maximum, ordering: 'canonical-policy-id' },
      { id: 'tokens-per-policy', minimum: 0, maximum, ordering: 'canonical-asset-name' },
      { id: 'withdrawal', alternatives: Object.keys(cardanoSchemas.CardanoWithdrawal.properties).filter((key) => key !== 'amount').map(words) },
      { id: 'required-signer', alternatives: Object.keys(cardanoSchemas.CardanoRequiredSigner.properties).map(words).map((item) => item === 'key-path' ? 'path' : item) },
      { id: 'certificate', alternatives: cardanoSchemas.CardanoCertificate.properties.type.anyOf.filter((item) => typeof item.const === 'string' && !/^\d+$/.test(item.const)).map((item) => words(item.const).replace('stake-pool-registration', 'pool-registration')) },
      { id: 'drep', alternatives: schemaStringConstants(cardanoSchemas.CardanoDRep.properties.type).map(words) },
      { id: 'pool-owner', alternatives: Object.keys(poolSchema.owners.items.properties).map(words), minimum: 0, maximum },
      { id: 'pool-relay', alternatives: schemaStringConstants(poolSchema.relays.items.properties.type).map(words), minimum: 0, maximum },
      { id: 'cip36-vote-key', alternatives: ['vote-public-key', 'delegations'].filter((item) => cardanoSchemas.CardanoCVoteRegistrationParameters.properties[item === 'vote-public-key' ? 'votePublicKey' : item]) },
      { id: 'cip36-payment', alternatives: ['payment-address-parameters', 'payment-address'].filter((item) => cardanoSchemas.CardanoCVoteRegistrationParameters.properties[item.replace(/-([a-z])/g, (_, letter) => letter.toUpperCase())]) },
    ],
  };
  const publicExports = loadPublicExports();
  const schemaKeys = (schema) => Object.keys(schema.properties || {}).sort();
  const certificateTags = cardanoSchemas.CardanoCertificate.properties.type.anyOf
    .map((item) => item.const)
    .filter((item) => Number.isInteger(item))
    .sort((left, right) => left - right);
  evidence = {
    publicEntry: packageJson.main,
    typesEntry: 'lib/types/api/cardano/index.d.ts',
    publicExports,
    sourceFiles: [
      source('package.json'),
      source('lib/types/api/cardano/index.d.ts'),
      source('lib/types/api/cardano/index.js'),
      source('lib/data/config.js'),
      source('lib/core/AbstractMethod.js'),
      source('lib/api/common/paramsValidator.js'),
      source('lib/data/coinInfo.js'),
      source('lib/constants/index.js'),
      source('lib/api/cardano/api/cardanoSignMessage.js'),
      source('lib/api/cardano/api/cardanoSignTransaction.js'),
      source('lib/api/cardano/cardanoAddressParameters.js'),
      source('lib/api/cardano/cardanoCertificate.js'),
      source('lib/api/cardano/cardanoOutputs.js'),
      source('lib/api/cardano/cardanoTokenBundle.js'),
      source('lib/api/cardano/cardanoWitnesses.js'),
    ],
    normalizedContract: {
      modelOperationGates,
      nestedInventory,
      messageRequest: {
        required: schemaRequired(cardanoSchemas.CardanoSignMessage),
        optional: schemaOptional(cardanoSchemas.CardanoSignMessage),
        branches: [],
      },
      transactionProof: {
        required: schemaRequired(cardanoSchemas.CardanoSignedTxData),
        optional: schemaOptional(cardanoSchemas.CardanoSignedTxData),
        witness: {
          required: schemaRequired(cardanoSchemas.CardanoSignedTxWitness),
          optional: schemaOptional(cardanoSchemas.CardanoSignedTxWitness),
          minimum: 1,
          keyAssociation: 'witness-request-path-public-key',
        },
        auxiliarySupplement: {
          nullable: true,
          requiredWhenPresent: schemaRequired(cardanoSchemas.CardanoAuxiliaryDataSupplement),
          optionalWhenPresent: schemaOptional(cardanoSchemas.CardanoAuxiliaryDataSupplement),
        },
      },
      messageProof: {
        required: schemaRequired(cardanoSchemas.CardanoSignedMessage),
        keyAssociation: 'path-public-key-protected-address',
        signature: 'ed25519',
        localCoseRequired: true,
        vendorCoseAccepted: false,
      },
    },
    inventory: {
      transactionFields: schemaKeys(cardanoSchemas.CardanoSignTransaction),
      certificateTags,
      messageFields: schemaKeys(cardanoSchemas.CardanoSignMessage),
      signedTransactionFields: schemaKeys(cardanoSchemas.CardanoSignedTxData),
      signedMessageFields: schemaKeys(cardanoSchemas.CardanoSignedMessage),
      outputAlternatives: ['legacy-array', 'babbage-map'],
      credentialAlternatives: ['path', 'key-hash', 'script-hash'],
      voterAlternatives: [],
    },
    capabilities: {
      signTransaction: factory.includes('cardanoSignTransaction'),
      signMessage: factory.includes('cardanoSignMessage'),
      returnedTransactionHash: types.includes('hash:'),
      alonzoOutput: types.includes('datumHash'),
      babbageOutput: types.includes('inlineDatum'),
      referenceScript: types.includes('referenceScript'),
      collateralReturn: types.includes('collateralReturn'),
      totalCollateral: types.includes('totalCollateral'),
      referenceInputs: types.includes('referenceInputs'),
      votingProcedures: types.includes('votingProcedures'),
      proposalProcedures: types.includes('proposalProcedures'),
      treasury: types.includes('treasury'),
      donation: types.includes('donation'),
      vendorCoseReturned: types.includes('coseSignature'),
      rawMessagePayloadReturned: types.includes('payload'),
      rawMessagePublicKeyReturned: types.includes('pubKey'),
      rawMessageSignatureReturned: types.includes('signature'),
      messageMethodLoads: message.includes('CardanoSignMessage'),
      returnedHeadersIncludeVersion: message.includes(
        'version: CardanoSignMessage.VERSION'
      ),
      encodedCoseUnprotectedMapIncludesVersion: message.includes(
        "new Map().set('hashed', false).set('version'"
      ),
    },
  };
} else {
  throw new Error(`Unsupported vendor: ${args.vendor}`);
}

if (args.vendor === 'trezor' && args['runtime-output']) {
  if (path.basename(lockPath) !== 'yarn.lock') {
    throw new Error('Trezor runtime graph requires a Yarn v1 lock');
  }
  const yarnLockText = fs.readFileSync(lockPath, 'utf8');
  const nodes = [];
  const edges = [];
  const visited = new Map();
  const packageRootFromEntry = (entryPath, expectedName) => {
    let directory = fs.statSync(entryPath).isDirectory()
      ? entryPath
      : path.dirname(entryPath);
    for (;;) {
      const metadataPath = path.join(directory, 'package.json');
      if (fs.existsSync(metadataPath)) {
        const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
        if (metadata.name === expectedName && metadata.version) return directory;
      }
      const parent = path.dirname(directory);
      if (parent === directory) throw new Error(`Cannot locate package for ${entryPath}`);
      directory = parent;
    }
  };
  const resolveDependency = (name, fromRoot) => {
    try {
      return packageRootFromEntry(require.resolve(`${name}/package.json`, { paths: [fromRoot] }), name);
    } catch (_error) {
      return packageRootFromEntry(require.resolve(name, { paths: [fromRoot] }), name);
    }
  };
  const visit = (root, requestedName, requestedSelector) => {
    const realRoot = fs.realpathSync(root);
    const metadata = JSON.parse(fs.readFileSync(path.join(realRoot, 'package.json'), 'utf8'));
    if (metadata.name !== requestedName) {
      throw new Error(`Resolved ${requestedName} to unexpected package ${metadata.name}`);
    }
    const lock = parseYarnIdentity(
      yarnLockText,
      metadata.name,
      metadata.version,
      requestedSelector,
      true
    );
    const integrity =
      lock.integrity ||
      (lock.sha1
        ? `sha1-${Buffer.from(lock.sha1, 'hex').toString('base64')}`
        : null);
    if (!lock.resolved || !integrity || !/^sha(1|256|384|512)-[A-Za-z0-9+/]+={0,2}$/.test(integrity)) {
      throw new Error(`Incomplete runtime lock identity for ${metadata.name}@${requestedSelector}`);
    }
    const runtimeLockIdentity = {
      resolved: lock.resolved,
      sha1: lock.sha1,
      integrity,
      integritySource: lock.integrity ? 'yarn.lock' : 'yarn.lock-resolved-sha1',
    };
    if (visited.has(realRoot)) {
      const existing = visited.get(realRoot);
      if (
        JSON.stringify(existing.lockIdentity) !==
        JSON.stringify(runtimeLockIdentity)
      ) {
        throw new Error(`Conflicting lock identities resolve to ${realRoot}`);
      }
      existing.node.lockSelectors.push(`${requestedName}@${requestedSelector}`);
      return existing.node.id;
    }
    const node = `${metadata.name}@${metadata.version}:${relativeToProject(realRoot)}`;
    const nodeRecord = {
      id: node,
      name: metadata.name,
      version: metadata.version,
      path: relativeToProject(realRoot),
      lockSelectors: [`${requestedName}@${requestedSelector}`],
      lockIdentity: runtimeLockIdentity,
      packageTree: packageTree(realRoot),
    };
    visited.set(realRoot, {
      node: nodeRecord,
      lockIdentity: runtimeLockIdentity,
    });
    nodes.push(nodeRecord);
    const dependencies = {
      ...(metadata.dependencies || {}),
      ...(metadata.optionalDependencies || {}),
    };
    for (const name of Object.keys(dependencies).sort()) {
      let dependencyRoot = null;
      try {
        dependencyRoot = resolveDependency(name, realRoot);
      } catch (error) {
        if (!metadata.optionalDependencies || !metadata.optionalDependencies[name]) {
          throw error;
        }
      }
      if (dependencyRoot) {
        const target = visit(dependencyRoot, name, dependencies[name]);
        edges.push({ from: node, dependency: name, to: target });
      }
    }
    return node;
  };
  const connectNode = visit(packageRoot, packageJson.name, packageJson.version);
  const rootTransportRoot = resolveDependency('@trezor/transport', projectRoot);
  const connectTransportRoot = resolveDependency('@trezor/transport', packageRoot);
  const rootTransport = JSON.parse(
    fs.readFileSync(path.join(rootTransportRoot, 'package.json'), 'utf8')
  );
  const connectTransport = JSON.parse(
    fs.readFileSync(path.join(connectTransportRoot, 'package.json'), 'utf8')
  );
  const sortedNodes = nodes
    .map((node) => ({
      ...node,
      lockSelectors: [...new Set(node.lockSelectors)].sort(),
    }))
    .sort((left, right) => left.id.localeCompare(right.id));
  const sortedEdges = edges.sort((left, right) =>
    `${left.from}\0${left.dependency}\0${left.to}`.localeCompare(
      `${right.from}\0${right.dependency}\0${right.to}`
    )
  );
  const runtime = {
    schemaVersion: 3,
    rootLockSha256: digest(lockPath),
    root: connectNode,
    graphSha256: createHash('sha256')
      .update(JSON.stringify({ nodes: sortedNodes, edges: sortedEdges }))
      .digest('hex'),
    configIdentity: {
      files: evidence.sourceFiles.filter((file) =>
        [
          'lib/data/config.js',
          'lib/core/AbstractMethod.js',
          'lib/api/common/paramsValidator.js',
          'lib/data/coinInfo.js',
          'lib/constants/index.js',
          'lib/types/api/cardano/index.js',
          'lib/api/cardano/api/cardanoSignMessage.js',
          'lib/api/cardano/api/cardanoSignTransaction.js',
        ].includes(file.path)
      ),
      sha256: createHash('sha256')
        .update(
          evidence.sourceFiles
            .filter((file) =>
              [
                'lib/data/config.js',
                'lib/core/AbstractMethod.js',
                'lib/api/common/paramsValidator.js',
                'lib/data/coinInfo.js',
                'lib/constants/index.js',
                'lib/types/api/cardano/index.js',
                'lib/api/cardano/api/cardanoSignMessage.js',
                'lib/api/cardano/api/cardanoSignTransaction.js',
              ].includes(file.path)
            )
            .map((file) => `${file.path}\0${file.sha256}\n`)
            .join('')
        )
        .digest('hex'),
    },
    nodes: sortedNodes,
    edges: sortedEdges,
    transportResolution: {
      daedalusRoot: {
        version: rootTransport.version,
        path: relativeToProject(rootTransportRoot),
      },
      connectRoot: {
        version: connectTransport.version,
        path: relativeToProject(connectTransportRoot),
      },
      distinctInstallations: rootTransportRoot !== connectTransportRoot,
    },
  };
  fs.writeFileSync(
    args['runtime-output'],
    prettier.format(JSON.stringify(runtime), { parser: 'json' })
  );
}

const result = {
  schemaVersion: 1,
  label: args.label,
  vendor: args.vendor,
  package: {
    name: packageJson.name,
    version: packageJson.version,
    nodeEngine: packageJson.engines ? packageJson.engines.node || null : null,
  },
  identity,
  evidence,
};

fs.writeFileSync(args.output, prettier.format(JSON.stringify(result), { parser: 'json' }));
