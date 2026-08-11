const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const cbor = require('cbor');

const argument = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0 || !process.argv[index + 1]) throw new Error(`Missing ${name}`);
  return process.argv[index + 1];
};

const sdkRoot = path.resolve(argument('--sdk-root'));
const manifestPath = path.resolve(argument('--manifest'));
const outputPath = path.resolve(argument('--output'));
const moduleRoot = argument('--label');
const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
const fixtureDirectory = path.dirname(manifestPath);
const packageJson = JSON.parse(
  fs.readFileSync(path.join(sdkRoot, 'package.json'), 'utf8')
);
const expectedPackage = [
  manifest.dependencyDecision.installed,
  manifest.dependencyDecision.candidate,
].find(({ version }) => version === packageJson.version);
if (!expectedPackage)
  throw new Error('SDK version is not declared in the manifest');

for (const property of ['version', 'gitHead']) {
  if (packageJson[property] !== expectedPackage[property]) {
    throw new Error(`SDK ${property} does not match the manifest`);
  }
}

let dependencyLockSha256 = null;
if (packageJson.version === manifest.dependencyDecision.candidate.version) {
  const lock = fs.readFileSync(
    path.resolve(sdkRoot, '../../..', 'package-lock.json')
  );
  dependencyLockSha256 = crypto.createHash('sha256').update(lock).digest('hex');
  if (dependencyLockSha256 !== expectedPackage.packageLockSha256) {
    throw new Error('Candidate dependency lock does not match the manifest');
  }
}

const sdk = require(path.join(sdkRoot, packageJson.main));
if (!sdk.Serialization || !sdk.Serialization.Transaction) {
  throw new Error('SDK does not expose Serialization.Transaction');
}

const fixtures = new Map(
  manifest.fixtures.map(({ id, file, expected }) => [
    id,
    {
      ...JSON.parse(fs.readFileSync(path.join(fixtureDirectory, file), 'utf8')),
      expected,
    },
  ])
);

const mutate = (fixture, mutation) => {
  const bytes = Buffer.from(fixture.cborHex, 'hex');
  if (mutation === 'none') return bytes;
  if (mutation === 'append-zero')
    return Buffer.concat([bytes, Buffer.from([0])]);
  if (mutation === 'indefinite-root') {
    return Buffer.concat([
      Buffer.from([0x9f]),
      bytes.subarray(1),
      Buffer.from([0xff]),
    ]);
  }
  if (
    mutation === 'insert-duplicate-fee' ||
    mutation === 'insert-unknown-body-key'
  ) {
    const mapHeader = bytes[fixture.expected.body.start];
    const pair =
      mutation === 'insert-duplicate-fee'
        ? Buffer.from('180200', 'hex')
        : Buffer.from('0c00', 'hex');
    return Buffer.concat([
      bytes.subarray(0, fixture.expected.body.start),
      Buffer.from([mapHeader + 1]),
      bytes.subarray(
        fixture.expected.body.start + 1,
        fixture.expected.body.end
      ),
      pair,
      bytes.subarray(fixture.expected.body.end),
    ]);
  }
  if (mutation === 'replace-first-set-tag') {
    const result = Buffer.from(bytes);
    const index = result.indexOf(Buffer.from('d90102', 'hex'));
    if (index < 0) throw new Error('set tag not found');
    result[index + 2] = 0x03;
    return result;
  }
  throw new Error(`Unknown mutation ${mutation}`);
};

const cases = manifest.sdkCases
  .map((testCase) => {
    const fixture = fixtures.get(testCase.fixtureId);
    if (!fixture) throw new Error(`Unknown fixture ${testCase.fixtureId}`);
    const bytes = mutate(fixture, testCase.mutation);
    const cborHex = bytes.toString('hex');
    let measuredFullConsumption = false;
    try {
      measuredFullConsumption = cbor.decodeAllSync(bytes).length === 1;
    } catch (_error) {
      measuredFullConsumption = false;
    }
    const result = {
      id: testCase.id,
      strictExpected: testCase.strictExpected,
      measuredFullConsumption,
      parse: 'rejected',
      preservedExact: null,
      toCbor: null,
      toCore: null,
      representedFields: [],
      errorClass: null,
    };
    try {
      const transaction = sdk.Serialization.Transaction.fromCbor(cborHex);
      result.parse = 'accepted';
      const serialized = transaction.toCbor();
      result.preservedExact = serialized === cborHex;
      result.toCbor = result.preservedExact ? 'exact' : 'changed';
      try {
        const core = transaction.toCore();
        result.toCore = 'accepted';
        result.representedFields = Object.entries(core.body || {})
          .filter(([, value]) => value !== undefined && value !== null)
          .map(([field]) => field)
          .sort();
      } catch (error) {
        result.toCore = 'rejected';
        result.errorClass =
          error && error.constructor ? error.constructor.name : 'UnknownError';
      }
    } catch (error) {
      result.errorClass =
        error && error.constructor ? error.constructor.name : 'UnknownError';
    }
    return result;
  })
  .sort((left, right) => left.id.localeCompare(right.id));

const result = {
  schemaVersion: 1,
  runtime: { node: process.version },
  sdk: {
    name: packageJson.name,
    version: packageJson.version,
    gitHead: packageJson.gitHead,
    tarball: expectedPackage.tarball,
    shasum: expectedPackage.shasum,
    integrity: expectedPackage.integrity,
    dependencyLockSha256,
    moduleRoot,
  },
  cases,
  summary: {
    accepted: cases.filter(({ parse }) => parse === 'accepted').length,
    rejected: cases.filter(({ parse }) => parse === 'rejected').length,
    strictRejectsAcceptedBySdk: cases.filter(
      ({ parse, strictExpected }) =>
        parse === 'accepted' && strictExpected === 'reject'
    ).length,
  },
};

fs.writeFileSync(outputPath, `${JSON.stringify(result, null, 2)}\n`);
