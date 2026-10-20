import { createHash, createPublicKey, verify } from 'crypto';
import { execFileSync, execSync } from 'child_process';
import fs from 'fs';
import os from 'os';
import path from 'path';
import Ajv from 'ajv';
import cbor from 'cbor';

const root = path.resolve(__dirname, '../../..');
const matrixRoot = path.join(__dirname, 'fixtures/capability-matrix');
const currentLock = require('@yarnpkg/lockfile').parse(
  fs.readFileSync(path.join(root, 'yarn.lock'), 'utf8')
).object;

const readJson = (filePath: string) =>
  JSON.parse(fs.readFileSync(filePath, 'utf8'));
const trezorSelector = `@trezor/connect@${
  readJson(path.join(root, 'package.json')).dependencies['@trezor/connect']
}`;
const trezorLock = currentLock[trezorSelector];
const ledgerPackage = '@cardano-foundation/ledgerjs-hw-app-cardano';
const ledgerSelector = `${ledgerPackage}@${
  readJson(path.join(root, 'package.json')).dependencies[ledgerPackage]
}`;
const ledgerLock = currentLock[ledgerSelector];
const digest = (filePath: string) =>
  createHash('sha256').update(fs.readFileSync(filePath)).digest('hex');
const canonicalInventory = (value) => {
  if (Array.isArray(value)) {
    return value
      .map(canonicalInventory)
      .sort((left, right) =>
        JSON.stringify(left).localeCompare(JSON.stringify(right))
      );
  }
  if (value && typeof value === 'object') {
    return Object.entries(value)
      .sort(([left], [right]) => left.localeCompare(right))
      .reduce(
        (result, [key, item]) => ({
          ...result,
          [key]: canonicalInventory(item),
        }),
        {}
      );
  }
  return value;
};
const manifest = readJson(path.join(matrixRoot, 'manifest.json'));
const generatorPath = path.join(
  root,
  'scripts/generate-hardware-wallet-cases.cjs'
);
const generatedCasesPath = path.join(
  os.tmpdir(),
  `daedalus-hardware-cases-${process.pid}.json`
);
execFileSync(process.execPath, [
  generatorPath,
  path.join(matrixRoot, 'manifest.json'),
  generatedCasesPath,
]);
const casesDocument = readJson(generatedCasesPath);

const validState = (state) => {
  if (
    ['not_representable', 'unresolved'].includes(state.libraryRepresentability)
  ) {
    return (
      state.deterministicProbe !== 'pass' &&
      state.physicalCertification !== 'pass' &&
      state.adapterImplementation !== 'pass' &&
      state.productEnablement !== 'enabled'
    );
  }
  if (state.deterministicProbe === 'fail') {
    return (
      state.physicalCertification !== 'pass' &&
      state.adapterImplementation !== 'pass' &&
      state.productEnablement !== 'enabled'
    );
  }
  if (state.productEnablement === 'enabled') {
    return (
      state.libraryRepresentability === 'representable' &&
      state.deterministicProbe === 'pass' &&
      state.emulatorEvidence === 'pass' &&
      state.physicalCertification === 'pass' &&
      state.adapterImplementation === 'pass'
    );
  }
  if (state.adapterImplementation === 'pass') {
    return (
      state.libraryRepresentability === 'representable' &&
      state.deterministicProbe === 'pass'
    );
  }
  return true;
};

describe('hardware capability matrix', () => {
  it('validates the closed manifest and all task-004 families', () => {
    const schema = readJson(path.join(matrixRoot, 'manifest.schema.json'));
    expect(new Ajv({ allErrors: true }).validate(schema, manifest)).toBe(true);
    const exactCbor = readJson(
      path.join(root, manifest.normativeReferences.exactCbor)
    );
    expect(manifest.conwayInventory.bodyKeys).toEqual(
      exactCbor.inventory.conway.bodyKeys
    );
    expect(manifest.conwayInventory.certificateTags).toEqual(
      exactCbor.inventory.conway.certificateTags
    );
    expect(new Set(manifest.exactBodyFamilies)).toEqual(
      new Set([
        ...exactCbor.wirePolicy.map((policy) => policy.family),
        'auxiliary-data-hash',
        'nested-semantic-order-and-cardinality',
      ])
    );
    for (const disposition of manifest.exactBodyDispositions) {
      expect(disposition.operation).toBe('signTx');
      expect(Object.keys(disposition.families).sort()).toEqual(
        [...manifest.exactBodyFamilies].sort()
      );
      expect(new Set(Object.values(disposition.families))).toEqual(
        new Set(['reject_pre_device'])
      );
      expect(disposition.physicalReturnedHashRequired).toBe(false);
    }
  });

  it('derives every body field, certificate tag, and response inventory from artifacts', () => {
    for (const coverage of manifest.staticFieldCoverage) {
      const artifact = manifest.artifacts.find(
        (item) => item.id === coverage.artifactId
      );
      const probe = readJson(path.join(matrixRoot, artifact.probe));
      const allBodyKeys = [
        ...coverage.representableBodyKeys,
        ...coverage.notRepresentableBodyKeys,
      ];
      expect(new Set(allBodyKeys)).toEqual(
        new Set(manifest.conwayInventory.bodyKeys)
      );
      expect(allBodyKeys).toHaveLength(new Set(allBodyKeys).size);
      expect(probe.evidence.inventory.certificateTags).toEqual(
        coverage.representableCertificateTags
      );
      expect(probe.evidence.inventory.outputAlternatives).toEqual(
        coverage.outputAlternatives
      );
      expect(
        [...probe.evidence.inventory.credentialAlternatives].sort()
      ).toEqual([...coverage.credentialAlternatives].sort());
      expect([...probe.evidence.inventory.voterAlternatives].sort()).toEqual(
        [...coverage.voterAlternatives].sort()
      );
      expect(coverage.nestedInventory.constraints.length).toBeGreaterThan(8);
      expect(
        coverage.nestedInventory.constraints.some(
          (constraint) => constraint.id === 'output-format'
        )
      ).toBe(true);
      expect([...probe.evidence.inventory.messageFields].sort()).toEqual(
        [
          ...coverage.messageRequest.required,
          ...coverage.messageRequest.optional,
          ...coverage.messageRequest.branches.flatMap(
            (branch) => branch.required
          ),
        ].sort()
      );
      expect(
        [...probe.evidence.inventory.signedTransactionFields].sort()
      ).toEqual(
        [
          ...coverage.transactionProof.required,
          ...(coverage.transactionProof.optional || []),
        ].sort()
      );
      expect([...probe.evidence.inventory.signedMessageFields].sort()).toEqual(
        [...coverage.messageProof.required].sort()
      );
      for (const section of [
        'messageRequest',
        'transactionProof',
        'messageProof',
      ]) {
        expect(
          canonicalInventory(probe.evidence.normalizedContract[section])
        ).toEqual(canonicalInventory(coverage[section]));
      }
      const derivedConstraints =
        probe.evidence.normalizedContract.nestedInventory.constraints;
      for (const constraint of coverage.nestedInventory.constraints) {
        const derived = derivedConstraints.find(
          (candidate) =>
            candidate.id === constraint.id &&
            JSON.stringify(candidate.appMajors || null) ===
              JSON.stringify(constraint.appMajors || null)
        );
        expect(derived).toBeDefined();
        // The matrix may freeze a stricter certification bound than the parser.
        expect(
          constraint.maximum == null ||
            derived.maximum == null ||
            constraint.maximum <= derived.maximum
        ).toBe(true);
        const withoutMaximum = (value) => {
          const result = { ...value };
          delete result.maximum;
          return result;
        };
        expect(canonicalInventory(withoutMaximum(constraint))).toEqual(
          canonicalInventory(withoutMaximum(derived))
        );
      }
      for (const key of coverage.representableBodyKeys) {
        const field = manifest.bodyFieldMap.find((item) => item.key === key);
        expect(
          field.vendorAliases.some((alias) =>
            probe.evidence.inventory.transactionFields.includes(alias)
          )
        ).toBe(true);
      }
      for (const key of coverage.notRepresentableBodyKeys) {
        const field = manifest.bodyFieldMap.find((item) => item.key === key);
        expect(
          field.vendorAliases.some((alias) =>
            probe.evidence.inventory.transactionFields.includes(alias)
          )
        ).toBe(false);
      }
    }
  });

  it('binds probe output to package, source, and lock identities', () => {
    for (const artifact of manifest.artifacts) {
      const probe = readJson(path.join(matrixRoot, artifact.probe));
      expect(probe.package).toMatchObject({
        name: artifact.package,
        version: artifact.version,
      });
      expect(probe.identity).toMatchObject({
        version: artifact.version,
        sha1: artifact.sha1,
        integrity: artifact.integrity,
        lockSha256: artifact.lockSha256,
        lockVerification: {
          version: 'match',
        },
      });
      expect(probe.identity).not.toHaveProperty('gitHead');
      expect(probe.identity.resolved).toMatch(/^https:\/\//);
      for (const sourceFile of probe.evidence.sourceFiles) {
        expect(sourceFile.sha256).toMatch(/^[0-9a-f]{64}$/);
      }
    }
    for (const artifact of manifest.artifacts.filter(
      ({ vendor }) => vendor === 'trezor'
    )) {
      const installed = readJson(
        path.join(root, 'node_modules', artifact.package, 'package.json')
      );
      expect(installed).toMatchObject({
        name: artifact.package,
        version: trezorLock.version,
      });
      // Archived hashes describe the reviewed version. The offline test below
      // binds installed source bytes to the current verified package tarball.
    }
    expect(
      digest(path.join(matrixRoot, 'ledger-8.0.0-package-lock.json'))
    ).toBe(manifest.artifacts[1].lockSha256);
    const consumer = readJson(
      path.join(matrixRoot, manifest.dependencyDecision.consumerResult)
    );
    expect(consumer.webpackBuildEvidence).toMatchObject({
      invocation: 'webpack-node-api-in-memory-alias',
      cache: false,
      watch: false,
      main: {
        status: 'pass',
        callbackError: false,
        compilationErrorCount: 0,
      },
      renderer: {
        status: 'pass',
        callbackError: false,
        compilationErrorCount: 0,
      },
    });
    expect(consumer).not.toHaveProperty('productionPackageRestored');
    expect(consumer.isolatedImports.map((item) => item.specifier)).toEqual([
      '@cardano-foundation/ledgerjs-hw-app-cardano',
      '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address',
      '@cardano-foundation/ledgerjs-hw-app-cardano/dist/types/internal',
    ]);
    expect(Object.values(consumer.validatorEvidence).every(Boolean)).toBe(true);
    expect(consumer.messageTemplateParserEvidence).toEqual({
      templateCount: 4,
      installedLedger7: 'pass',
      candidateLedger8: 'pass',
    });
    expect(consumer.lockVerification).toMatchObject({
      installedMatchesCommitted: true,
      integrityVerifiedBeforeLoad: 'cached-tarball-sri',
    });
    expect(consumer.diagnosticCodes).toEqual([2339]);
    expect(consumer.diagnostics).toHaveLength(3);
    expect(consumer.migrations).toEqual(
      manifest.dependencyDecision.requiredMigrations
    );
    const production = readJson(
      path.join(matrixRoot, manifest.dependencyDecision.productionResult)
    );
    expect(production.identity).toMatchObject({
      version: '8.0.0',
      sha1: '7f6b1dcfcc5b397156507b0c82d25d7595687a68',
      integrity:
        'sha512-hyWBk4HQApPdIvidQOExOP+GxD36WDsgzCz1PAFeJ4heL/b5Bmplyyg03/lA95NDNjjpqgDzN2rJyBHYpqgfmQ==',
    });
    // The production fixture records the original 8.0.0 review. Bind the
    // installed SDK to today's lock and verified tarball below, rather than
    // relabeling archived source hashes as evidence for a newer SDK.
    expect(
      readJson(
        path.join(
          root,
          'node_modules/@cardano-foundation/ledgerjs-hw-app-cardano/package.json'
        )
      )
    ).toMatchObject({
      name: production.package.name,
      version: ledgerLock.version,
    });
    const regeneratedProbe = path.join(
      os.tmpdir(),
      `daedalus-ledger-probe-${process.pid}.json`
    );
    execFileSync(process.execPath, [
      path.join(root, 'scripts/hardware-wallet-capability-probe.cjs'),
      '--vendor=ledger',
      `--root=${path.join(root, 'node_modules', ledgerPackage)}`,
      `--label=installed-ledger-${ledgerLock.version}`,
      `--lock=${path.join(root, 'yarn.lock')}`,
      `--output=${regeneratedProbe}`,
    ]);
    const regenerated = readJson(regeneratedProbe);
    expect(regenerated.identity).toMatchObject({
      version: ledgerLock.version,
      resolved: ledgerLock.resolved,
      sha1: ledgerLock.resolved.split('#')[1],
      integrity: ledgerLock.integrity,
    });
    const tarball = process.env.DAEDALUS_YARN_OFFLINE_CACHE
      ? path.join(
          process.env.DAEDALUS_YARN_OFFLINE_CACHE,
          ledgerLock.resolved
            .split('#')[0]
            .replace(/https:\/\/.*\.com\//g, '')
            .replace(/[@/%:-]/g, '_')
        )
      : path.join(
          JSON.parse(
            execSync('yarn cache dir --json', {
              cwd: root,
              encoding: 'utf8',
            }).trim()
          ).data,
          `npm-${ledgerPackage.replace(/\//g, '-')}-${ledgerLock.version}-${ledgerLock.resolved.split('#')[1]}-integrity`,
          'node_modules',
          ledgerPackage,
          '.yarn-tarball.tgz'
        );
    const tarballBytes = fs.readFileSync(tarball);
    expect(createHash('sha1').update(tarballBytes).digest('hex')).toBe(
      ledgerLock.resolved.split('#')[1]
    );
    const [algorithm, expectedIntegrity] = ledgerLock.integrity.split('-');
    expect(createHash(algorithm).update(tarballBytes).digest('base64')).toBe(
      expectedIntegrity
    );
    for (const sourceFile of regenerated.evidence.sourceFiles) {
      expect(
        digest(
          path.join(
            root,
            'node_modules/@cardano-foundation/ledgerjs-hw-app-cardano',
            sourceFile.path
          )
        )
      ).toBe(sourceFile.sha256);
      expect(
        createHash('sha256')
          .update(
            execFileSync('tar', ['-xOf', tarball, `package/${sourceFile.path}`])
          )
          .digest('hex')
      ).toBe(sourceFile.sha256);
    }
  });

  it('preserves Trezor runtime package, lock, source, and consumer identities', () => {
    const identity = readJson(
      path.join(matrixRoot, 'trezor-9.7.2-runtime-identity.json')
    );
    expect(identity.schemaVersion).toBe(3);
    expect(identity.nodes.map((node) => node.id)).toEqual(
      [...identity.nodes.map((node) => node.id)].sort()
    );
    expect(identity.edges).toEqual(
      [...identity.edges].sort((left, right) =>
        `${left.from}\0${left.dependency}\0${left.to}`.localeCompare(
          `${right.from}\0${right.dependency}\0${right.to}`
        )
      )
    );
    for (const node of identity.nodes) {
      expect(node.lockSelectors.length).toBeGreaterThan(0);
      expect(node.lockIdentity.resolved).toMatch(/^https:\/\//);
      expect(node.lockIdentity.sha1).toMatch(/^[0-9a-f]{40}$/);
      expect(node.lockIdentity.integrity).toMatch(
        /^sha(1|256|384|512)-[A-Za-z0-9+/]+={0,2}$/
      );
      expect(node.lockIdentity.integritySource).toMatch(
        /^yarn\.lock(?:-resolved-sha1)?$/
      );
    }
    expect(identity.graphSha256).toBe(
      createHash('sha256')
        .update(
          JSON.stringify({ nodes: identity.nodes, edges: identity.edges })
        )
        .digest('hex')
    );
    expect(manifest.runtimeProvenance).toMatchObject({
      artifactId: 'trezor-connect-9.7.2',
      runtimeGraphSha256: identity.graphSha256,
    });
    expect(manifest.runtimeProvenance.configIdentitySha256).toBe(
      identity.configIdentity.sha256
    );
    const regeneratedProbe = path.join(
      os.tmpdir(),
      `daedalus-trezor-probe-${process.pid}.json`
    );
    const regeneratedRuntime = path.join(
      os.tmpdir(),
      `daedalus-trezor-runtime-${process.pid}.json`
    );
    const yarnCacheDirectory = JSON.parse(
      execSync('yarn cache dir --json', {
        cwd: root,
        encoding: 'utf8',
        env: { ...process.env, FORCE_COLOR: '1' },
      }).trim()
    ).data;
    expect(path.isAbsolute(yarnCacheDirectory)).toBe(true);
    expect(yarnCacheDirectory).not.toContain('\u001b');
    execFileSync(
      process.execPath,
      [
        path.join(root, 'scripts/hardware-wallet-capability-probe.cjs'),
        '--vendor=trezor',
        `--root=${path.join(root, 'node_modules/@trezor/connect')}`,
        `--label=installed-trezor-connect-${trezorLock.version}`,
        `--lock=${path.join(root, 'yarn.lock')}`,
        `--output=${regeneratedProbe}`,
        `--runtime-output=${regeneratedRuntime}`,
      ],
      {
        // Yarn appends v6 to this configured root; the probe must discover it.
        env: {
          ...process.env,
          YARN_CACHE_FOLDER: path.dirname(yarnCacheDirectory),
          FORCE_COLOR: '1',
        },
      }
    );
    const regenerated = readJson(regeneratedRuntime);
    for (const node of regenerated.nodes) {
      expect(
        readJson(path.join(root, node.path, 'package.json'))
      ).toMatchObject({ name: node.name, version: node.version });
      for (const selector of node.lockSelectors) {
        const locked = currentLock[selector];
        expect(locked).toBeDefined();
        expect(node.version).toBe(locked.version);
        expect(node.lockIdentity).toEqual({
          resolved: locked.resolved,
          sha1: locked.resolved.split('#')[1],
          integrity:
            locked.integrity ||
            `sha1-${Buffer.from(locked.resolved.split('#')[1], 'hex').toString(
              'base64'
            )}`,
          integritySource: locked.integrity
            ? 'yarn.lock'
            : 'yarn.lock-resolved-sha1',
        });
      }
    }
    for (const edge of regenerated.edges) {
      const source = regenerated.nodes.find((node) => node.id === edge.from);
      const target = regenerated.nodes.find((node) => node.id === edge.to);
      const metadata = readJson(path.join(root, source.path, 'package.json'));
      const selector = {
        ...metadata.dependencies,
        ...metadata.optionalDependencies,
      }[edge.dependency];
      expect(target.name).toBe(edge.dependency);
      expect(target.lockSelectors).toContain(`${edge.dependency}@${selector}`);
    }
    // Frozen evidence attests the reviewed installation, not today's hoisting
    // or unrelated lock updates. Live dependencies must match the current lock.
    expect(
      regenerated.nodes.find((node) => node.id === regenerated.root)
    ).toMatchObject({
      name: '@trezor/connect',
      version: trezorLock.version,
      lockSelectors: [trezorSelector],
      lockIdentity: {
        resolved: trezorLock.resolved,
        sha1: trezorLock.resolved.split('#')[1],
        integrity: trezorLock.integrity,
        integritySource: 'yarn.lock',
      },
    });
    expect(regenerated.configIdentity.files.map((file) => file.path)).toEqual(
      identity.configIdentity.files.map((file) => file.path)
    );
    for (const file of regenerated.configIdentity.files) {
      expect(file.sha256).toBe(
        digest(path.join(root, 'node_modules/@trezor/connect', file.path))
      );
    }
    expect(regenerated.configIdentity.sha256).toBe(
      createHash('sha256')
        .update(
          regenerated.configIdentity.files
            .map((file) => `${file.path}\0${file.sha256}\n`)
            .join('')
        )
        .digest('hex')
    );
    expect(regenerated.transportResolution).toMatchObject({
      daedalusRoot: {
        version: readJson(
          path.join(root, 'node_modules/@trezor/transport/package.json')
        ).version,
      },
      connectRoot: {
        version: readJson(
          require.resolve('@trezor/transport/package.json', {
            paths: [path.join(root, 'node_modules/@trezor/connect')],
          })
        ).version,
      },
      distinctInstallations:
        fs.realpathSync(
          path.join(root, 'node_modules/@trezor/transport/package.json')
        ) !==
        fs.realpathSync(
          require.resolve('@trezor/transport/package.json', {
            paths: [path.join(root, 'node_modules/@trezor/connect')],
          })
        ),
    });
  });

  it('verifies explicit offline tarballs in a fresh HOME and rejects corrupt evidence', () => {
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), 'daedalus-hermetic-provenance-')
    );
    const home = path.join(directory, 'home');
    const mirror = path.join(directory, 'mirror');
    fs.mkdirSync(home);
    fs.mkdirSync(mirror);
    const name = '@trezor/connect';
    const lock = trezorLock;
    const filename = `_trezor_connect___connect_${lock.version}.tgz`;
    const cacheDirectory = process.env.DAEDALUS_YARN_OFFLINE_CACHE
      ? null
      : JSON.parse(
          execSync('yarn cache dir --json', {
            cwd: root,
            encoding: 'utf8',
            env: { ...process.env, FORCE_COLOR: '1' },
          }).trim()
        ).data;
    const tarball = process.env.DAEDALUS_YARN_OFFLINE_CACHE
      ? path.join(process.env.DAEDALUS_YARN_OFFLINE_CACHE, filename)
      : path.join(
          cacheDirectory,
          `npm-${name.replace(/\//g, '-')}-${lock.version}-${lock.resolved.split('#')[1]}-integrity`,
          'node_modules',
          name,
          '.yarn-tarball.tgz'
        );
    fs.copyFileSync(tarball, path.join(mirror, filename));
    // copyFileSync preserves the Nix store's read-only mode on this test copy.
    fs.chmodSync(path.join(mirror, filename), 0o600);
    const output = path.join(directory, 'probe.json');
    const argumentsForProbe = [
      path.join(root, 'scripts/hardware-wallet-capability-probe.cjs'),
      '--vendor=trezor',
      `--root=${path.join(root, 'node_modules', name)}`,
      `--label=hermetic-trezor-connect-${lock.version}`,
      `--lock=${path.join(root, 'yarn.lock')}`,
      `--output=${output}`,
    ];
    const options = {
      env: {
        ...process.env,
        HOME: home,
        YARN_CACHE_FOLDER: path.join(home, 'empty-cache'),
        DAEDALUS_YARN_OFFLINE_CACHE: mirror,
        FORCE_COLOR: '1',
      },
    };
    try {
      execFileSync(process.execPath, argumentsForProbe, options);
      expect(readJson(output).identity.lockVerification).toEqual({
        version: 'match',
        integrity: 'verified-offline-tarball-before-load',
      });
      const unpacked = path.join(directory, 'unpacked');
      fs.mkdirSync(unpacked);
      require('tar').x({
        file: path.join(mirror, filename),
        cwd: unpacked,
        sync: true,
      });
      for (const file of readJson(output).evidence.sourceFiles) {
        expect(digest(path.join(unpacked, 'package', file.path))).toBe(
          file.sha256
        );
        expect(digest(path.join(root, 'node_modules', name, file.path))).toBe(
          file.sha256
        );
      }
      fs.unlinkSync(output);
      const lockDirectory = path.join(directory, 'altered-lock');
      fs.mkdirSync(lockDirectory);
      const alteredLock = path.join(lockDirectory, 'yarn.lock');
      fs.writeFileSync(
        alteredLock,
        fs
          .readFileSync(path.join(root, 'yarn.lock'), 'utf8')
          .replace(
            lock.integrity,
            `sha512-${Buffer.alloc(64).toString('base64')}`
          )
      );
      expect(() =>
        execFileSync(
          process.execPath,
          argumentsForProbe.map((argument) =>
            argument.startsWith('--lock=') ? `--lock=${alteredLock}` : argument
          ),
          options
        )
      ).toThrow(/Offline tarball does not match lock SRI/);
      expect(fs.existsSync(output)).toBe(false);
      fs.writeFileSync(path.join(mirror, filename), 'corrupt tarball');
      expect(() =>
        execFileSync(process.execPath, argumentsForProbe, options)
      ).toThrow(/Offline tarball does not match lock SHA-1/);
      expect(fs.existsSync(output)).toBe(false);
    } finally {
      fs.rmSync(directory, { recursive: true, force: true });
    }
  });

  it('fails Trezor graph generation when a resolved dependency loses lock identity', () => {
    const connectPackage = readJson(
      path.join(root, 'node_modules/@trezor/connect/package.json')
    );
    const [dependency, selector] = Object.entries(
      connectPackage.dependencies
    ).sort(([left], [right]) => left.localeCompare(right))[0];
    const lockText = fs.readFileSync(path.join(root, 'yarn.lock'), 'utf8');
    const lockedSelector = `${dependency}@${selector}`;
    expect(lockText).toContain(lockedSelector);
    const brokenLock = path.join(
      fs.mkdtempSync(
        path.join(os.tmpdir(), `daedalus-trezor-broken-lock-${process.pid}-`)
      ),
      'yarn.lock'
    );
    fs.writeFileSync(
      brokenLock,
      lockText.replace(
        lockedSelector,
        `${dependency}@__missing_lock_identity__`
      )
    );
    expect(() =>
      execFileSync(
        process.execPath,
        [
          path.join(root, 'scripts/hardware-wallet-capability-probe.cjs'),
          '--vendor=trezor',
          `--root=${path.join(root, 'node_modules/@trezor/connect')}`,
          '--label=broken-lock-test',
          `--lock=${brokenLock}`,
          `--output=${path.join(
            os.tmpdir(),
            `broken-probe-${process.pid}.json`
          )}`,
          `--runtime-output=${path.join(
            os.tmpdir(),
            `broken-runtime-${process.pid}.json`
          )}`,
        ],
        { stdio: 'ignore' }
      )
    ).toThrow();
  });

  it('rejects caller-supplied identity, build, and restoration claims', () => {
    const capabilityScript = path.join(
      root,
      'scripts/hardware-wallet-capability-probe.cjs'
    );
    const capabilitySource = fs.readFileSync(capabilityScript, 'utf8');
    const consumerSource = fs.readFileSync(
      path.join(root, 'scripts/hardware-wallet-ledger-consumer-probe.cjs'),
      'utf8'
    );
    for (const retiredFlag of [
      '--expected-version',
      '--sha1',
      '--sha256',
      '--integrity',
      '--resolved',
      '--git-head',
      '--main-build',
      '--renderer-build',
      '--production-package-restored',
    ]) {
      expect(capabilitySource).not.toContain(retiredFlag);
      expect(consumerSource).not.toContain(retiredFlag);
    }
    const baseArguments = [
      capabilityScript,
      '--vendor=ledger',
      `--root=${path.join(
        root,
        'node_modules/@cardano-foundation/ledgerjs-hw-app-cardano'
      )}`,
      '--label=rejection-test',
      `--lock=${path.join(root, 'yarn.lock')}`,
      `--output=${path.join(
        os.tmpdir(),
        `rejected-probe-${process.pid}.json`
      )}`,
    ];
    for (const retired of [
      '--expected-version=7.1.4',
      '--sha1=e3e484edf950a871d3d3c87750077565162eee9f',
      '--sha256=claimed',
      '--integrity=sha512-claimed',
      '--resolved=https://example.invalid/package.tgz',
      '--git-head=claimed',
      '--main-build=pass',
      '--renderer-build=pass',
      '--production-package-restored=true',
    ]) {
      expect(() =>
        execFileSync(process.execPath, [...baseArguments, retired], {
          stdio: 'ignore',
        })
      ).toThrow();
    }
    expect(() =>
      execFileSync(
        process.execPath,
        [
          path.join(root, 'scripts/hardware-wallet-ledger-consumer-probe.cjs'),
          '/tmp/candidate',
          '/tmp/result.json',
          '--main-build=pass',
        ],
        { stdio: 'ignore' }
      )
    ).toThrow();
    const generated = JSON.stringify(
      manifest.artifacts.map((artifact) =>
        readJson(path.join(matrixRoot, artifact.probe))
      )
    );
    expect(generated).not.toContain('gitHead');
    expect(generated).not.toContain('productionPackageRestored');
  });

  it('rejects contradictory evidence dimensions', () => {
    const base = {
      libraryRepresentability: 'representable',
      deterministicProbe: 'pass',
      emulatorEvidence: 'not_run',
      physicalCertification: 'not_run',
      adapterImplementation: 'not_implemented',
      productEnablement: 'disabled',
    };
    expect(validState(base)).toBe(true);
    expect(validState({ ...base, libraryRepresentability: 'unresolved' })).toBe(
      false
    );
    expect(
      validState({
        ...base,
        deterministicProbe: 'fail',
        physicalCertification: 'pass',
      })
    ).toBe(false);
    expect(validState({ ...base, adapterImplementation: 'pass' })).toBe(true);
    expect(validState({ ...base, productEnablement: 'enabled' })).toBe(false);
    const otherwiseEnabled = {
      ...base,
      emulatorEvidence: 'pass',
      physicalCertification: 'pass',
      adapterImplementation: 'pass',
      productEnablement: 'enabled',
    };
    expect(validState(otherwiseEnabled)).toBe(true);
    expect(
      validState({ ...otherwiseEnabled, emulatorEvidence: 'not_run' })
    ).toBe(false);
    for (const row of manifest.staticRows) {
      expect(validState(row)).toBe(true);
      expect(row.physicalCertification).toBe('not_run');
      expect(row.adapterImplementation).toBe('not_implemented');
      expect(row.productEnablement).toBe('disabled');
    }
  });

  it('validates generated task-607 cases and complete coverage', () => {
    const schema = readJson(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/cases.schema.json'
      )
    );
    expect(new Ajv({ allErrors: true }).validate(schema, casesDocument)).toBe(
      true
    );
    const ids = casesDocument.cases.map((testCase) => testCase.id);
    expect(ids).toHaveLength(new Set(ids).size);
    const capabilityRows = new Set([
      ...manifest.staticRows.map((row) => row.id),
      ...manifest.modelRows.map((row) => row.id),
    ]);
    for (const testCase of casesDocument.cases) {
      expect(capabilityRows.has(testCase.capabilityRowId)).toBe(true);
    }
    for (const coverage of manifest.staticFieldCoverage) {
      for (const key of manifest.conwayInventory.bodyKeys) {
        expect(
          casesDocument.cases.some(
            (testCase) =>
              testCase.artifactBinding.id === coverage.artifactId &&
              testCase.category === 'body-field' &&
              testCase.subject.key === key
          )
        ).toBe(true);
      }
      for (const tag of manifest.conwayInventory.certificateTags) {
        expect(
          casesDocument.cases.some(
            (testCase) =>
              testCase.artifactBinding.id === coverage.artifactId &&
              testCase.category === 'certificate' &&
              testCase.subject.tag === tag
          )
        ).toBe(true);
      }
    }
    for (const family of manifest.exactBodyFamilies) {
      const applicableRows = manifest.modelRows.filter(
        (row) => row.operationSupport.signTx === 'representable'
      );
      expect(
        casesDocument.cases.filter(
          (testCase) =>
            testCase.category === 'exact-body-family' &&
            testCase.subject.name === family
        )
      ).toHaveLength(applicableRows.length);
    }
    for (const model of manifest.modelRows) {
      for (const operation of Object.keys(model.operationSupport)) {
        expect(
          casesDocument.cases.some(
            (testCase) =>
              testCase.capabilityRowId === model.id &&
              testCase.operation === operation &&
              testCase.category === 'model-version'
          )
        ).toBe(true);
      }
    }
    for (const error of manifest.errorContract) {
      for (const operation of ['signTx', 'signData']) {
        for (const model of manifest.modelRows.filter(
          (row) => row.operationSupport[operation] === 'representable'
        )) {
          expect(
            casesDocument.cases.some(
              (testCase) =>
                testCase.category === 'error' &&
                testCase.capabilityRowId === model.id &&
                testCase.operation === operation &&
                testCase.subject.predicate === error.predicate &&
                testCase.expected.errorCode === error[operation]
            )
          ).toBe(true);
        }
      }
    }
  });

  it('binds task-607 physical targets to the reviewed Nano X runtime only', () => {
    const targetRowId = 'ledger-8-library-nano-x-app7';
    const expectedIds = [
      'ledger-8-library-nano-x-app7-signdata-message-mode-drep-direct',
      'ledger-8-library-nano-x-app7-signdata-message-mode-drep-type6',
      'ledger-8-library-nano-x-app7-signdata-message-mode-payment-address',
      'ledger-8-library-nano-x-app7-signdata-message-mode-stake-address',
      'ledger-8-library-nano-x-app7-signtx-physical-transaction-ordered-batch',
      'ledger-8-library-nano-x-app7-signtx-physical-transaction-single-transaction',
    ];
    const targets = casesDocument.cases.filter(
      (testCase) => testCase.certificationTarget
    );
    const runtime = readJson(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/evidence-examples.json'
      )
    ).promotedValid.version;
    expect(targets.map((testCase) => testCase.id)).toEqual(expectedIds);
    expect(runtime).toEqual([7, 3, 0]);
    for (const testCase of targets) {
      const row = manifest.modelRows.find(
        (item) => item.id === testCase.capabilityRowId
      );
      const minimum = testCase.modelBinding.certificationVersion;
      expect(testCase.capabilityRowId).toBe(targetRowId);
      expect(testCase.expected).toEqual({
        outcome: 'pass',
        errorCode: 'none',
        preDevice: false,
      });
      expect(testCase.physicalExecution).toBe(true);
      expect(testCase.signingBinding.executionLayer).toBe('physical-device');
      expect(runtime[0]).toBe(row.versionMajor);
      const firstDifference = runtime.findIndex(
        (part, index) => part !== minimum[index]
      );
      expect(firstDifference).toBeGreaterThanOrEqual(0);
      expect(runtime[firstDifference]).toBeGreaterThan(
        minimum[firstDifference]
      );
    }
    expect(
      targets
        .filter((testCase) => testCase.operation === 'signTx')
        .map((testCase) => testCase.signingBinding.signingMode)
    ).toEqual(['exact-body', 'exact-body']);
    const batch = targets.find(
      (testCase) => testCase.subject.name === 'ordered-batch'
    );
    expect(JSON.parse(batch.inputRecipe.canonicalJson)).toMatchObject({
      contextFixture: 'task-607-ordinary-ledger-context-v1',
      execution: 'ordered-batch',
      itemKinds: ['ready', 'canonical-empty-witness-set', 'ready'],
      signedIndices: [0, 2],
      release: 'all-after-success',
      refusalIndices: [0, 1, 2],
    });
    const direct = targets.find(
      (testCase) => testCase.subject.name === 'drep-direct'
    )!;
    const type6 = targets.find(
      (testCase) => testCase.subject.name === 'drep-type6'
    )!;
    const directRequest = JSON.parse(direct.inputRecipe.canonicalJson).request;
    const type6Request = JSON.parse(type6.inputRecipe.canonicalJson).request;
    expect(type6Request).toEqual(directRequest);
    expect(direct.signingBinding.requestBranch).toBe('key-hash');
    expect(type6.signingBinding.requestBranch).toBe('key-hash');
    expect(direct.fixtureBinding.selectedInput.kind).toBe('drep-key-hash');
    expect(type6.fixtureBinding.selectedInput.kind).toBe('enterprise-address');
    expect(
      casesDocument.cases
        .filter(
          (testCase) =>
            testCase.operation === 'signTx' &&
            [
              'body-field',
              'certificate',
              'exact-body-family',
              'nested-constraint',
              'model-version',
            ].includes(testCase.category)
        )
        .every(
          (testCase) =>
            ['pre-device-reject', 'static-source-assertion'].includes(
              testCase.expected.outcome
            ) && !testCase.physicalExecution
        )
    ).toBe(true);
  });

  it('reproduces concrete cases and binds authoritative fixture digests', () => {
    const secondPath = path.join(
      os.tmpdir(),
      `daedalus-hardware-cases-second-${process.pid}.json`
    );
    execFileSync(process.execPath, [
      generatorPath,
      path.join(matrixRoot, 'manifest.json'),
      secondPath,
    ]);
    expect(fs.readFileSync(secondPath)).toEqual(
      fs.readFileSync(generatedCasesPath)
    );
    expect(fs.readFileSync(secondPath)).toEqual(
      fs.readFileSync(
        path.join(root, 'hardware-wallet-tests/capability-matrix/cases.json')
      )
    );
    const evidenceSchemaPath = path.join(
      root,
      'hardware-wallet-tests/capability-matrix/evidence.schema.json'
    );
    const generatedEvidencePath = path.join(
      os.tmpdir(),
      `daedalus-hardware-evidence-${process.pid}.json`
    );
    const generatedExamplesPath = path.join(
      os.tmpdir(),
      `daedalus-hardware-examples-${process.pid}.json`
    );
    const generatedInputRecipesPath = path.join(
      os.tmpdir(),
      `daedalus-hardware-input-recipes-${process.pid}.json`
    );
    fs.copyFileSync(evidenceSchemaPath, generatedEvidencePath);
    execFileSync(process.execPath, [
      generatorPath,
      path.join(matrixRoot, 'manifest.json'),
      secondPath,
      generatedEvidencePath,
      generatedExamplesPath,
      generatedInputRecipesPath,
    ]);
    for (const [generated, committed] of [
      [generatedEvidencePath, evidenceSchemaPath],
      [
        generatedExamplesPath,
        path.join(
          root,
          'hardware-wallet-tests/capability-matrix/evidence-examples.json'
        ),
      ],
      [
        generatedInputRecipesPath,
        path.join(
          root,
          'hardware-wallet-tests/capability-matrix/input-recipes.json'
        ),
      ],
    ]) {
      expect(fs.readFileSync(generated)).toEqual(fs.readFileSync(committed));
    }
    for (const fixture of Object.values(manifest.fixtureBindings) as Array<{
      path: string;
      sha256: string;
    }>) {
      expect(digest(path.join(root, fixture.path))).toBe(fixture.sha256);
    }
    const generatedText = fs.readFileSync(generatedCasesPath, 'utf8');
    for (const placeholder of [
      'exact-task-607-model-row',
      'exact-task-607-version-row',
      'task-002-owned-credential-binding',
      'deterministic-hash-match-then-physical-returned-hash',
    ]) {
      expect(generatedText).not.toContain(placeholder);
    }
    expect(
      casesDocument.cases
        .filter(
          (testCase) =>
            testCase.operation === 'signTx' &&
            [
              'body-field',
              'certificate',
              'exact-body-family',
              'nested-constraint',
              'model-version',
            ].includes(testCase.category)
        )
        .every(
          (testCase) =>
            ['pre-device-reject', 'static-source-assertion'].includes(
              testCase.expected.outcome
            ) && testCase.physicalExecution === false
        )
    ).toBe(true);
  });

  it('binds app and per-model firmware gates to probe extraction', () => {
    for (const artifact of manifest.artifacts) {
      const probe = readJson(path.join(matrixRoot, artifact.probe));
      const gates = probe.evidence.normalizedContract.modelOperationGates;
      const rows = manifest.modelRows.filter(
        (item) => item.artifactId === artifact.id
      );
      for (const row of rows) {
        const extracted = gates.find((gate) =>
          artifact.vendor === 'ledger'
            ? gate.appMajor === row.versionMajor
            : gate.model === row.model
        );
        expect(extracted).toBeDefined();
        for (const operation of ['signTx', 'signData']) {
          expect(row.operationSupport[operation]).toBe(
            extracted[operation].support
          );
          expect(row.operationGates[operation].minimumVersion).toEqual(
            extracted[operation].minimumVersion
          );
        }
      }
    }
  });

  it('rejects generated case identity mutations', () => {
    const schema = readJson(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/cases.schema.json'
      )
    );
    const validate = new Ajv({ allErrors: true }).compile(schema);
    const candidate = JSON.parse(JSON.stringify(casesDocument));
    const testCase = candidate.cases.find(
      (item) => item.artifactBinding.vendor === 'trezor'
    );
    testCase.artifactBinding.vendor = 'ledger';
    expect(validate(candidate)).toBe(false);
  });

  it('enforces product limits and synthetic versus physical COSE rules', () => {
    const inputRecipes = readJson(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/input-recipes.json'
      )
    );
    const metadataValueLengths: number[] = [];
    const inspectMetadatum = (value) => {
      if (typeof value === 'string') {
        metadataValueLengths.push(Buffer.byteLength(value));
      } else if (Buffer.isBuffer(value)) {
        metadataValueLengths.push(value.length);
      } else if (Array.isArray(value)) {
        value.forEach(inspectMetadatum);
      } else if (value instanceof Map) {
        for (const [key, item] of value) {
          inspectMetadatum(key);
          inspectMetadatum(item);
        }
      }
    };
    for (const fixture of inputRecipes.transactions) {
      const bytes = Buffer.from(fixture.cborHex, 'hex');
      const transaction = cbor.decodeFirstSync(bytes);
      expect(bytes).toHaveLength(fixture.decodedByteLength);
      for (const value of transaction[3].values()) inspectMetadatum(value);
    }
    expect(
      inputRecipes.transactions.map((item) => item.decodedByteLength)
    ).toEqual([65536, 65537]);
    expect(Math.max(...metadataValueLengths)).toBeLessThanOrEqual(64);
    const product = manifest.errorContract.find(
      (row) => row.predicate === 'decoded-request-above-65536'
    );
    const hardware = manifest.errorContract.find(
      (row) => row.predicate === 'within-product-limit-but-over-hardware-limit'
    );
    expect(product).toMatchObject({
      signTx: 'APIError.InvalidRequest',
      signData: 'APIError.InvalidRequest',
    });
    expect(hardware).toMatchObject({
      signTx: 'TxSignError.ProofGeneration',
      signData: 'DataSignError.ProofGeneration',
    });
    expect(
      manifest.errorContract.find(
        (row) =>
          row.predicate ===
          'vendor-cancellation-unrelated-to-host-cancellation-or-explicit-refusal'
      )
    ).toMatchObject({
      signTx: 'TxSignError.ProofGeneration',
      signData: 'DataSignError.ProofGeneration',
    });
    for (const model of manifest.modelRows) {
      for (const operation of ['signTx', 'signData']) {
        const limits = casesDocument.cases.filter(
          (testCase) =>
            testCase.capabilityRowId === model.id &&
            testCase.operation === operation &&
            testCase.category === 'limit'
        );
        expect(limits.map((testCase) => testCase.subject.name).sort()).toEqual(
          model.operationSupport[operation] === 'representable'
            ? ['decoded-bytes-65536', 'decoded-bytes-65537']
            : []
        );
      }
    }
    expect(manifest.messageRows.map((row) => row.id)).toEqual([
      'payment-address',
      'stake-address',
      'drep-direct',
      'drep-type6',
    ]);
    expect(
      casesDocument.cases.filter(
        (testCase) => testCase.category === 'message-mode'
      )
    ).toHaveLength(
      manifest.modelRows.filter(
        (row) => row.operationSupport.signData === 'representable'
      ).length * manifest.messageRows.length
    );
    expect(
      manifest.staticRows.find((row) => row.id === 'trezor-message')
    ).toMatchObject({ vendorCosePassThrough: false });
    const trezorProbe = readJson(
      path.join(matrixRoot, 'trezor-9.7.2-library-results.json')
    );
    expect(trezorProbe.evidence.capabilities).toMatchObject({
      returnedHeadersIncludeVersion: true,
      encodedCoseUnprotectedMapIncludesVersion: false,
    });
    expect(manifest.trezorCoseRule.publicDisposition).toBe(
      'reject-vendor-cose-and-reconstruct-locally-from-validated-raw-material'
    );
    const fixture = readJson(
      path.join(root, manifest.fixtureBindings.signData.path)
    );
    const coseSign1 = cbor.decodeFirstSync(fixture.coseSign1);
    const reconstructedSigStructure = cbor.encodeCanonical([
      'Signature1',
      coseSign1[0],
      Buffer.alloc(0),
      coseSign1[2],
    ]);
    expect(reconstructedSigStructure.toString('hex')).toBe(
      fixture.sigStructure
    );
    expect(
      verify(
        null,
        reconstructedSigStructure,
        createPublicKey({
          key: Buffer.concat([
            Buffer.from('302a300506032b6570032100', 'hex'),
            Buffer.from(fixture.publicKey, 'hex'),
          ]),
          format: 'der',
          type: 'spki',
        }),
        coseSign1[3]
      )
    ).toBe(true);
    expect(cbor.encode(coseSign1).toString('hex')).toBe(fixture.coseSign1);
  });

  it('rejects impossible or sensitive task-607 evidence', () => {
    const schemaText = fs.readFileSync(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/evidence.schema.json'
      ),
      'utf8'
    );
    const schema = JSON.parse(schemaText);
    const examples = readJson(
      path.join(
        root,
        'hardware-wallet-tests/capability-matrix/evidence-examples.json'
      )
    );
    const validate = new Ajv({ allErrors: true, schemaId: 'auto' }).compile(
      schema
    );
    expect(
      Array.from(
        new Set(
          schema.allOf[0].oneOf.flatMap((group) =>
            group.oneOf.map((branch) => branch.properties.caseId.const)
          )
        )
      )
    ).toEqual(
      casesDocument.cases
        .filter((testCase) => testCase.evidenceOwner === 'task-607')
        .map((testCase) => testCase.id)
    );
    expect(validate(examples.valid)).toBe(true);
    expect(examples.valid.executionKind).toBe('mock');
    expect(examples.valid.version).toBeNull();
    expect(examples.promotedValid.executionKind).toBe('physical');
    expect(examples.promotedValid.version).toEqual([7, 3, 0]);
    expect(validate(examples.promotedValid)).toBe(true);
    const certificationRoot = path.join(
      root,
      'hardware-wallet-tests/certification/task-607/ledger-nano-x-app7'
    );
    const certificationRecords = fs
      .readdirSync(certificationRoot)
      .sort()
      .map((fileName) => readJson(path.join(certificationRoot, fileName)));
    expect(certificationRecords.map((record) => record.caseId)).toEqual(
      casesDocument.cases
        .filter((testCase) => testCase.certificationTarget)
        .map((testCase) => testCase.id)
    );
    for (const record of certificationRecords) {
      expect(validate(record)).toBe(true);
    }
    for (const invalid of examples.promotedInvalid) {
      const candidate = JSON.parse(JSON.stringify(examples.promotedValid));
      if (invalid.target === 'proof') {
        candidate.proof[invalid.property] = invalid.value;
      } else {
        candidate[invalid.property] = invalid.value;
      }
      expect(validate(candidate)).toBe(false);
    }
    expect(
      casesDocument.cases.some(
        (testCase) => testCase.id === examples.valid.caseId
      )
    ).toBe(true);
    for (const invalid of examples.invalid) {
      const candidate = JSON.parse(JSON.stringify(examples.valid));
      if (invalid.target === 'proof') {
        candidate.proof[invalid.property] = invalid.value;
      } else {
        candidate[invalid.property] = invalid.value;
      }
      expect(validate(candidate)).toBe(false);
    }
    expect(examples.invalid.map((item) => item.name)).toEqual(
      expect.arrayContaining([
        'wrong-vendor',
        'wrong-operation',
        'wrong-artifact',
        'wrong-model',
        'wrong-model-row',
        'wrong-version',
        'reviewer-rejected-evidence',
        'wrong-error',
        'positive-proof-on-pre-device-rejection',
        'returned-digest-on-pre-device-rejection',
        'witness-on-pre-device-rejection',
      ])
    );
    expect(examples.promotedInvalid.map((item) => item.name)).toContain(
      'mock-cannot-attest-physical-pass'
    );
    const wrongOperationError = JSON.parse(JSON.stringify(examples.valid));
    wrongOperationError.outcome = 'proof-generation';
    wrongOperationError.errorCode = 'TxSignError.ProofGeneration';
    expect(validate(wrongOperationError)).toBe(false);
    for (const forbidden of [
      'devicePath',
      'serial',
      'screenshot',
      'rawTransaction',
      'xpub',
      'errorText',
      'promptText',
    ]) {
      expect(schemaText).not.toContain(`"${forbidden}"`);
    }
  });

  it('assigns static, physical, and release ownership without overlap', () => {
    expect(manifest.downstreamOwnership.staticContractConsumers).toEqual([
      'task-600',
      'task-601',
      'task-602',
      'task-603',
      'task-604',
      'task-605',
      'task-606',
    ]);
    expect(manifest.downstreamOwnership.physicalCertificationOwner).toBe(
      'task-607'
    );
    expect(manifest.downstreamOwnership.productEnablementOwner).toBe(
      'later-release-policy'
    );
  });
});
