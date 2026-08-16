#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const ts = require('typescript');
const prettier = require('prettier');
const webpack = require('webpack');
const { createHash } = require('crypto');

const arguments = process.argv.slice(2);
if (arguments.length !== 2 || arguments.some((argument) => argument.startsWith('--'))) {
  throw new Error(
    'Usage: hardware-wallet-ledger-consumer-probe.cjs <candidate-root> <output>'
  );
}
const [candidateArgument, outputPath] = arguments;
const candidateRoot = path.resolve(candidateArgument);

const projectRoot = path.resolve(__dirname, '..');
const packageName = '@cardano-foundation/ledgerjs-hw-app-cardano';
const committedLockPath = path.join(
  projectRoot,
  'source/common/hardware/fixtures/capability-matrix/ledger-8.0.0-package-lock.json'
);
const packageJson = JSON.parse(
  fs.readFileSync(path.join(candidateRoot, 'package.json'), 'utf8')
);
const lockIdentity = (lock) => {
  if (lock.lockfileVersion !== 3) throw new Error('Candidate lockfileVersion must be 3');
  const entry = lock.packages && lock.packages[`node_modules/${packageName}`];
  if (!entry || !entry.version || !entry.resolved || !entry.integrity) {
    throw new Error('Candidate lock identity is incomplete');
  }
  if (!/^sha(1|256|384|512)-[A-Za-z0-9+/]+={0,2}$/.test(entry.integrity)) {
    throw new Error('Candidate lock integrity is not valid SRI');
  }
  return {
    version: entry.version,
    resolved: entry.resolved,
    integrity: entry.integrity,
  };
};
const committedIdentity = lockIdentity(
  JSON.parse(fs.readFileSync(committedLockPath, 'utf8'))
);
const verifyCachedTarball = (integrity) => {
  const [algorithm, encodedDigest] = integrity.split('-');
  const expected = Buffer.from(encodedDigest, 'base64').toString('hex');
  const cachePath = path.join(
    process.env.npm_config_cache || path.join(os.homedir(), '.npm'),
    '_cacache/content-v2',
    algorithm,
    expected.slice(0, 2),
    expected.slice(2, 4),
    expected.slice(4)
  );
  if (
    !fs.existsSync(cachePath) ||
    createHash(algorithm).update(fs.readFileSync(cachePath)).digest('hex') !== expected
  ) {
    throw new Error('Cached candidate tarball does not match committed lock SRI');
  }
};
const installedIdentity = lockIdentity(
  JSON.parse(
    fs.readFileSync(path.join(candidateRoot, '..', '..', '.package-lock.json'), 'utf8')
  )
);
if (
  JSON.stringify(committedIdentity) !== JSON.stringify(installedIdentity) ||
  packageJson.version !== committedIdentity.version
) {
  throw new Error('Candidate package does not match the committed lock identity');
}
verifyCachedTarball(committedIdentity.integrity);

const importPaths = [
  packageName,
  `${packageName}/dist/utils/address`,
  `${packageName}/dist/types/internal`,
];
const isolatedImports = importPaths.map((specifier) => {
  const suffix = specifier.slice(packageName.length).replace(/^\//, '');
  const resolved = suffix
    ? require.resolve(path.join(candidateRoot, suffix))
    : require.resolve(candidateRoot);
  require(resolved);
  return { specifier, resolved: path.relative(candidateRoot, resolved).split(path.sep).join('/') };
});
const validators = require(path.join(candidateRoot, 'dist/validation/deviceCapabilities.js'));
require(path.join(candidateRoot, 'dist/validation/requestCompatibility.js'));
require(path.join(candidateRoot, 'dist/validation/v7/xs.js'));
const validatorEvidence = {
  appV7: validators.isV7App({ major: 7, minor: 1, patch: 0 }) === true,
  appV8: validators.isV8App({ major: 8, minor: 0, patch: 0 }) === true,
  appV7RejectedByV8: validators.isV8App({ major: 7, minor: 1, patch: 0 }) === false,
  appV8RejectedByV7: validators.isV7App({ major: 8, minor: 0, patch: 0 }) === false,
};
if (Object.values(validatorEvidence).some((value) => !value)) {
  throw new Error('Ledger app-v7/app-v8 validator path failed');
}
const cases = JSON.parse(
  fs.readFileSync(
    path.join(projectRoot, 'hardware-wallet-tests/capability-matrix/cases.json'),
    'utf8'
  )
).cases;
const ledgerMessageTemplates = Array.from(
  new Map(
    cases
      .filter(
        (testCase) =>
          testCase.artifactBinding.vendor === 'ledger' &&
          testCase.inputRecipe.kind === 'physical-message-request'
      )
      .map((testCase) => {
        const template = JSON.parse(testCase.inputRecipe.canonicalJson)
          .requestTemplate;
        return [JSON.stringify(template), template];
      })
  ).values()
);
const candidateMessageParser = require(path.join(
  candidateRoot,
  'dist/parsing/messageData.js'
)).parseMessageData;
const installedMessageParser = require(path.join(
  projectRoot,
  'node_modules',
  packageName,
  'dist/parsing/messageData.js'
)).parseMessageData;
for (const template of ledgerMessageTemplates) {
  installedMessageParser(template);
  candidateMessageParser(template);
}
const messageTemplateParserEvidence = {
  templateCount: ledgerMessageTemplates.length,
  installedLedger7: 'pass',
  candidateLedger8: 'pass',
};

const configPath = path.join(projectRoot, 'tsconfig.json');
const configFile = ts.readConfigFile(configPath, ts.sys.readFile);
if (configFile.error) throw new Error('Unable to read tsconfig.json');
const parsed = ts.parseJsonConfigFileContent(
  configFile.config,
  ts.sys,
  projectRoot,
  {
    baseUrl: projectRoot,
    paths: {
      '@cardano-foundation/ledgerjs-hw-app-cardano': [candidateRoot],
      '@cardano-foundation/ledgerjs-hw-app-cardano/*': [
        path.join(candidateRoot, '*'),
      ],
    },
    noEmit: true,
    skipLibCheck: true,
  },
  configPath
);

const consumers = [
  'source/common/types/hardware-wallets.types.ts',
  'source/main/ipc/getHardwareWalletChannel.ts',
  'source/renderer/app/components/wallet/receive/WalletReceiveDialog.tsx',
  'source/renderer/app/utils/dataSerialization.ts',
  'source/renderer/app/utils/hardwareWalletUtils.ts',
  'source/renderer/app/utils/shelleyLedger.ts',
  'source/renderer/app/utils/shelleyTrezor.ts',
  'source/renderer/app/stores/HardwareWalletsStore.ts',
].map((file) => path.join(projectRoot, file));

const program = ts.createProgram(parsed.fileNames, parsed.options);
const diagnostics = ts
  .getPreEmitDiagnostics(program)
  .filter((diagnostic) => diagnostic.category === ts.DiagnosticCategory.Error);
const normalizedDiagnostics = diagnostics.map((diagnostic) => {
  const position =
    diagnostic.file && diagnostic.start !== undefined
      ? diagnostic.file.getLineAndCharacterOfPosition(diagnostic.start)
      : null;
  return {
    code: diagnostic.code,
    file: diagnostic.file
      ? path.relative(projectRoot, diagnostic.file.fileName).split(path.sep).join('/')
      : 'global',
    line: position ? position.line + 1 : null,
  };
});
if (
  normalizedDiagnostics.length !== 3 ||
  normalizedDiagnostics.some((diagnostic) => diagnostic.code !== 2339)
) {
  throw new Error(
    `Expected exactly the three observed TS2339 candidate migrations: ${JSON.stringify(
      normalizedDiagnostics
    )}`
  );
}

const runWebpack = (name, configPath, outputDirectory) =>
  new Promise((resolve) => {
    delete require.cache[require.resolve(configPath)];
    const original = require(configPath);
    const config = {
      ...original,
      cache: false,
      watch: false,
      output: { ...original.output, path: outputDirectory },
      resolve: {
        ...original.resolve,
        alias: {
          ...(original.resolve.alias || {}),
          [packageName]: candidateRoot,
        },
      },
    };
    webpack(config, (error, stats) => {
      const json = stats
        ? stats.toJson({ all: false, errors: true, modules: true })
        : { errors: [], modules: [] };
      const modules = [];
      const collect = (items) => {
        for (const module of items || []) {
          if (module.name && module.name.includes(candidateRoot)) {
            modules.push(module.name.slice(module.name.indexOf(candidateRoot) + candidateRoot.length + 1));
          }
          collect(module.modules);
        }
      };
      collect(json.modules);
      const candidateModules = Array.from(new Set(modules)).sort();
      resolve({
        status: !error && !stats.hasErrors() && candidateModules.length > 0 ? 'pass' : 'fail',
        callbackError: Boolean(error),
        compilationErrorCount: json.errors.length,
        candidateModuleCount: candidateModules.length,
        candidateModules,
      });
    });
  });

const main = async () => {
  const temporaryRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'daedalus-ledger-probe-'));
  const [mainBuild, rendererBuild] = await Promise.all([
    runWebpack(
      'main',
      path.join(projectRoot, 'source/main/webpack.config.js'),
      path.join(temporaryRoot, 'main')
    ),
    runWebpack(
      'renderer',
      path.join(projectRoot, 'source/renderer/webpack.config.js'),
      path.join(temporaryRoot, 'renderer')
    ),
  ]);
  fs.rmSync(temporaryRoot, { recursive: true, force: true });
  const result = {
  schemaVersion: 1,
  candidateIdentity: committedIdentity,
  lockVerification: {
    lockfileVersion: 3,
    installedMatchesCommitted: true,
    packageVersionMatchesLock: true,
    integrityVerifiedBeforeLoad: 'cached-tarball-sri',
  },
  isolatedImports,
  validatorEvidence,
  messageTemplateParserEvidence,
  typescriptVersion: ts.version,
  consumers: consumers.map((file) => path.relative(projectRoot, file)),
  status: diagnostics.length === 0 ? 'pass' : 'fail',
  webpackBuildEvidence: {
    invocation: 'webpack-node-api-in-memory-alias',
    cache: false,
    watch: false,
    main: mainBuild,
    renderer: rendererBuild,
  },
  diagnosticCodes: Array.from(
    new Set(diagnostics.map((diagnostic) => diagnostic.code))
  ).sort((left, right) => left - right),
  diagnostics: normalizedDiagnostics,
  migrations:
    diagnostics.length === 0
      ? []
      : [
          'Replace the three removed Ledger utils.hex_to_buf call sites with strict validated Buffer conversion.',
        ],
  };

  if (mainBuild.status !== 'pass' || rendererBuild.status !== 'pass') {
    throw new Error('Candidate webpack build failed or did not resolve candidate modules');
  }
  fs.writeFileSync(outputPath, prettier.format(JSON.stringify(result), { parser: 'json' }));
};

main().catch((error) => {
  process.stderr.write(`${error.stack || error}\n`);
  process.exitCode = 1;
});
