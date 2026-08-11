const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const argument = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0 || !process.argv[index + 1]) throw new Error(`Missing ${name}`);
  return process.argv[index + 1];
};

const destination = path.resolve(argument('--destination'));
const lockPath = path.resolve(argument('--lock'));
const manifest = JSON.parse(
  fs.readFileSync(path.resolve(argument('--manifest')), 'utf8')
);
const expected = manifest.dependencyDecision.candidate;

if (fs.existsSync(destination)) {
  throw new Error(`Destination already exists: ${destination}`);
}
fs.mkdirSync(destination, { recursive: true });
fs.copyFileSync(lockPath, path.join(destination, 'package-lock.json'));
fs.writeFileSync(
  path.join(destination, 'package.json'),
  `${JSON.stringify(
    {
      name: 'daedalus-task-004-sdk-candidate',
      private: true,
      dependencies: { '@cardano-sdk/core': expected.version },
    },
    null,
    2
  )}\n`
);

const run = (command, args) => {
  const result = spawnSync(command, args, {
    cwd: destination,
    encoding: 'utf8',
    stdio: 'pipe',
  });
  if (result.status !== 0) {
    throw new Error(`${command} failed: ${result.stderr || result.stdout}`);
  }
  return result.stdout.trim();
};

run('npm', ['ci', '--ignore-scripts']);
const tarballName = run('npm', [
  'pack',
  '--silent',
  `@cardano-sdk/core@${expected.version}`,
]);
const tarball = fs.readFileSync(path.join(destination, tarballName));
const shasum = crypto.createHash('sha1').update(tarball).digest('hex');
const integrity = `sha512-${crypto
  .createHash('sha512')
  .update(tarball)
  .digest('base64')}`;
if (shasum !== expected.shasum || integrity !== expected.integrity) {
  throw new Error('Candidate tarball identity does not match the manifest');
}

const packageJson = JSON.parse(
  fs.readFileSync(
    path.join(destination, 'node_modules/@cardano-sdk/core/package.json'),
    'utf8'
  )
);
if (
  packageJson.version !== expected.version ||
  packageJson.gitHead !== expected.gitHead
) {
  throw new Error('Installed candidate identity does not match the manifest');
}

process.stdout.write(
  `${JSON.stringify({
    moduleRoot: path.join(destination, 'node_modules/@cardano-sdk/core'),
    version: packageJson.version,
    gitHead: packageJson.gitHead,
    shasum,
    integrity,
  })}\n`
);
