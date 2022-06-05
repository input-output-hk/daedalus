#!/usr/bin/env node

const cp = require('child_process');
const path = require('path');
const resolve = require('resolve-tree');
const mainWebpackConfig = require('../source/main/webpack.config');

const uniq = list => Array.from(new Set(list));
const exec = command => {
  try {
    cp.execSync(command).toString();
  } catch (e) {
    console.error(e.stderr.toString());
    process.exit(1);
  }
};

let destinationDir = process.argv[2];
if (!destinationDir) {
  console.error('Destination path argument is required');
  process.exit(1);
}

destinationDir = path.resolve(process.cwd(), destinationDir);
const runtimePackagesNames = uniq(Object.keys(mainWebpackConfig.externals));

console.info(`\
Going to copy following packages along with their dependencies:
${runtimePackagesNames.map(p => ` - ${p}`).join('\n')}`);

const packagesAbsolutePaths = uniq(
  resolve.flatten(resolve.packagesSync(runtimePackagesNames)).map(j => j.root),
);

console.info(`Collected ${packagesAbsolutePaths.length} packages in total.`);

const rootNodeModulesPath = `${
  packagesAbsolutePaths[0].split('/node_modules/')[0]
}/node_modules/`;
const relativePackagesPaths = packagesAbsolutePaths.map(p =>
  p.replace(rootNodeModulesPath, ''),
);
const patternForAllPackagesNames =
  relativePackagesPaths.length > 1
    ? `{${relativePackagesPaths.join(',')}}`
    : relativePackagesPaths[0];
const fullGlobPattern = `${rootNodeModulesPath}./${patternForAllPackagesNames}`;

console.info('Copying..');
exec(`rsync -aR ${fullGlobPattern} ${destinationDir}`);

console.info('Done.');
