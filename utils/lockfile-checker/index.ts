/* eslint-disable no-console */
// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'lodash'.
const lodash = require('lodash');

// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'fs'.
const fs = require('fs');

const lockfile = require('yarn-lockfile');

const file = fs.readFileSync('yarn.lock', 'utf8');
const json = lockfile.parse(file);
console.log('\n\x1b[36m%s\x1b[0m', 'Script started!\n');
const dependenciesWithIntegrity = [];
lodash.map(json.object, (entry, key) => {
  if (entry.integrity) {
    dependenciesWithIntegrity.push(key);
  }
});
const dependencyNamesToRemove = ['@types/react-syntax-highlighter', '@types/npmlog', '@types/reactcss', '@types/uglify-js', '@types/parse-json', '@types/html-minifier-terser', '@types/minimist', 'blake2b-wasm@https://github.com/BitGo/blake2b-wasm', 'blake2b@https://github.com/BitGo/blake2b', '@types/aria-query', '@types/istanbul-lib-report'];
const dependenciesToRemove = Object.keys(json.object).filter(key => dependencyNamesToRemove.find(name => key.includes(name)));
const affectedDependencies = Object.keys(json.object).filter(key => {
  const {
    dependencies
  } = json.object[key];
  return dependencies && dependencyNamesToRemove.find(name => dependencies[name]);
});
const blake2bVersion = '2.1.3';
const blake2bGitRefDependency = Object.keys(json.object).find(key => key.includes('blake2b@git+https://github.com'));
const shouldFix = process.argv.slice(2)[0] === '--fix';

if (shouldFix) {
  fix();
} else {
  check();
}

function check() {
  console.log('\x1b[36m%s\x1b[0m', 'Checking yarn.lock file...\n');

  if (!dependenciesWithIntegrity.length && !dependenciesToRemove.length && !affectedDependencies.length && !blake2bGitRefDependency) {
    console.log('\n \x1b[32m', 'All good, yarn.lock is clean!\n', '\x1b[0m');
    return;
  }

  console.log(`\x1b[31myarn.lock is not VALID.`, '\x1b[0m');

  if (dependenciesWithIntegrity.length) {
    console.log(`\x1b[31mPlease check dependency integrity hashes!`, '\x1b[0m');
    console.log(`\x1b[31mDependencies with integrity: ${lodash.join(dependenciesWithIntegrity, ', ')}\n`, '\x1b[0m');
  }

  if (dependenciesToRemove.length) {
    console.log(`\x1b[31mPlease check dependencies to remove!`, '\x1b[0m');
    console.log(`\x1b[31mDependencies to remove: ${lodash.join(dependenciesToRemove, ', ')}\n`, '\x1b[0m');
  }

  if (affectedDependencies.length) {
    console.log(`\x1b[31mPlease check affected dependencies!`, '\x1b[0m');
    console.log(`\x1b[31mAffected dependencies: ${lodash.join(affectedDependencies, ', ')}\n`, '\x1b[0m');
  }

  if (blake2bGitRefDependency) {
    console.log(`\x1b[31mPlease check blake2b dependency!`, '\x1b[0m');
    console.log(`\x1b[31mblake2b dependency: ${blake2bGitRefDependency}\n`, '\x1b[0m');
  }

  console.log('To FIX issues run: \x1b[36m yarn lockfile:fix\n', '\x1b[0m');
}

function fix() {
  if (!dependenciesWithIntegrity.length && !dependenciesToRemove.length && !affectedDependencies.length && !blake2bGitRefDependency) {
    console.log('\n \x1b[32m', 'Nothing to fix, yarn.lock is clean!\n', '\x1b[0m');
    return;
  }

  console.log('\x1b[36m%s\x1b[0m', 'Fixing yarn.lock file...\n');
  const fixedJSON = {};
  lodash.map(json.object, (entry, key) => {
    if (dependenciesToRemove.includes(key) || key === blake2bGitRefDependency) {
      return;
    }

    let obj = entry;

    if (entry.integrity) {
      obj = lodash.omit(entry, 'integrity');
    }

    const cleanedDependencies = obj.dependencies;

    if (cleanedDependencies) {
      dependencyNamesToRemove.forEach(name => delete cleanedDependencies[name]);

      if (cleanedDependencies.blake2b) {
        cleanedDependencies.blake2b = blake2bVersion;
      }

      obj.dependencies = cleanedDependencies;
    }

    Object.assign(fixedJSON, {
      [key]: obj
    });
  });

  try {
    const fixedFileContent = lockfile.stringify(fixedJSON);
    fs.writeFileSync('yarn.lock', fixedFileContent);
    console.log('\n \x1b[32m', 'yarn.lock file successfully FIXED!\n', '\x1b[0m');
  } catch (err) {
    throw err;
  }
}