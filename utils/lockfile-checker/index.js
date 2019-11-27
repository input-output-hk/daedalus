// @flow
/* eslint-disable no-console */
const lodash = require('lodash');
const fs = require('fs');
const lockfile = require('yarn-lockfile');

const file = fs.readFileSync('yarn.lock', 'utf8');
const json = lockfile.parse(file);

console.log('\n\x1b[36m%s\x1b[0m', 'Script started!\n');

const dependenciesWithIntegrity = [];
lodash.map(json.object, (entry, key) => {
  if(entry.integrity) {
    dependenciesWithIntegrity.push(key);
  }
})

const shouldFix = process.argv.slice(2)[0] === '--fix';
if (shouldFix) {
  fix();
} else {
  check();
}

function check() {
  console.log('\x1b[36m%s\x1b[0m', 'Checking yarn.lock file...\n');
  if (!dependenciesWithIntegrity.length) {
    console.log('\n \x1b[32m', 'All good, yarn.lock is clean!\n', '\x1b[0m');
    return;
  }
  console.log(`\x1b[31myarn.lock is not VALID. Please check dependency integrity hashes!`, '\x1b[0m');
  console.log(`\x1b[31mAffected dependencies: ${lodash.join(dependenciesWithIntegrity, ', ')}\n`, '\x1b[0m');
  console.log('To FIX issues run: \x1b[36m yarn lockfile:fix\n', '\x1b[0m');
}

function fix() {
  if (!dependenciesWithIntegrity.length) {
    console.log('\n \x1b[32m', 'Nothing to fix, yarn.lock is clean!\n', '\x1b[0m');
    return;
  }

  console.log('\x1b[36m%s\x1b[0m', 'Fixing yarn.lock file...\n');

  const fixedJSON = {};
  lodash.map(json.object, (entry, key) => {
    let obj = entry;
    if(entry.integrity) {
      obj = lodash.omit(entry, 'integrity');
    }
    Object.assign(fixedJSON, {[key]: obj});
  })

  try {
    const fixedFileContent = lockfile.stringify(fixedJSON);
    fs.writeFileSync('yarn.lock',fixedFileContent);
    console.log('\n \x1b[32m', 'yarn.lock file successfully FIXED!\n', '\x1b[0m');
  } catch (err) {
    throw err;
  }
}