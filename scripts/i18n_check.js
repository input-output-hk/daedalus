//
// Requires: npm install -G jsonfile
// Author: Angel Leon (@gubatron)
// Created on Apr 5th, 2018
//
// Usage Example:
// node i18n_check.js /path/to/en-US.json /path/to/es-ES.json
//

const fs = require('fs');

function usage(err) {
  console.log('\ni18n_check.js - Tool to check what translation entries are missing and no longer necessary.\n\nUsage:');
  console.log('node i18n_check.js <base translation .json file path> <target translation .json file path>\n\n');
  if (err) {
    console.log('Error: ' + err + '\n');
  }
}

// make sure all parameters are there
if (process.argv.length !== 4) {
  if (process.argv.length === 3) {
    usage('missing target translation .json file path');
  } else if (process.argv.length === 2) {
    usage('missing base and target translation .json file paths');
  }
  return;
}

// make sure they're valid paths
const baseJsonFilePath = process.argv[2];
if (!fs.existsSync(baseJsonFilePath)) {
  usage('base translation file path "' + baseJsonFilePath + '" does not exist');
  return;
}

const targetJsonFilePath = process.argv[3];
if (baseJsonFilePath === targetJsonFilePath) {
  usage('base and target translation file paths cannot be the same');
  return;
}

if (!fs.existsSync(targetJsonFilePath)) {
  usage('target translation file path "' + targetJsonFilePath + '" does not exist');
  return;
}


fs.readFile(baseJsonFilePath, (err, baseTranslationBuffer) => {
  if (err) {
    usage('invalid base .json translation file "' + baseJsonFilePath + '"');
    console.log(err + '\n');
    return;
  }

  const baseTranslation = JSON.parse(baseTranslationBuffer);

  fs.readFile(targetJsonFilePath, (err2, targetTranslationBuffer) => {
    if (err2) {
      usage('invalid target .json translation file "' + targetJsonFilePath + '"');
      console.log(err2 + '\n');
      return;
    }

    const targetTranslation = JSON.parse(targetTranslationBuffer);

    checkTranslations(baseTranslation, targetTranslation, targetJsonFilePath);
  });
});

function getKeys(jsonObj) {
  const keys = [];
  for (const k in jsonObj) {
    keys.push(k);
  }
  return keys;
}

function findMissingKeys(keys, jsonObj) {
  const missingKeys = [];
  for (let i = 0; i < keys.length; i++) {
    const key = keys[i];
    const found = jsonObj[key];
    if (found === undefined || found === null) {
      missingKeys.push(key);
    }
  }
  return missingKeys;
}

function checkTranslations(baseJson, targetJson, targetPath) {
  const baseKeys = getKeys(baseJson);
  const targetKeys = getKeys(targetJson);

  // find missing translations
  const missingKeys = findMissingKeys(baseKeys, targetJson);

  // find unused translations
  const unusedKeys = findMissingKeys(targetKeys, baseJson);

  // print report
  console.log('\n========================================================================================\n');
  if (missingKeys.length === 0) {
    console.log('Target file is up to date, no translation entries missing.\n');
  } else {
    console.log(missingKeys.length + ' translation entries NOT found in target file "' + targetPath + '"\n');
    for (let i = 0; i < missingKeys.length; i++) {
      const k = missingKeys[i];
      console.log('  "' + k + '" : "' + baseJson[k] + '",\n');
    }
  }

  console.log('\n========================================================================================\n');
  if (unusedKeys.length === 0) {
    console.log('Target file has no unused left over translation entries.');
  } else {
    console.log(unusedKeys.length + ' translation entries  no longer needed in target file "' + targetPath + '"\n');
    for (let i = 0; i < unusedKeys.length; i++) {
      const k = unusedKeys[i];
      console.log('  "' + k + '"\n');
    }
  }
}
