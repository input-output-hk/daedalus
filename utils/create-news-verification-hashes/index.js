/**
 * SCRIPT FOR NEWS FEED VERIFICATION HASH CREATION
 *
 * COMMANDS:
 * 1. Fetch ALL files and create hashes:
 *   yarn create-news-verification-hashes
 *
 * 2. Fetch file for specific environment and create hash:
 *   NEWS_ENV=development yarn create-news-verification-hashes
 */

const lodash = require('lodash');
const https = require('https');
const crypto = require('crypto');
const fs = require('fs');

const readModuleFile = (path, callback) => {
  try {
    const filename = require.resolve(path);
    fs.readFile(filename, 'utf8', callback);
  } catch (e) {
    callback(e);
  }
}


// Start script
console.log('\n\x1b[36m%s\x1b[0m', 'Creating news verification hashes...\n');

const newsEnvironment = process.env.NEWS_ENV;
const allowedFiles = [
  {name: 'newsfeed_development1.json', env: 'development'},
  {name: 'newsfeed_mainnet.json', env: 'mainnet'},
  {name: 'newsfeed_staging.json', env: 'staging'},
  {name: 'newsfeed_testnet.json', env: 'testnet'},
  {name: 'news.dummy.json', env: 'dummy_development'}, // Faked test file for development purposes
];

let filesToHash = [];
if (newsEnvironment) {
  const fileName = `newsfeed_${newsEnvironment.toLowerCase()}.json`;
  const fileAllowed = lodash.find(allowedFiles, (allowedFiles => allowedFiles.name === fileName));

  if (fileAllowed) {
    filesToHash.push(fileAllowed);
  } else {
    console.log(`FILE: \x1b[31m ${fileName} not allowed. Use one of available environments \x1b[0m development | staging | testnet | mainnet \n`, '\x1b[0m');
    return;
  }
}

if (filesToHash.length === 0) {
  filesToHash = allowedFiles;
  console.log(`\x1b[36m I am hashing ALL available files: [ \x1b[32m ${lodash.map(filesToHash, file => file.name)} \x1b[36m ]`, '\x1b[0m')
} else {
  console.log(`\x1b[36m I am hashing files: [ \x1b[32m ${lodash.map(filesToHash, file => file.name)} \x1b[36m ]`, '\x1b[0m')
}

console.log('\n \x1b[33m', 'NOTE: create file with NAME and put HASH as content! \n', '\x1b[0m');
lodash.map(filesToHash, file => {

  readModuleFile(`../../source/renderer/app/config/newsfeed-files/${file.name}`, function (error, words) {
    if (error) {
      console.error(error.message)
    } else {
      const hash = crypto.createHash('sha256');
      const hashBuffer = hash.digest(hash.update(words, 'utf8'));
      const hashArray = Array.from(new Uint8Array(hashBuffer))
      const verificationHash = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');

      const parsedFile = JSON.parse(words);
      console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');
      console.log(`New verification FILE NAME: \x1b[36m ${parsedFile.updatedAt}.txt \x1b[0m | HASH: \x1b[36m ${verificationHash}`, '\x1b[0m');
    }
  });
});

