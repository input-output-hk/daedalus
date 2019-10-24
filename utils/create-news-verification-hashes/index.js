/**
 * SCRIPT FOR NEWS FEED VERIFICATION HASH CREATION
 *
 * COMMANDS:
 * 1. Fetch ALL files and create hashes:
 *   yarn create-news-verification-hashes
 *
 * 2. Fetch file for specific environment and create hash:
 *   NEWS_ENV=development yarn create-news-verification-hashes
 *
 * 3. Fetch files from specific GH branch (default: develop) and create hash:
 *   GH_BRANCH=develop yarn create-news-verification-hashes
 *
 * 4. Fetch files from specific GH branch (default: develop) and for specific environment (default: *) and create hash:
 *   NEWS_ENV=development GH_BRANCH=develop yarn create-news-verification-hashes
 *
 * EXAMPLE:
 *  NEWS_ENV=development GH_BRANCH=feature/ddw-907-add-internal-link-support-in-newsfeed-items yarn create-news-verification-hashes
 */

const lodash = require('lodash');
const https = require('https');
const crypto = require('crypto');

externalRequest = (file) => (
  new Promise((resolve, reject) => {
    const ghBranch = process.env.GH_BRANCH || 'develop';
    const options = {
      hostname: 'raw.githubusercontent.com',
      path: `/input-output-hk/daedalus/${ghBranch}/source/renderer/app/config/newsfeed-files/${file.name}`,
      method: 'GET',
      protocol: 'https:',
    }

    const req = https.request(options);
    req.on('response', response => {
      let body = '';
      response.on('data', chunk => {
        body += chunk;
      });
      response.on('end', () => {
        try {
          // Check if file exist on server
          if (response.statusCode !== 200) {
            return reject(new Error('NOT_FOUND'));
          }

          // Check if file is valid JSON file
          let parsedFile;
          try {
            parsedFile = JSON.parse(body);
          } catch (err) {
            return reject(new Error('NOT_VALID_JSON_FORMAT'));
          }

          // Check all timestamps in file and throw error if there are duplicates
          const timestamps = lodash.map(parsedFile.items, (item => (item.date)))
          const hasDuplicatedTimestamps = lodash.uniq(timestamps).length !== timestamps.length;
          if (hasDuplicatedTimestamps) {
            return reject(new Error('HAS_DUPLICATES'));
          }

          // Create varification hash
          const hash = crypto.createHash('sha256');
          const hashBuffer = hash.digest(hash.update(body, 'utf8'));
          const hashArray = Array.from(new Uint8Array(hashBuffer))
          const verificationHash = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
          resolve({
            verificationHash,
            fileName: file.name,
            env: file.env,
            filtUpdatedAt: parsedFile.updatedAt
          });

        } catch (error) {
          reject(new Error(error));
        }
      });
    });
    req.end()
  }
));

const getVerificationHashForFile = async (file) => {
  try {
    const data = await externalRequest(file);
    console.log('\n \x1b[32m', `${lodash.capitalize(file.env)} (${file.name})`, '\x1b[0m');
    console.log(`New verification FILE NAME: \x1b[36m ${data.filtUpdatedAt}.txt \x1b[0m | HASH: \x1b[36m ${data.verificationHash}`, '\x1b[0m');
  } catch (error) {
    if (error.message === 'NOT_FOUND') { // File not found
      console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');
      console.log(`\x1b[31m File: ${file.name} NOT FOUND`, '\x1b[0m')
    } else if (error.message === 'HAS_DUPLICATES') { // File has one or more same timestamps
      console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');
      console.log(`\x1b[31m File: ${file.name} has duplicated TIMESTAMPS. Please check file and try again!`, '\x1b[0m')
    } else if (error.message === 'NOT_VALID_JSON_FORMAT') { // File is not valid file in JSON format
      console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');
      console.log(`\x1b[31m File: ${file.name} is not VALID json file. Please check file and try again!`, '\x1b[0m')
    } else { // Default server error
      console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');
      console.log('\x1b[31m', 'An Error occured', '\x1b[0m');
    }
  }
}

// Start Script
console.log('\n\x1b[36m%s\x1b[0m', 'Creating news verification hashes...\n');

const newsEnvironment = process.env.NEWS_ENV;
const allowedFiles = [
  {name: 'newsfeed_development.json', env: 'development'},
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
  getVerificationHashForFile(file)
});
