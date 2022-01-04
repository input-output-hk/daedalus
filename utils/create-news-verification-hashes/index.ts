/* eslint-disable no-console, func-names */

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
// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'lodash'.
const lodash = require('lodash');

// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'crypto'.
const crypto = require('crypto');

// @ts-ignore ts-migrate(2451) FIXME: Cannot redeclare block-scoped variable 'fs'.
const fs = require('fs');

const readModuleFile = (path, callback) => {
  try {
    const filename = require.resolve(path);

    fs.readFile(filename, 'utf8', callback);
  } catch (e) {
    callback(e);
  }
};

(() => {
  () => {
    // Start script
    console.log('\n\x1b[36m%s\x1b[0m', 'Creating news verification hashes...\n');
    const newsEnvironment = process.env.NEWS_ENV;
    const allowedFiles = [{
      name: 'newsfeed_development.json',
      env: 'development'
    }, {
      name: 'newsfeed_mainnet.json',
      env: 'mainnet'
    }, {
      name: 'newsfeed_staging.json',
      env: 'staging'
    }, {
      name: 'newsfeed_testnet.json',
      env: 'testnet'
    }, {
      name: 'news.dummy.json',
      env: 'dummy_development'
    } // Faked test file for development purposes
    ];
    let filesToHash = [];

    if (newsEnvironment) {
      const fileName = `newsfeed_${newsEnvironment.toLowerCase()}.json`;
      const fileAllowed = lodash.find(allowedFiles, allowedFile => allowedFile.name === fileName);

      if (fileAllowed) {
        filesToHash.push(fileAllowed);
      } else {
        console.log(`FILE: \x1b[31m ${fileName} not allowed. Use one of available environments \x1b[0m development | staging | testnet | mainnet \n`, '\x1b[0m');
        return;
      }
    }

    if (filesToHash.length === 0) {
      filesToHash = allowedFiles;
      console.log(`\x1b[36m I am hashing ALL available files: [ \x1b[32m ${lodash.map(filesToHash, file => file.name)} \x1b[36m ]`, '\x1b[0m');
    } else {
      console.log(`\x1b[36m I am hashing files: [ \x1b[32m ${lodash.map(filesToHash, file => file.name)} \x1b[36m ]`, '\x1b[0m');
    }

    console.log('\n \x1b[33m', 'NOTE: create file with NAME and put HASH as content! \n', '\x1b[0m');
    lodash.map(filesToHash, file => {
      readModuleFile(`../../source/renderer/app/config/newsfeed-files/${file.name}`, function (error, fileContent) {
        // Log Environment
        console.log('\n \x1b[32m', `${lodash.capitalize(file.env)}`, '\x1b[0m');

        if (error) {
          // e.g.File not found
          console.log('\x1b[31m', error.message, '\x1b[36m');
        } else {
          // Check if file is valid JSON file
          let parsedFile;

          try {
            parsedFile = JSON.parse(fileContent);
          } catch (err) {
            console.log(`\x1b[31m File: ${file.name} is not VALID json file. Please check file and try again!`, '\x1b[0m');
            return;
          }

          // Check all timestamps in file and throw error if there are duplicates
          const timestamps = lodash.map(parsedFile.items, item => item.date);
          const hasDuplicatedTimestamps = lodash.uniq(timestamps).length !== timestamps.length;

          if (hasDuplicatedTimestamps) {
            console.log(`\x1b[31m File: ${file.name} has duplicated TIMESTAMPS. Please check file and try again!`, '\x1b[0m');
            return;
          }

          // Create verification hash
          // @ts-ignore ts-migrate(2339) FIXME: Property 'createHash' does not exist on type 'Cryp... Remove this comment to see the full error message
          const hash = crypto.createHash('sha256');
          const hashBuffer = hash.digest(hash.update(fileContent, 'utf8'));
          const hashArray = Array.from(new Uint8Array(hashBuffer));
          const verificationHash = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
          console.log(`New verification FILE NAME: \x1b[36m ${parsedFile.updatedAt}.txt \x1b[0m | HASH: \x1b[36m ${verificationHash}`, '\x1b[0m');
        }
      });
    })();
  };
})();