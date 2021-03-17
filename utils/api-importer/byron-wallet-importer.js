/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1471
const mnemonics = [
  ['arctic', 'decade', 'pink', 'easy', 'jar', 'index', 'base', 'bright', 'vast', 'ocean', 'hard', 'pizza'],
  ['finish', 'evoke', 'alone', 'town', 'express', 'wide', 'pair', 'story', 'west', 'safe', 'news', 'wrap'],
  ['fox', 'now', 'hello', 'inmate', 'era', 'jealous', 'cruel', 'wreck', 'dash', 'supply', 'book', 'attend'],
  ['must', 'lock', 'cereal', 'water', 'silver', 'cake', 'circle', 'express', 'sock', 'arm', 'chapter', 'avoid'],
  ['give', 'verb', 'balcony', 'hurdle', 'pistol', 'flee', 'manage', 'barely', 'pulse', 'episode', 'speak', 'school'],
  ['divert', 'entire', 'urge', 'banner', 'repair', 'mechanic', 'muffin', 'illness', 'genre', 'intact', 'coin', 'boss'],
  ['pink', 'radio', 'various', 'frame', 'argue', 'draft', 'sun', 'speak', 'club', 'salute', 'thank', 'price'],
  ['all', 'beef', 'link', 'funny', 'swing', 'duck', 'sweet', 'swallow', 'slow', 'shield', 'weekend', 'open'],
  ['green', 'friend', 'captain', 'entry', 'utility', 'lake', 'blur', 'matrix', 'will', 'prefer', 'breeze', 'shed'],
  ['reveal', 'jazz', 'equal', 'salmon', 'first', 'decline', 'liquid', 'wolf', 'powder', 'account', 'elbow', 'figure'],
];

const names = [
  'Barry',
  'Benny',
  'Billy',
  'Blake',
  'Bob',
  'Bowie',
  'Boyd',
  'Bruce',
  'Bruno',
  'Byron',
];

const API_PORT = process.env.API_PORT || 8088;
const IS_HTTPS = process.env.IS_HTTPS || false;
const WALLET_COUNT = process.env.WALLET_COUNT || 3;

async function main() {
  const shuffledMnemonics = shuffle(mnemonics);
  const shuffledNames = shuffle(names);
  try {
    if (IS_HTTPS) {
      const httpsAgent = new https.Agent({
        cert: fs.readFileSync('tls/client/client.pem'),
        key: fs.readFileSync('tls/client/client.key'),
        ca: fs.readFileSync('tls/client/ca.crt'),
      });
      const request = axios.create({ httpsAgent });
      await Promise.all(sampleSize(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
        const name = shuffledNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return request.post(`https://localhost:${API_PORT}/v2/byron-wallets`, payload);
      }));
    } else {
      await Promise.all(sampleSize(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
        const name = shuffledNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return axios.post(`http://localhost:${API_PORT}/v2/byron-wallets`, payload);
      }));
    }
  } catch (e) {
    console.log(e);
  }
}

function generateImportPayload(mnemonic, name) {
  return {
    name,
    mnemonic_sentence: mnemonic,
    passphrase: 'Secret1234',
    style: 'random',
  };
}

main();
