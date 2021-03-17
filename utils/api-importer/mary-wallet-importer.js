/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L963
const mnemonics = [
  ['shrug', 'library', 'ecology', 'live', 'carpet', 'body', 'bike', 'grass', 'clown', 'consider', 'drum', 'toe', 'movie', 'fan', 'give'],
  ['shine', 'fetch', 'half', 'orange', 'document', 'creek', 'desk', 'below', 'van', 'output', 'debris', 'topic', 'first', 'below', 'soft'],
  ['surprise', 'noise', 'address', 'earn', 'imitate', 'loyal', 'wolf', 'payment', 'earth', 'frost', 'hunt', 'afford', 'puzzle', 'salute', 'legend'],
  ['try', 'aspect', 'verify', 'elevator', 'blossom', 'remember', 'away', 'include', 'erode', 'castle', 'review', 'leg', 'summer', 'switch', 'width'],
  ['album', 'public', 'spawn', 'snap', 'bunker', 'label', 'grit', 'heavy', 'auto', 'survey', 'palm', 'mean', 'crouch', 'alpha', 'access'],
  ['exclude', 'web', 'uncover', 'century', 'voice', 'praise', 'north', 'floor', 'copy', 'aware', 'lift', 'tomato', 'chalk', 'fringe', 'powder'],
  ['click', 'code', 'cereal', 'opinion', 'doctor', 'quit', 'chicken', 'coach', 'present', 'clinic', 'net', 'marine', 'speed', 'reflect', 'ceiling'],
  ['leader', 'initial', 'ready', 'author', 'still', 'crouch', 'fat', 'resist', 'stadium', 'embark', 'match', 'stem', 'pig', 'motor', 'minor'],
  ['cruel', 'injury', 'safe', 'gravity', 'ladder', 'genius', 'educate', 'collect', 'lizard', 'join', 'wink', 'cruise', 'flight', 'daughter', 'sausage'],
  ['weather', 'grain', 'few', 'awkward', 'behind', 'review', 'order', 'room', 'damage', 'sick', 'gate', 'sponsor', 'guitar', 'cement', 'lady'],
];

const names = [
  'Madelyn',
  'Maggie',
  'Mary',
  'Meadow',
  'Megan',
  'Melissa',
  'Meredith',
  'Mia',
  'Michelle',
  'Monica',
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
        return request.post(`https://localhost:${API_PORT}/v2/wallets`, payload);
      }));
    } else {
      await Promise.all(sampleSize(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
        const name = shuffledNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return axios.post(`http://localhost:${API_PORT}/v2/wallets`, payload);
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
    address_pool_gap: 20
  };
}

main();
