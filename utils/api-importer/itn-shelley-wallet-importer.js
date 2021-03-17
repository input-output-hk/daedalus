/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L159
const mnemonics = [
  ['vintage', 'poem', 'topic', 'machine', 'hazard', 'cement', 'dune', 'glimpse', 'fix', 'brief', 'account', 'badge', 'mass', 'silly', 'business'],
  ['shift', 'mistake', 'rural', 'security', 'inspire', 'loyal', 'wink', 'special', 'blast', 'retreat', 'crouch', 'noise', 'dirt', 'around', 'drastic'],
  ['soldier', 'this', 'verb', 'copper', 'immune', 'unveil', 'engine', 'know', 'tower', 'only', 'foot', 'riot', 'orchard', 'member', 'guitar'],
  ['cupboard', 'fringe', 'garment', 'dawn', 'caught', 'cream', 'alpha', 'sorry', 'unusual', 'federal', 'item', 'leopard', 'lawn', 'rescue', 'desk'],
  ['glad', 'hold', 'sweet', 'tobacco', 'topple', 'rich', 'grab', 'bridge', 'adjust', 'excess', 'improve', 'job', 'lottery', 'diary', 'dust'],
  ['all', 'flee', 'sugar', 'mail', 'response', 'minimum', 'bulk', 'stone', 'cost', 'dynamic', 'forget', 'embrace', 'spray', 'ocean', 'luggage'],
  ['kiwi', 'million', 'space', 'squirrel', 'deliver', 'galaxy', 'cat', 'private', 'meadow', 'canvas', 'differ', 'rescue', 'artist', 'laptop', 'claim'],
  ['length', 'alpha', 'return', 'angle', 'siren', 'buyer', 'reject', 'absurd', 'piece', 'crash', 'toilet', 'flag', 'viable', 'brick', 'sense'],
  ['viable', 'become', 'talk', 'benefit', 'start', 'shield', 'chapter', 'skull', 'donor', 'hip', 'place', 'aware', 'acquire', 'mango', 'hold'],
  ['awkward', 'electric', 'strong', 'early', 'rose', 'abuse', 'mutual', 'limit', 'ketchup', 'child', 'limb', 'exist', 'hurry', 'business', 'whisper'],
];

const names = [
  'Sabrina',
  'Sarah',
  'Scarlett',
  'Sharon',
  'Selena',
  'Siena',
  'Sofia',
  'Sonia',
  'Stella',
  'Stephanie',
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
