/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');

const mnemonics = [
  ['glad', 'hold', 'sweet', 'tobacco', 'topple', 'rich', 'grab', 'bridge', 'adjust', 'excess', 'improve', 'job', 'lottery', 'diary', 'dust'],
  ['all', 'flee', 'sugar', 'mail', 'response', 'minimum', 'bulk', 'stone', 'cost', 'dynamic', 'forget', 'embrace', 'spray', 'ocean', 'luggage'],
  ['kiwi', 'million', 'space', 'squirrel', 'deliver', 'galaxy', 'cat', 'private', 'meadow', 'canvas', 'differ', 'rescue', 'artist', 'laptop', 'claim'],
  ['length', 'alpha', 'return', 'angle', 'siren', 'buyer', 'reject', 'absurd', 'piece', 'crash', 'toilet', 'flag', 'viable', 'brick', 'sense'],
  ['either', 'flip', 'maple', 'shift', 'dismiss', 'bridge', 'sweet', 'reveal', 'green', 'tornado', 'need', 'patient', 'wall', 'stamp', 'pass'],
  ['radar', 'scare', 'sense', 'winner', 'little', 'jeans', 'blue', 'spell', 'mystery', 'sketch', 'omit', 'time', 'tiger', 'leave', 'load'],
];

const walletNames = [
  'Laura',
  'Layla',
  'Linda',
  'Louise',
  'Lorraine',
  'Lucille',
];

const API_PORT = process.env.API_PORT || 8088;
const IS_HTTPS = process.env.IS_HTTPS || false;

async function main() {
  try {
    if (IS_HTTPS) {
      const httpsAgent = new https.Agent({
        cert: fs.readFileSync('tls/client/client.pem'),
        key: fs.readFileSync('tls/client/client.key'),
        ca: fs.readFileSync('tls/client/ca.crt'),
      });
      const request = axios.create({ httpsAgent });
      await Promise.all(mnemonics.map((mnemonic, index) => {
        const name = walletNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return request.post(`https://localhost:${API_PORT}/v2/wallets`, payload);
      }));
    } else {
      await Promise.all(mnemonics.map((mnemonic, index) => {
        const name = walletNames[index];
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
