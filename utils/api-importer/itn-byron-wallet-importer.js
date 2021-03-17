/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1832
const mnemonics = [
  ['phrase', 'rapid', 'fine', 'neglect', 'already', 'nut', 'note', 'chair', 'mushroom', 'rack', 'ivory', 'riot'],
  ['ivory', 'citizen', 'rule', 'scare', 'angle', 'method', 'bounce', 'caution', 'noble', 'pottery', 'plunge', 'resource'],
  ['behave', 'attitude', 'glide', 'else', 'have', 'moon', 'settle', 'minute', 'provide', 'trade', 'negative', 'nothing'],
  ['diary', 'chunk', 'total', 'cruise', 'they', 'curious', 'foil', 'actress', 'wish', 'universe', 'grape', 'kind'],
  ['mushroom', 'print', 'dish', 'slim', 'agent', 'tube', 'expand', 'actor', 'layer', 'idea', 'example', 'quarter'],
  ['riot', 'sport', 'access', 'grid', 'destroy', 'chronic', 'evil', 'doll', 'sibling', 'blanket', 'seed', 'goose'],
  ['pyramid', 'song', 'photo', 'filter', 'subway', 'rich', 'broken', 'anchor', 'blur', 'lecture', 'liar', 'hope'],
  ['sort', 'crouch', 'seven', 'exile', 'extend', 'evoke', 'summer', 'oppose', 'fork', 'result', 'plate', 'goat'],
  ['safe', 'wrap', 'order', 'affair', 'fiber', 'walnut', 'skill', 'timber', 'rookie', 'ghost', 'spot', 'napkin'],
  ['jaguar', 'bitter', 'merry', 'destroy', 'frozen', 'dune', 'embody', 'pull', 'cradle', 'peasant', 'sail', 'whisper'],
];

const names = [
  'Nash',
  'Nate',
  'Nathan',
  'Nelson',
  'Neo',
  'Newton',
  'Nick',
  'Nigel',
  'Nino',
  'Noah',
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
