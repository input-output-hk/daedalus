/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1067
const mnemonics = [
  ['public', 'wild', 'salad', 'cereal', 'when', 'zone', 'ship', 'circle', 'other', 'second', 'time', 'priority', 'select', 'apart', 'social'],
  ['report', 'weird', 'border', 'gesture', 'since', 'earn', 'motor', 'elbow', 'huge', 'pilot', 'cool', 'civil', 'duty', 'outer', 'exhaust'],
  ['illegal', 'uncover', 'fruit', 'april', 'snap', 'army', 'brown', 'sister', 'situate', 'lunch', 'they', 'fog', 'isolate', 'earn', 'vocal'],
  ['knife', 'satisfy', 'measure', 'around', 'time', 'thought', 'cigar', 'boss', 'truck', 'bar', 'mushroom', 'hold', 'raccoon', 'asset', 'canvas'],
  ['amazing', 'pole', 'kiss', 'expose', 'whip', 'unfair', 'example', 'slice', 'great', 'they', 'element', 'claw', 'photo', 'dwarf', 'green'],
  ['round', 'trend', 'rescue', 'flight', 'awkward', 'enemy', 'luggage', 'range', 'eagle', 'shaft', 'giggle', 'double', 'pencil', 'jazz', 'home'],
  ['talent', 'example', 'renew', 'true', 'amused', 'alcohol', 'immune', 'exclude', 'cat', 'ceiling', 'squeeze', 'cover', 'slender', 'pond', 'turkey'],
  ['box', 'elegant', 'raccoon', 'brick', 'uphold', 'behind', 'blame', 'marble', 'tip', 'move', 'gift', 'juice', 'crystal', 'circle', 'sound'],
  ['mango', 'street', 'flush', 'universe', 'clap', 'system', 'talk', 'steel', 'tray', 'target', 'forum', 'dust', 'brisk', 'expose', 'prevent'],
  ['behind', 'rib', 'say', 'absorb', 'enroll', 'pyramid', 'balance', 'strategy', 'response', 'evolve', 'pipe', 'dolphin', 'shift', 'flag', 'history'],
];

const names = [
  'Yakov',
  'Yanni',
  'Yara',
  'Yasmin',
  'Yehoshua',
  'Yolanda',
  'Yoseba',
  'Yoshito',
  'Yulia',
  'Yusef',
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
    style: 'icarus',
  };
}

main();
