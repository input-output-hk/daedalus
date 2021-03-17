/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');
const { sampleSize, shuffle } = require('lodash');

// Taken from: https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core-integration/src/Test/Integration/Faucet.hs#L1876
const mnemonics = [
  ['ketchup', 'embody', 'define', 'thing', 'few', 'tornado', 'worry', 'few', 'wisdom', 'people', 'sure', 'bean', 'ring', 'impact', 'clerk', 'mirror', 'antenna', 'truly', 'chief', 'truth', 'sign', 'drip', 'sorry', 'flush'],
  ['obscure', 'protect', 'still', 'woman', 'rescue', 'plunge', 'lemon', 'warm', 'cash', 'quote', 'wood', 'adapt', 'erase', 'muffin', 'blush', 'diet', 'noodle', 'biology', 'scrap', 'involve', 'radar', 'filter', 'oval' ,'filter'],
  ['bird', 'toilet', 'maid', 'mule', 'mercy', 'album', 'powder', 'misery', 'ozone', 'fragile', 'concert', 'media', 'inhale', 'lonely', 'height', 'box', 'enforce', 'mesh', 'budget', 'arch', 'top', 'tenant', 'spoil', 'drop'],
  ['gadget', 'rate', 'fame', 'nothing', 'onion', 'surround', 'loan', 'panel', 'moment', 'used', 'fruit', 'jacket', 'pretty', 'replace', 'pig', 'stairs', 'guard', 'slab', 'shadow', 'child', 'over', 'win', 'focus', 'glue'],
  ['amount', 'become', 'cousin', 'degree', 'practice', 'garbage', 'fall', 'witness', 'mushroom', 'update', 'this', 'define', 'exile', 'fame', 'paper', 'symptom', 'ride', 'oil', 'plate', 'park', 'broom', 'fine', 'six', 'coast'],
  ['nasty', 'abstract', 'scale', 'idle', 'benefit', 'staff', 'normal', 'auto', 'anchor', 'balance', 'measure', 'action', 'crucial', 'virtual', 'lobster', 'wave', 'caution', 'text', 'obey', 'enact', 'only', 'nature', 'illness', 'gain'],
  ['beyond', 'rare', 'pulse', 'setup', 'story', 'side', 'envelope', 'illness', 'warm', 'doll', 'snake', 'turtle', 'oak', 'host', 'horse', 'where', 'rate', 'quantum', 'notice', 'allow', 'monkey', 'shallow', 'police' ,'code'],
  ['brief', 'asset', 'spell', 'behave', 'real', 'galaxy', 'dad', 'solar', 'animal', 'wisdom', 'imitate', 'arch', 'abuse', 'parade', 'loud', 'mention', 'volcano', 'fall', 'awake', 'course', 'solution', 'super', 'guitar', 'rebel'],
  ['onion', 'secret', 'sphere', 'horror', 'hint', 'engine', 'denial', 'six', 'omit', 'shove', 'quit', 'sibling', 'code', 'shallow', 'square', 'athlete', 'dog', 'bleak', 'cost', 'axis', 'alone', 'nut', 'frozen', 'stumble'],
  ['about', 'magnet', 'nut', 'edit', 'awake', 'matrix', 'bamboo', 'casual', 'diamond', 'joke', 'man', 'crumble', 'staff', 'ten', 'potato', 'laptop', 'off', 'action', 'chuckle', 'medal', 'bread', 'blind', 'peanut', 'horse'],
];

const names = [
  'Laura',
  'Layla',
  'Leia',
  'Lily',
  'Linda',
  'Louise',
  'Lorraine',
  'Lucille',
  'Luz',
  'Lyn',
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
