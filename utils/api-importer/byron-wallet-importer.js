/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');

const mnemonics = [
  ['surface', 'light', 'ridge', 'alter', 'reflect', 'digital', 'field', 'sibling', 'moon', 'giant', 'target', 'bleak'],
  ['firm', 'scare', 'trap', 'reopen', 'window', 'govern', 'truck', 'negative', 'fragile', 'share', 'setup', 'coral'],
  ['zero', 'heart', 'develop', 'wolf', 'salute', 'parade', 'supreme', 'pigeon', 'fragile', 'glue', 'manual', 'cloth'],
  ['mobile', 'phone', 'apple', 'reduce', 'organ', 'since', 'grace', 'song', 'pole', 'heavy', 'speak', 'danger'],
  ['vibrant', 'verb', 'entry', 'record', 'sketch', 'aspect', 'ensure', 'true', 'caution', 'long', 'suggest', 'draft'],
  ['diary', 'setup', 'wire', 'claim', 'tattoo', 'street', 'slice', 'skin', 'neck', 'sunset', 'frame', 'tip'],
];

const walletNames = [
  'Rosalind',
  'Dorothy',
  'Gertrude',
  'Irène',
  'Lorenzo',
  'Valentina',
];

const API_PORT = process.env.API_PORT || 8088;

async function main() {
  const httpsAgent = new https.Agent({
    cert: fs.readFileSync('tls/client/client.pem'),
    key: fs.readFileSync('tls/client/client.key'),
    ca: fs.readFileSync('tls/client/ca.crt'),
  });
  const request = axios.create({ httpsAgent })
  try {
    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index];
      const data = generateImportPayload(mnemonic, name);
      return request.post(`https://localhost:${API_PORT}/v2/byron-wallets`, data);
    }))
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
