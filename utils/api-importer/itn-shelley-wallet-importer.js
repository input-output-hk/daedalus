/* eslint-disable no-console */
const axios = require('axios');

const mnemonics = [
  ['awkward', 'electric', 'strong', 'early', 'rose', 'abuse', 'mutual', 'limit', 'ketchup', 'child', 'limb', 'exist', 'hurry', 'business', 'whisper'],
];

const walletNames = [
  'Bob',
];

const API_PORT = process.env.API_PORT || 8088;

async function main() {
  try {
    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index];
      const payload = generateImportPayload(mnemonic, name);
      return axios.post(`http://localhost:${API_PORT}/v2/wallets`, payload);
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
    address_pool_gap: 20
  };
}

main();
