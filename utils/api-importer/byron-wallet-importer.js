/* eslint-disable no-console */
const axios = require('axios');

const mnemonics = [
  ['craft', 'blade', 'oil', 'fork', 'able', 'math', 'cat', 'kidney', 'clutch', 'menu', 'remind', 'clap'],
  ['kitten', 'lesson', 'gravity', 'hurry', 'total', 'today', 'accuse', 'lottery', 'meadow', 'grab', 'shiver', 'elder'],
  ['flash', 'nothing', 'foam', 'hint', 'vague', 'estate', 'innocent', 'lobster', 'brush', 'can', 'spray', 'radio'],
  ['humor', 'meadow', 'now', 'mimic', 'amazing', 'increase', 'wire', 'aerobic', 'jeans', 'sleep', 'step', 'change'],
  ['lady', 'lucky', 'charge', 'peasant', 'start', 'cheese', 'fitness', 'differ', 'city', 'amused', 'multiply', 'west'],
  ['wash', 'truly', 'birth', 'stairs', 'quarter', 'ethics', 'afraid', 'unfold', 'medal', 'park', 'quick', 'short'],
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
  try {
    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index];
      const payload = generateImportPayload(mnemonic, name);
      return axios.post(`http://localhost:${API_PORT}/v2/byron-wallets`, payload);
    }));
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
