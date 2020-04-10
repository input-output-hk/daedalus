/* eslint-disable no-console */
const axios = require('axios')
const https = require('https');
const fs = require('fs');

const mnemonics = [
  ['craft', 'blade', 'oil', 'fork', 'able', 'math', 'cat', 'kidney', 'clutch', 'menu', 'remind', 'clap'],
  ['kitten', 'lesson', 'gravity', 'hurry', 'total', 'today', 'accuse', 'lottery', 'meadow', 'grab', 'shiver', 'elder'],
  ['flash', 'nothing', 'foam', 'hint', 'vague', 'estate', 'innocent', 'lobster', 'brush', 'can', 'spray', 'radio'],
  ['humor', 'meadow', 'now', 'mimic', 'amazing', 'increase', 'wire', 'aerobic', 'jeans', 'sleep', 'step', 'change'],
  ['lady', 'lucky', 'charge', 'peasant', 'start', 'cheese', 'fitness', 'differ', 'city', 'amused', 'multiply', 'west'],
  ['wash', 'truly', 'birth', 'stairs', 'quarter', 'ethics', 'afraid', 'unfold', 'medal', 'park', 'quick', 'short'],
]

const walletNames = [
  'Rosalind',
  'Dorothy',
  'Gertrude',
  'Irène',
  'Lorenzo',
  'Valentina',
]

const API_PORT = process.env.API_PORT || 8088

async function main() {
  const httpsAgent = new https.Agent({
    cert: fs.readFileSync('tls/client/client.pem'),
    key: fs.readFileSync('tls/client/client.key'),
    ca: fs.readFileSync('tls/client/ca.crt'),
  });
  const request = axios.create({ httpsAgent })
  try {
    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index]
      const data = generateImportPayload(mnemonic, name)
      return request.post(`https://localhost:${API_PORT}/v2/byron-wallets`, data)
    }))
  } catch (e) {
    console.log(e)
  }
}

function generateImportPayload(mnemonic, name) {
  return {
    name,
    mnemonic_sentence: mnemonic,
    passphrase: 'Secret1234',
    style: 'random',
  }
}

main()
