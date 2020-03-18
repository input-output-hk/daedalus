/* eslint-disable no-console */
const axios = require('axios')

const mnemonics = [
  ['arctic', 'decade', 'pink', 'easy', 'jar', 'index', 'base', 'bright', 'vast', 'ocean', 'hard', 'pizza'],
  ['finish', 'evoke', 'alone', 'town', 'express', 'wide', 'pair', 'story', 'west', 'safe', 'news', 'wrap'],
  ['fox', 'now', 'hello', 'inmate', 'era', 'jealous', 'cruel', 'wreck', 'dash', 'supply', 'book', 'attend'],
  ['must', 'lock', 'cereal', 'water', 'silver', 'cake', 'circle', 'express', 'sock', 'arm', 'chapter', 'avoid'],
  ['give', 'verb', 'balcony', 'hurdle', 'pistol', 'flee', 'manage', 'barely', 'pulse', 'episode', 'speak', 'school'],
  ['divert', 'entire', 'urge', 'banner', 'repair', 'mechanic', 'muffin', 'illness', 'genre', 'intact', 'coin', 'boss'],
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
  try {
    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index]
      const payload = generateImportPayload(mnemonic, name)
      return axios.post(`http://localhost:${API_PORT}/v2/byron-wallets`, payload)
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
