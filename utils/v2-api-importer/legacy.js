/* eslint-disable no-console */
const axios = require('axios')

const mnemonics = [
  ['connect', 'fish', 'fitness', 'palace', 'electric', 'suit', 'student', 'page', 'home', 'scissors', 'moon', 'staff'],
  ['judge', 'sting', 'fish', 'script', 'silent', 'soup', 'chef', 'very', 'employ', 'wage', 'cloud', 'tourist'],
  ['collect', 'fold', 'file', 'clown', 'injury', 'sun', 'brass', 'diet', 'exist', 'spike', 'behave', 'clip'],
  ['arctic', 'decade', 'pink', 'easy', 'jar', 'index', 'base', 'bright', 'vast', 'ocean', 'hard', 'pizza'],
]

const walletNames = [
  'Rosalind',
  'Dorothy',
  'Gertrude',
  'Irène',
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
