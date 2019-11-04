const axios = require('axios')

const mnemonics = [
  ['connect', 'fish', 'fitness', 'palace', 'electric', 'suit', 'student', 'page', 'home', 'scissors', 'moon', 'staff'],
]

const walletNames = [
  'Legacy',
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
  }
}

main()
