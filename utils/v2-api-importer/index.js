const axios = require('axios')
const fs = require('fs')

const faucetTx = fs.readFileSync('utils/v2-api-importer/faucet-txt.b64').toString()

// There are 100 mnemonics with funds we could use, but 10 will do 
const mnemonics = fs.readFileSync('utils/v2-api-importer/mnemonics.txt')
  .toString()
  .split('\n')
  .slice(0, 5)

const walletNames = [
  'Bob',
  'Alice',
  'Jane',
  'Charles',
  'Jason'
]

const API_PORT = process.env.API_PORT || 8088
const NODE_PORT = process.env.NODE_PORT || 8888

async function main() {
  try {
    await axios.post(`http://localhost:${NODE_PORT}/local/txs/signed`, {
      signedTx: faucetTx
    })

    // Wait for tx to propogate
    console.log('Waiting for tx to propogate...')
    await new Promise(res => setTimeout(res, 15000))


    await Promise.all(mnemonics.map((mnemonic, index) => {
      const name = walletNames[index]
      const payload = generateImportPayload(mnemonic, name)
      return axios.post(`http://localhost:${API_PORT}/v2/wallets`, payload)
    }))
  } catch (e) {
    console.log(e)
  }
}

function generateImportPayload(mnemonic, name) {
  return {
    name,
    mnemonic_sentence: mnemonic.split(' '),
    passphrase: 'Secure Passphrase',
    address_pool_gap: 20
  }
}

main()