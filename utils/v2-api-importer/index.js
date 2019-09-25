const axios = require('axios')

const mnemonics = [
  ['pass', 'proud', 'clarify', 'cargo', 'control', 'fancy', 'question', 'option', 'bring', 'recall', 'dolphin', 'meat', 'comic', 'version', 'pitch'],
  ['behave', 'worth', 'later', 'banana', 'buzz', 'advance', 'local', 'owner', 'bulb', 'swear', 'theory', 'border', 'elephant', 'armor', 'chest'],
  ['index', 'cover', 'candy', 'upper', 'orient', 'unit', 'shaft', 'peace', 'size', 'lava', 'wood', 'blush', 'huge', 'sunset', 'grid'],
  ['liberty', 'powder', 'spoil', 'health', 'cable', 'problem', 'dice', 'alpha', 'solution', 'wagon', 'ask', 'siege', 'uncle', 'brick', 'life'],
  ['shed', 'comic', 'subject', 'stage', 'dirt', 'yard', 'access', 'good', 'weasel', 'surround', 'since', 'climb', 'menu', 'blossom', 'joy'],
]

const walletNames = [
  'Bob',
  'Alice',
  'Jane',
  'Charles',
  'Jason'
]

const API_PORT = process.env.API_PORT || 8088

async function main() {
  try {
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
    mnemonic_sentence: mnemonic,
    passphrase: 'Secret1234',
    address_pool_gap: 20
  }
}

main()
