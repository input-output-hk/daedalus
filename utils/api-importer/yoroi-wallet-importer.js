/* eslint-disable no-console */
const axios = require('axios');
const https = require('https');
const fs = require('fs');

const mnemonics = [
  ['oblige', 'expire', 'empty', 'style', 'stadium', 'notable', 'usage', 'say', 'language', 'hawk', 'news', 'grab', 'wonder', 'dice', 'come'],
  ['girl', 'hurt', 'cargo', 'save', 'jaguar', 'plastic', 'human', 'void', 'rule', 'major', 'camp', 'click', 'endorse', 'learn', 'want'],
  ['resource', 'fat', 'energy', 'patrol', 'happy', 'song', 'sure', 'crisp', 'argue', 'sunset', 'during', 'heart', 'cabbage', 'asset', 'blossom'],
  ['canoe', 'emerge', 'squeeze', 'south', 'cupboard', 'elbow', 'cinnamon', 'review', 'flock', 'inside', 'mandate', 'hidden', 'hammer', 'virtual', 'bacon'],
  ['stand', 'hint', 'team', 'rebuild', 'glad', 'cruel', 'chunk', 'exact', 'police', 'monkey', 'market', 'bamboo', 'pulse', 'cover', 'provide'],
  ['pair', 'message', 'umbrella', 'talent', 'focus', 'media', 'wheel', 'supreme', 'decade', 'always', 'level', 'prefer', 'bundle', 'dance', 'during'],
  ['shock', 'mouse', 'develop', 'wrestle', 'play', 'chief', 'employ', 'hurdle', 'yellow', 'pig', 'few', 'attack', 'pave', 'second', 'alpha'],
  ['disagree', 'clip', 'card', 'bargain', 'shop', 'sad', 'issue', 'yard', 'divorce', 'sound', 'rural', 'enact', 'cart', 'fresh', 'cluster'],
  ['vacuum', 'cement', 'hollow', 'method', 'cat', 'shoe', 'dial', 'calm', 'congress', 'club', 'student', 'involve', 'slush', 'bracket', 'useful'],
  ['bracket', 'state', 'twin', 'rather', 'toy', 'face', 'click', 'ocean', 'blast', 'carry', 'topic', 'wage', 'cousin', 'brother', 'beach'],
];

const walletNames = [
  'Yakov',
  'Yanni',
  'Yara ',
  'Yasmin ',
  'Yehoshua',
  'Yolanda',
  'Yoseba',
  'Yoshito',
  'Yulia',
  'Yusef',
];

const API_PORT = process.env.API_PORT || 8088;

async function main() {
  const httpsAgent = new https.Agent({
    cert: fs.readFileSync('tls/client/client.pem'),
    key: fs.readFileSync('tls/client/client.key'),
    ca: fs.readFileSync('tls/client/ca.crt'),
  });
  const request = axios.create({ httpsAgent });
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
    style: 'icarus',
  };
}

main();
