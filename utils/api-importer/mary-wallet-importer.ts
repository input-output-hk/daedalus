/* eslint-disable no-console */
import axios from "axios";
import https from "https";
import fs from "fs";
import { sampleSize, shuffle } from "lodash";
import { maryMnemonics } from "./mnemonics";

const names = ['Madelyn', 'Maggie', 'Mary', 'Meadow', 'Megan', 'Melissa', 'Meredith', 'Mia', 'Michelle', 'Monica'];
const API_PORT = process.env.API_PORT || 8088;
const IS_HTTPS = process.env.IS_HTTPS || false;
const WALLET_COUNT = process.env.WALLET_COUNT || 3;

(async () => {
  function generateImportPayload(mnemonic, name) {
    return {
      name,
      mnemonic_sentence: mnemonic,
      passphrase: 'Secret1234',
      address_pool_gap: 20
    };
  }

  const shuffledMnemonics = shuffle(maryMnemonics);
  const shuffledNames = shuffle(names);

  try {
    if (IS_HTTPS) {
      const httpsAgent = new https.Agent({
        cert: fs.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/client.pem`),
        key: fs.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/client.key`),
        ca: fs.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/ca.crt`)
      });
      const request = axios.create({
        httpsAgent
      });
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      await Promise.all(sampleSize(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
        const name = shuffledNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return request.post(`https://localhost:${API_PORT}/v2/wallets`, payload);
      }));
    } else {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      await Promise.all(sampleSize(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
        const name = shuffledNames[index];
        const payload = generateImportPayload(mnemonic, name);
        return axios.post(`http://localhost:${API_PORT}/v2/wallets`, payload);
      }));
    }
  } catch (e) {
    console.log(e);
  }
})();
