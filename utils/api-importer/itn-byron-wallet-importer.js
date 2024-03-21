"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable no-console */
const axios_1 = __importDefault(require("axios"));
const https_1 = __importDefault(require("https"));
const fs_1 = __importDefault(require("fs"));
const lodash_1 = require("lodash");
const mnemonics_1 = require("./mnemonics");
const names = ['Nash', 'Nate', 'Nathan', 'Nelson', 'Neo', 'Newton', 'Nick', 'Nigel', 'Nino', 'Noah'];
const API_PORT = process.env.API_PORT || 8088;
const IS_HTTPS = process.env.IS_HTTPS || false;
const WALLET_COUNT = process.env.WALLET_COUNT || 3;
(async () => {
    function generateImportPayload(mnemonic, name) {
        return {
            name,
            mnemonic_sentence: mnemonic,
            passphrase: 'Secret1234',
            style: 'random'
        };
    }
    const shuffledMnemonics = (0, lodash_1.shuffle)(mnemonics_1.itnByronMnemonics);
    const shuffledNames = (0, lodash_1.shuffle)(names);
    try {
        if (IS_HTTPS) {
            const httpsAgent = new https_1.default.Agent({
                cert: fs_1.default.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/client.pem`),
                key: fs_1.default.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/client.key`),
                ca: fs_1.default.readFileSync(`${process.env.CARDANO_WALLET_TLS_PATH}/client/ca.crt`)
            });
            const request = axios_1.default.create({
                httpsAgent
            });
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            await Promise.all((0, lodash_1.sampleSize)(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
                const name = shuffledNames[index];
                const payload = generateImportPayload(mnemonic, name);
                return request.post(`https://localhost:${API_PORT}/v2/byron-wallets`, payload);
            }));
        }
        else {
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            await Promise.all((0, lodash_1.sampleSize)(shuffledMnemonics, WALLET_COUNT).map((mnemonic, index) => {
                const name = shuffledNames[index];
                const payload = generateImportPayload(mnemonic, name);
                return axios_1.default.post(`http://localhost:${API_PORT}/v2/byron-wallets`, payload);
            }));
        }
    }
    catch (e) {
        console.log(e);
    }
})();
//# sourceMappingURL=itn-byron-wallet-importer.js.map