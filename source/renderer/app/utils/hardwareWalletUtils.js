'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.groupTokensByPolicyId = exports.bech32DecodePublicKey = exports.bech32EncodePublicKey = exports.hardenedPathToDerivationPath = exports.hardenedPathToString = exports.getParamsFromPath = exports.derivationPathToLedgerPath = exports.derivationPathToAddressPath = exports.derivationPathToString = exports.isReceiverAddressType = exports.KEY_PREFIXES = exports.MAX_HUMAN_ADDRESS_LENGTH = exports.PATH_ROLE_IDENTITY = exports.CERTIFICATE_TYPE = void 0;
const lodash_1 = __importDefault(require('lodash'));
const bech32_1 = require('bech32');
const address_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address');
const hardwareWalletsConfig_1 = require('../config/hardwareWalletsConfig');
// Constants
exports.CERTIFICATE_TYPE = {
  register_reward_account: 0,
  // register_reward_account
  quit_pool: 1,
  // quit_pool
  join_pool: 2, // join_pool
};
exports.PATH_ROLE_IDENTITY = {
  role0: 'utxo_external',
  // address
  role1: 'utxo_internal',
  // change
  role2: 'mutable_account',
  // stake
  role3: 'multisig_script', // script
};
// See src/cardano.h in https://github.com/vacuumlabs/ledger-app-cardano-shelley
exports.MAX_HUMAN_ADDRESS_LENGTH = 150;
// https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md
exports.KEY_PREFIXES = {
  // ...add more keys if needed
  PUBLIC_KEY_WITH_CHAIN_CODE: 'acct_xvk', // Ed25519 public key with chain code
};
// Helpers
const receiverAddressTypes = new Set([0, 1, 2, 3, 4, 5, 6, 7, 8]);
const isReceiverAddressType = (addressType) =>
  receiverAddressTypes.has(addressType);
exports.isReceiverAddressType = isReceiverAddressType;
// [1852H, 1815H, 0H] => m/1852'/1815'/0'
const derivationPathToString = (derivationPath) => {
  let constructedPath = 'm';
  lodash_1.default.map(derivationPath, (chunk) => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  });
  return constructedPath;
};
exports.derivationPathToString = derivationPathToString;
// m/1852'/1815'/0' => 1852'/1815'/0'
const derivationPathToAddressPath = (derivationPath) => {
  const derivationPathString = (0, exports.derivationPathToString)(
    derivationPath
  );
  const addressPath = derivationPathString.replace('m/', '');
  return addressPath;
};
exports.derivationPathToAddressPath = derivationPathToAddressPath;
// [1852H, 1815H, 0H] => [2147485500, 2147485463, 2147483648]
const derivationPathToLedgerPath = (derivationPath) => {
  const transformedPath = lodash_1.default.map(derivationPath, (chunk) =>
    chunk.replace('H', "'")
  );
  const constructedPath = lodash_1.default.join(transformedPath, '/');
  return (0, address_1.str_to_path)(constructedPath);
};
exports.derivationPathToLedgerPath = derivationPathToLedgerPath;
const getParamsFromPath = (derivationPath) => {
  const pathParams = lodash_1.default.takeRight(derivationPath, 2);
  return {
    role: pathParams[0],
    index: pathParams[1],
    roleIdentity: exports.PATH_ROLE_IDENTITY[`role${pathParams[0]}`],
  };
};
exports.getParamsFromPath = getParamsFromPath;
// [2147485500, 2147485463, 2147483648] => 1852'/1815'/0'
const hardenedPathToString = (hardendedPath) => {
  const path = lodash_1.default.map(
    hardendedPath,
    (chunk) => `${chunk - hardwareWalletsConfig_1.HARDENED}H`
  );
  return (0, exports.derivationPathToString)(path).replace('m/', '');
};
exports.hardenedPathToString = hardenedPathToString;
// [2147485500, 2147485463, 2147483648] => [1852H, 1815H, 0H, 0, 1]
const hardenedPathToDerivationPath = (hardendedPath) => {
  const derivationPath = [];
  const constructedDerivationPath = ['m'];
  lodash_1.default.map(hardendedPath, (chunk, index) => {
    let pathChunk = chunk.toString();
    let constructedPathChunk = chunk.toString();
    if (index <= 2) {
      pathChunk = `${chunk - hardwareWalletsConfig_1.HARDENED}H`;
      constructedPathChunk = `${chunk - hardwareWalletsConfig_1.HARDENED}'`;
    }
    derivationPath.push(pathChunk);
    constructedDerivationPath.push(constructedPathChunk);
  });
  return {
    derivationPath,
    constructed: constructedDerivationPath,
  };
};
exports.hardenedPathToDerivationPath = hardenedPathToDerivationPath;
const bech32EncodePublicKey = (data) => {
  const data5bit = bech32_1.bech32.toWords(data);
  return bech32_1.bech32.encode(
    exports.KEY_PREFIXES.PUBLIC_KEY_WITH_CHAIN_CODE,
    data5bit,
    exports.MAX_HUMAN_ADDRESS_LENGTH
  );
};
exports.bech32EncodePublicKey = bech32EncodePublicKey;
const bech32DecodePublicKey = (data) => {
  const { words } = bech32_1.bech32.decode(data, 1000);
  return Buffer.from(bech32_1.bech32.fromWords(words));
};
exports.bech32DecodePublicKey = bech32DecodePublicKey;
const groupTokensByPolicyId = (assets) => {
  const compareStringsCanonically = (string1, string2) =>
    string1.length - string2.length || string1.localeCompare(string2);
  const groupedAssets = {};
  (0, lodash_1.default)(assets)
    .orderBy(['policyId', 'assetName'], ['asc', 'asc'])
    .groupBy(({ policyId }) => policyId)
    .mapValues((tokens) =>
      tokens.map(({ assetName, quantity, policyId }) => ({
        assetName,
        quantity,
        policyId,
      }))
    )
    .map((tokens, policyId) => ({
      policyId,
      assets: tokens.sort((token1, token2) =>
        compareStringsCanonically(token1.assetName, token2.assetName)
      ),
    }))
    .sort((token1, token2) =>
      compareStringsCanonically(token1.policyId, token2.policyId)
    )
    .value()
    .map((sortedAssetsGroup) => {
      groupedAssets[sortedAssetsGroup.policyId] = sortedAssetsGroup.assets;
      return groupedAssets;
    });
  return groupedAssets;
};
exports.groupTokensByPolicyId = groupTokensByPolicyId;
//# sourceMappingURL=hardwareWalletUtils.js.map
