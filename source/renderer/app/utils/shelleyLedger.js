'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.prepareBody = exports.prepareTxAux = exports.cborizeTxAuxiliaryVotingData = exports.cborizeTxVotingRegistration = exports.prepareLedgerAuxiliaryData = exports.prepareLedgerOutput = exports.prepareLedgerInput = exports.indexIsHardened = exports.CachedDeriveXpubFactory = exports.ShelleySignedTransactionStructured = exports.ShelleyTxAux = exports.ShelleyTtl = exports.ShelleyFee = exports.prepareLedgerWithdrawal = exports.prepareLedgerCertificate = exports.ShelleyTxWithdrawal = exports.ShelleyTxCert = exports.ShelleyTxOutput = exports.prepareTokenBundle = exports.ShelleyTxOutputAssets = exports.ShelleyTxInputFromUtxo = exports.ShelleyTxWitnessShelley = exports.derivationScheme = exports.HARDENED_THRESHOLD = exports.CATALYST_VOTING_REGISTRATION_TYPE = void 0;
const ledgerjs_hw_app_cardano_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano');
const address_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address');
const borc_1 = require('borc');
const blakejs_1 = __importDefault(require('blakejs'));
const lodash_1 = __importDefault(require('lodash'));
const hardwareWalletUtils_1 = require('./hardwareWalletUtils');
const getHardwareWalletChannel_1 = require('../ipc/getHardwareWalletChannel');
const WalletAddress_1 = require('../domains/WalletAddress');
exports.CATALYST_VOTING_REGISTRATION_TYPE = 'CATALYST_VOTING';
// Constants
exports.HARDENED_THRESHOLD = 0x80000000;
exports.derivationScheme = {
  type: 'v2',
  ed25519Mode: 2,
  keyfileVersion: '2.0.0',
};
// Constructors
const ShelleyTxWitnessShelley = (publicKey, signature) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny([publicKey, signature]);
  }
  return {
    publicKey,
    signature,
    encodeCBOR,
  };
};
exports.ShelleyTxWitnessShelley = ShelleyTxWitnessShelley;
const ShelleyTxInputFromUtxo = (utxoInput) => {
  const { address, amount, id, index } = utxoInput;
  const coins = amount.quantity;
  const outputNo = index;
  const txHash = Buffer.from(id, 'hex');
  function encodeCBOR(encoder) {
    return encoder.pushAny([txHash, outputNo]);
  }
  return {
    txid: id,
    coins,
    address,
    outputNo,
    encodeCBOR,
  };
};
exports.ShelleyTxInputFromUtxo = ShelleyTxInputFromUtxo;
const ShelleyTxOutputAssets = (assets) => {
  const policyIdMap = new Map();
  const tokenObject = (0, hardwareWalletUtils_1.groupTokensByPolicyId)(assets);
  Object.entries(tokenObject).forEach(([policyId, tokens]) => {
    const assetMap = new Map();
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    lodash_1.default.map(tokens, (token) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'assetName' does not exist on type 'unkno... Remove this comment to see the full error message
      assetMap.set(Buffer.from(token.assetName, 'hex'), token.quantity);
    });
    policyIdMap.set(Buffer.from(policyId, 'hex'), assetMap);
  });
  return policyIdMap;
};
exports.ShelleyTxOutputAssets = ShelleyTxOutputAssets;
const prepareTokenBundle = (assets) => {
  const tokenObject = (0, hardwareWalletUtils_1.groupTokensByPolicyId)(assets);
  const tokenObjectEntries = Object.entries(tokenObject);
  const tokenBundle = lodash_1.default.map(
    tokenObjectEntries,
    ([policyId, tokens]) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'unknown'.
      const tokensList = tokens.map(({ assetName, quantity }) => ({
        assetNameHex: assetName,
        amount: quantity.toString(),
      }));
      return {
        policyIdHex: policyId,
        tokens: tokensList,
      };
    }
  );
  return tokenBundle;
};
exports.prepareTokenBundle = prepareTokenBundle;
function ShelleyTxOutput(output, addressStyle) {
  const { address, amount, derivationPath, assets } = output;
  const adaCoinQuantity = amount.quantity;
  const coins =
    assets && assets.length > 0
      ? [adaCoinQuantity, (0, exports.ShelleyTxOutputAssets)(assets)]
      : adaCoinQuantity;
  function encodeCBOR(encoder) {
    const addressBuff =
      addressStyle === WalletAddress_1.AddressStyles.ADDRESS_SHELLEY
        ? ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(address)
        : (0, address_1.base58_decode)(address);
    return encoder.pushAny([addressBuff, coins]);
  }
  const isChange = derivationPath !== null;
  return {
    address,
    coins,
    isChange,
    spendingPath: isChange
      ? (0, hardwareWalletUtils_1.derivationPathToLedgerPath)(derivationPath)
      : null,
    stakingPath: isChange ? [2147485500, 2147485463, 2147483648, 2, 0] : null,
    encodeCBOR,
  };
}
exports.ShelleyTxOutput = ShelleyTxOutput;
function ShelleyTxCert(cert) {
  const { type, accountAddress, pool } = cert;
  let hash;
  let poolHash;
  if (pool) {
    poolHash = ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
      ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(pool)
    );
    hash = Buffer.from(poolHash, 'hex');
  }
  function encodeCBOR(encoder) {
    const accountAddressHash = ledgerjs_hw_app_cardano_1.utils
      .bech32_decodeAddress(accountAddress)
      .slice(1);
    const account = [0, accountAddressHash];
    const encodedCertsTypes = {
      [0]: [type, account],
      [1]: [type, account],
      [2]: [type, account, hash],
    };
    return encoder.pushAny(encodedCertsTypes[type]);
  }
  return {
    address: accountAddress,
    type,
    accountAddress,
    poolHash: poolHash || null,
    encodeCBOR,
  };
}
exports.ShelleyTxCert = ShelleyTxCert;
const ShelleyTxWithdrawal = (withdrawals) => {
  function encodeCBOR(encoder) {
    const withdrawalMap = new Map();
    lodash_1.default.map(withdrawals, (withdrawal) => {
      const rewardAccount = ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(
        withdrawal.stakeAddress
      );
      const coin = withdrawal.amount.quantity;
      withdrawalMap.set(rewardAccount, coin);
    });
    return encoder.pushAny(withdrawalMap);
  }
  return {
    withdrawals,
    encodeCBOR,
  };
};
exports.ShelleyTxWithdrawal = ShelleyTxWithdrawal;
const prepareLedgerCertificate = (cert) => {
  return {
    type: hardwareWalletUtils_1.CERTIFICATE_TYPE[cert.certificateType],
    params: {
      stakeCredential: {
        type: ledgerjs_hw_app_cardano_1.StakeCredentialParamsType.KEY_PATH,
        keyPath: (0, hardwareWalletUtils_1.derivationPathToLedgerPath)(
          cert.rewardAccountPath
        ),
      },
      poolKeyHashHex: cert.pool
        ? ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
            ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(cert.pool)
          )
        : null,
    },
  };
};
exports.prepareLedgerCertificate = prepareLedgerCertificate;
const prepareLedgerWithdrawal = (withdrawal) => {
  return {
    stakeCredential: {
      type: ledgerjs_hw_app_cardano_1.StakeCredentialParamsType.KEY_PATH,
      keyPath: (0, hardwareWalletUtils_1.derivationPathToLedgerPath)(
        withdrawal.derivationPath
      ),
    },
    amount: withdrawal.amount.quantity.toString(),
  };
};
exports.prepareLedgerWithdrawal = prepareLedgerWithdrawal;
const ShelleyFee = (fee) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny(fee);
  }
  return {
    fee,
    encodeCBOR,
  };
};
exports.ShelleyFee = ShelleyFee;
const ShelleyTtl = (ttl) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny(ttl);
  }
  return {
    ttl,
    encodeCBOR,
  };
};
exports.ShelleyTtl = ShelleyTtl;
const ShelleyTxAux = (
  inputs,
  outputs,
  fee,
  ttl,
  certs,
  withdrawals,
  auxiliaryData,
  auxiliaryDataHash
) => {
  const blake2b = (data) => blakejs_1.default.blake2b(data, null, 32);
  function getId() {
    return blake2b(
      (0, borc_1.encode)(
        (0, exports.ShelleyTxAux)(
          inputs,
          outputs,
          fee,
          ttl,
          certs,
          withdrawals,
          auxiliaryData,
          auxiliaryDataHash
        )
      ) // 32
    ).toString('hex');
  }
  function encodeCBOR(encoder) {
    const txMap = new Map();
    txMap.set(0, inputs);
    txMap.set(1, outputs);
    txMap.set(2, fee);
    txMap.set(3, ttl);
    if (certs && certs.length) txMap.set(4, certs);
    if (withdrawals) txMap.set(5, withdrawals);
    if (auxiliaryDataHash) txMap.set(7, Buffer.from(auxiliaryDataHash, 'hex'));
    return encoder.pushAny(txMap);
  }
  return {
    getId,
    inputs,
    outputs,
    fee,
    ttl,
    certs,
    withdrawals,
    auxiliaryData,
    auxiliaryDataHash,
    encodeCBOR,
  };
};
exports.ShelleyTxAux = ShelleyTxAux;
const ShelleySignedTransactionStructured = (
  txAux,
  witnesses,
  txAuxiliaryData
) => {
  function getId() {
    return txAux.getId();
  }
  function encodeCBOR(encoder) {
    return encoder.pushAny([txAux, witnesses, txAuxiliaryData]);
  }
  return {
    getId,
    witnesses,
    txAux,
    txAuxiliaryData,
    encodeCBOR,
  };
};
exports.ShelleySignedTransactionStructured = ShelleySignedTransactionStructured;
const CachedDeriveXpubFactory = (deriveXpubHardenedFn) => {
  const derivedXpubs = {};
  let xpubMemo;
  const deriveXpub = async (absDerivationPath, xpubHex) => {
    if (xpubHex) xpubMemo = xpubHex;
    const memoKey = JSON.stringify(absDerivationPath);
    let derivedXpubsMemo = await derivedXpubs[memoKey];
    if (!derivedXpubsMemo) {
      const deriveHardened =
        absDerivationPath.length === 0 ||
        (0, exports.indexIsHardened)(absDerivationPath.slice(-1)[0]);
      derivedXpubsMemo = deriveHardened
        ? await deriveXpubHardenedFn(xpubMemo)
        : await deriveXpubNonhardenedFn(absDerivationPath);
    }
    /*
     * the derivedXpubs map stores promises instead of direct results
     * to deal with concurrent requests to derive the same xpub
     */
    return derivedXpubsMemo;
  };
  const deriveXpubNonhardenedFn = async (derivationPath) => {
    const lastIndex = derivationPath.slice(-1)[0];
    const parentXpub = await deriveXpub(derivationPath.slice(0, -1), null);
    try {
      const parentXpubHex = ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
        parentXpub
      );
      const derivedXpub = await getHardwareWalletChannel_1.deriveXpubChannel.request(
        {
          parentXpubHex,
          lastIndex,
          derivationScheme: exports.derivationScheme.ed25519Mode,
        }
      );
      return ledgerjs_hw_app_cardano_1.utils.hex_to_buf(derivedXpub);
    } catch (e) {
      throw e;
    }
  };
  return deriveXpub;
};
exports.CachedDeriveXpubFactory = CachedDeriveXpubFactory;
// Helpers
const indexIsHardened = (index) => {
  return index >= exports.HARDENED_THRESHOLD;
};
exports.indexIsHardened = indexIsHardened;
const prepareLedgerInput = (input) => {
  return {
    txHashHex: input.id,
    outputIndex: input.index,
    path: (0, hardwareWalletUtils_1.derivationPathToLedgerPath)(
      input.derivationPath
    ),
  };
};
exports.prepareLedgerInput = prepareLedgerInput;
const prepareLedgerOutput = (output, addressStyle) => {
  const isChange = output.derivationPath !== null;
  let tokenBundle = [];
  if (output.assets) {
    tokenBundle = (0, exports.prepareTokenBundle)(output.assets);
  }
  if (isChange) {
    return {
      destination: {
        type: ledgerjs_hw_app_cardano_1.TxOutputDestinationType.DEVICE_OWNED,
        params: {
          type:
            ledgerjs_hw_app_cardano_1.AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
          params: {
            spendingPath: (0, hardwareWalletUtils_1.derivationPathToLedgerPath)(
              output.derivationPath
            ),
            stakingPath: (0, address_1.str_to_path)("1852'/1815'/0'/2/0"),
          },
        },
      },
      amount: output.amount.quantity.toString(),
      tokenBundle,
    };
  }
  return {
    destination: {
      type: ledgerjs_hw_app_cardano_1.TxOutputDestinationType.THIRD_PARTY,
      params: {
        addressHex:
          addressStyle === WalletAddress_1.AddressStyles.ADDRESS_SHELLEY
            ? ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
                ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(
                  output.address
                )
              )
            : ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
                (0, address_1.base58_decode)(output.address)
              ),
      },
    },
    amount: output.amount.quantity.toString(),
    tokenBundle,
  };
};
exports.prepareLedgerOutput = prepareLedgerOutput;
const prepareLedgerAuxiliaryData = (txAuxiliaryData) => {
  const { votingPubKey, rewardDestinationAddress, type } = txAuxiliaryData;
  if (type === exports.CATALYST_VOTING_REGISTRATION_TYPE) {
    return {
      type: ledgerjs_hw_app_cardano_1.TxAuxiliaryDataType.CIP36_REGISTRATION,
      params: {
        format: ledgerjs_hw_app_cardano_1.CIP36VoteRegistrationFormat.CIP_15,
        voteKeyHex: votingPubKey,
        stakingPath: rewardDestinationAddress.stakingPath,
        paymentDestination: {
          type: ledgerjs_hw_app_cardano_1.TxOutputDestinationType.DEVICE_OWNED,
          params: {
            type:
              ledgerjs_hw_app_cardano_1.AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
            params: {
              stakingPath: rewardDestinationAddress.stakingPath,
              spendingPath: (0, address_1.str_to_path)(
                rewardDestinationAddress.address.spendingPath
              ),
            },
          },
        },
        nonce: `${txAuxiliaryData.nonce}`,
      },
    };
  }
  // Regular tx has no voting metadata
  return null;
};
exports.prepareLedgerAuxiliaryData = prepareLedgerAuxiliaryData;
const cborizeTxVotingRegistration = ({
  votingPubKey,
  stakePubKey,
  rewardDestinationAddress,
  nonce,
}) => {
  return [
    61284,
    new Map([
      [1, Buffer.from(votingPubKey, 'hex')],
      [2, Buffer.from(stakePubKey, 'hex')],
      [
        3,
        ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(
          rewardDestinationAddress.address.id
        ),
      ],
      [4, Number(nonce)],
    ]),
  ];
};
exports.cborizeTxVotingRegistration = cborizeTxVotingRegistration;
const cborizeTxAuxiliaryVotingData = (txAuxiliaryData, signatureHex) => [
  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
  new Map([
    (0, exports.cborizeTxVotingRegistration)(txAuxiliaryData),
    [61285, new Map([[1, Buffer.from(signatureHex, 'hex')]])],
  ]),
  [],
];
exports.cborizeTxAuxiliaryVotingData = cborizeTxAuxiliaryVotingData;
const prepareTxAux = ({
  txInputs,
  txOutputs,
  fee,
  ttl,
  certificates,
  withdrawals,
  txAuxiliaryData,
  txAuxiliaryDataHash,
}) => {
  const txFee = (0, exports.ShelleyFee)(fee);
  const txTtl = (0, exports.ShelleyTtl)(ttl);
  const txCerts = certificates;
  const txWithdrawals = withdrawals;
  return (0, exports.ShelleyTxAux)(
    txInputs,
    txOutputs,
    txFee,
    txTtl,
    txCerts,
    txWithdrawals,
    txAuxiliaryData,
    txAuxiliaryDataHash
  );
};
exports.prepareTxAux = prepareTxAux;
const prepareBody = (unsignedTx, txWitnesses, txAuxiliaryData) => {
  const signedTransactionStructure = (0,
  exports.ShelleySignedTransactionStructured)(
    unsignedTx,
    txWitnesses,
    txAuxiliaryData
  );
  return (0, borc_1.encode)(signedTransactionStructure).toString('hex');
};
exports.prepareBody = prepareBody;
//# sourceMappingURL=shelleyLedger.js.map
