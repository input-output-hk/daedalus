'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.prepareTokenBundle = exports.prepareTrezorAuxiliaryData = exports.prepareTrezorWithdrawal = exports.prepareTrezorCertificate = exports.prepareTrezorOutput = exports.prepareTrezorInput = exports.TrezorTransactionSigningMode = void 0;
const ledgerjs_hw_app_cardano_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano');
const transport_1 = require('@trezor/transport');
const lodash_1 = require('lodash');
const hardwareWalletUtils_1 = require('./hardwareWalletUtils');
exports.TrezorTransactionSigningMode = {
  ORDINARY_TRANSACTION: 0,
  POOL_REGISTRATION_AS_OWNER: 1,
};
const prepareTrezorInput = (input) => {
  return {
    path: (0, hardwareWalletUtils_1.derivationPathToString)(
      input.derivationPath
    ),
    prev_hash: input.id,
    prev_index: input.index,
  };
};
exports.prepareTrezorInput = prepareTrezorInput;
const prepareTrezorOutput = (output) => {
  let tokenBundle = [];
  if (output.assets) {
    tokenBundle = (0, exports.prepareTokenBundle)(output.assets);
  }
  if (output.derivationPath) {
    // Change output
    return {
      amount: output.amount.quantity.toString(),
      tokenBundle,
      addressParameters: {
        addressType: 0,
        // BASE address
        path: (0, hardwareWalletUtils_1.derivationPathToString)(
          output.derivationPath
        ),
        stakingPath: "m/1852'/1815'/0'/2/0",
      },
    };
  }
  return {
    address: output.address,
    amount: output.amount.quantity.toString(),
    tokenBundle,
  };
};
exports.prepareTrezorOutput = prepareTrezorOutput;
const prepareTrezorCertificate = (cert) => {
  if (cert.pool) {
    return {
      type: hardwareWalletUtils_1.CERTIFICATE_TYPE[cert.certificateType],
      path: (0, hardwareWalletUtils_1.derivationPathToString)(
        cert.rewardAccountPath
      ),
      pool: ledgerjs_hw_app_cardano_1.utils.buf_to_hex(
        ledgerjs_hw_app_cardano_1.utils.bech32_decodeAddress(cert.pool)
      ),
    };
  }
  return {
    type: hardwareWalletUtils_1.CERTIFICATE_TYPE[cert.certificateType],
    path: (0, hardwareWalletUtils_1.derivationPathToString)(
      cert.rewardAccountPath
    ),
  };
};
exports.prepareTrezorCertificate = prepareTrezorCertificate;
const prepareTrezorWithdrawal = (withdrawal) => {
  return {
    path: (0, hardwareWalletUtils_1.derivationPathToString)(
      withdrawal.derivationPath
    ),
    amount: withdrawal.amount.quantity.toString(),
  };
};
exports.prepareTrezorWithdrawal = prepareTrezorWithdrawal;
const prepareTrezorAuxiliaryData = ({ address, votingKey, nonce }) => ({
  cVoteRegistrationParameters: {
    votePublicKey: votingKey,
    stakingPath: "m/1852'/1815'/0'/2/0",
    paymentAddressParameters: {
      addressType: transport_1.Messages.CardanoAddressType.BASE,
      path: `m/${address.spendingPath}`,
      stakingPath: "m/1852'/1815'/0'/2/0",
    },
    nonce,
    format: transport_1.Messages.CardanoCVoteRegistrationFormat.CIP15, // Catalyst voting format
  },
});
exports.prepareTrezorAuxiliaryData = prepareTrezorAuxiliaryData;
// Helper Methods
const prepareTokenBundle = (assets) => {
  const tokenObject = (0, hardwareWalletUtils_1.groupTokensByPolicyId)(assets);
  const tokenObjectEntries = Object.entries(tokenObject);
  const tokenBundle = (0, lodash_1.map)(
    tokenObjectEntries,
    ([policyId, tokens]) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'unknown'.
      const tokenAmounts = tokens.map(({ assetName, quantity }) => ({
        assetNameBytes: assetName,
        amount: quantity.toString(),
      }));
      return {
        policyId,
        tokenAmounts,
      };
    }
  );
  return tokenBundle;
};
exports.prepareTokenBundle = prepareTokenBundle;
//# sourceMappingURL=shelleyTrezor.js.map
