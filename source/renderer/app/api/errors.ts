import BigNumber from 'bignumber.js';
import get from 'lodash/get';
import { defineMessages } from 'react-intl';
import ApiError, { ErrorType } from '../domains/ApiError';

export const messages = defineMessages({
  // common
  wrongEncryptionPassphrase: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.',
  },
  // wallets
  walletAlreadyExists: {
    id: 'api.errors.WalletAlreadyRestoredError',
    defaultMessage: '!!!Wallet you are trying to restore already exists.',
    description:
      '"Wallet you are trying to restore already exists." error message.',
  },
  forbiddenMnemonic: {
    id: 'api.errors.ForbiddenMnemonicError',
    defaultMessage:
      '!!!Invalid recovery phrase. Submitted recovery phrase is one of the example recovery phrases from the documentation and should not be used for wallets holding funds.',
    description:
      '"Forbidden Mnemonic: an example Mnemonic has been submitted." error message',
  },
  walletAlreadyImported: {
    id: 'api.errors.WalletAlreadyImportedError',
    defaultMessage: '!!!Wallet you are trying to import already exists.',
    description:
      '"Wallet you are trying to import already exists." error message.',
  },
  walletFileImportError: {
    id: 'api.errors.WalletFileImportError',
    defaultMessage:
      '!!!Wallet could not be imported, please make sure you are providing a correct file.',
    description:
      '"Wallet could not be imported, please make sure you are providing a correct file." error message.',
  },
  invalidMnemonic: {
    id: 'global.errors.invalidMnemonic',
    defaultMessage: '!!!Invalid phrase entered, please check.',
    description: 'Error message shown when invalid bip39 mnemonic was entered.',
  },
  walletInternalError: {
    id: 'api.errors.WalletInternalError',
    defaultMessage:
      '!!!The wallet service encountered an internal error. Please check your wallet logs and restart Daedalus.',
    description:
      'Error shown when cardano-wallet returns an unhandled internal error (HTTP 500).',
  },
  // transactions
  notEnoughMoney: {
    id: 'api.errors.NotEnoughMoneyToSendError',
    defaultMessage: '!!!Not enough money to make this transaction.',
    description: '"Not enough money to make this transaction." error message.',
  },
  canNotCalculateTransactionFees: {
    id: 'api.errors.CanNotCalculateTransactionFeesError',
    defaultMessage:
      '!!!Cannot calculate fees while there are pending transactions.',
    description:
      '"Cannot calculate fees while there are pending transactions." error message',
  },
  cannotCoverFee: {
    id: 'api.errors.NotEnoughFundsForTransactionFeesError',
    defaultMessage: '!!!Not enough ada for fees. Try sending a smaller amount.',
    description:
      '"Not enough ada for fees. Try sending a smaller amount." error message',
  },
  inputsDepleted: {
    id: 'api.errors.inputsDepleted',
    defaultMessage:
      '!!!Your wallet contains only reward funds. Please send at least one ADA to your wallet so that you can spend the funds.',
    description:
      '"Your wallet contains only reward funds. Please send at least one ADA to your wallet so that you can spend the funds." error message',
  },
  transactionIsTooBig: {
    id: 'api.errors.TooBigTransactionError',
    defaultMessage: '!!!Transaction too big due to too many inputs.',
    description: '"Transaction too big due to too many inputs." error message.',
  },
  notEnoughFundsForTransaction: {
    id: 'api.errors.NotEnoughFundsForTransactionError',
    defaultMessage: '!!!Not enough ada. Try sending a smaller amount.',
    description:
      '"Not enough ada . Try sending a smaller amount." error message',
  },
  notEnoughFundsRewardsHeld: {
    id: 'api.errors.notEnoughFundsRewardsHeld',
    defaultMessage:
      '!!!Not enough ada. Only funds other than this wallet\'s staking rewards count for this payment. The rewards cannot be withdrawn while the wallet\'s voting power is not delegated, for example because its DRep retired. Send a smaller amount, or delegate the voting power to any DRep, Abstain or No Confidence in the <a href="#/governance/delegate">Governance Center</a>. Once the delegation is confirmed, the rewards can be withdrawn and spent.',
    description:
      "Shown when a payment needs more than the wallet's funds other than its staking rewards, and the rewards cannot be withdrawn because the wallet's voting power is not delegated. HTML. The link opens the voting power delegation form.",
  },
  notEnoughFundsRewardsHeldRegistration: {
    id: 'api.errors.notEnoughFundsRewardsHeldRegistration',
    defaultMessage:
      '!!!Not enough ada. Only funds other than this wallet\'s staking rewards count for this registration. The rewards cannot be withdrawn while the wallet\'s voting power is not delegated, for example because its DRep retired. Delegate the voting power to any DRep, Abstain or No Confidence in the <a href="#/governance/delegate">Governance Center</a>. Once the delegation is confirmed, a payment from this wallet withdraws the rewards, and they then count for the registration.',
    description:
      "Shown on the Catalyst voting registration step when the wallet's funds other than its staking rewards do not cover the registration, and the rewards cannot be withdrawn because the wallet's voting power is not delegated. HTML. The link opens the voting power delegation form.",
  },
  fundsHeldByPendingTransaction: {
    id: 'api.errors.fundsHeldByPendingTransaction',
    defaultMessage:
      '!!!This wallet\'s funds are held by a pending transaction, so a new transaction cannot be built until that one is confirmed or removed. A pending transaction that is never confirmed expires two hours after it was sent. Once it has been pending for 10 minutes, it can be cancelled in the <a href="#/wallets/{walletId}/transactions">wallet\'s transaction list</a>.',
    description:
      "Shown when a transaction cannot be built because a pending transaction holds the wallet's funds. HTML. {walletId} is the wallet's id and the link opens its transaction list.",
  },
  drepNotRegistered: {
    id: 'api.errors.drepNotRegistered',
    defaultMessage:
      '!!!This DRep is not registered, and the Cardano ledger refuses a delegation to a DRep that is not registered. A retired DRep is no longer registered. Choose another DRep, Abstain or No Confidence.',
    description:
      'Shown when the wallet refuses a vote delegation because the chosen DRep is not registered on the ledger.',
  },
  invalidAddress: {
    id: 'api.errors.invalidAddress',
    defaultMessage: '!!!Please enter a valid address.',
    description: 'Error message shown when invalid address was entered.',
  },
  utxoTooSmall: {
    id: 'api.errors.utxoTooSmall',
    defaultMessage: '!!!Invalid transaction.',
    description: '"Invalid transaction." error message',
  },
  nothingToMigrate: {
    id: 'api.errors.nothingToMigrate',
    defaultMessage:
      '!!!Funds cannot be transferred from this wallet because it contains some unspent transaction outputs (UTXOs), with amounts of ada that are too small to be migrated.',
    description:
      '"Funds cannot be transferred from this wallet because it contains some unspent transaction outputs (UTXOs), with amounts of ada that are too small to be migrated." error message',
  },
  invalidSmashServer: {
    id: 'api.errors.invalidSmashServer',
    defaultMessage: '!!!This URL is not a valid SMASH server',
    description: '"This URL is not a valid SMASH server" error message',
  },
  cannotLeaveWalletEmpty: {
    id: 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens',
    defaultMessage:
      '!!!Insufficient funds to support tokens. You need at least an additional {adaAmount} ADA in your wallet to process this transaction.',
    description:
      '"Balance after transaction would not leave enough ada in the wallet to support tokens remaining in wallet',
  },
  withdrawalNotPossibleWithoutVote: {
    id: 'api.errors.withdrawalNotPossibleWithoutVote',
    defaultMessage:
      '!!!This transaction withdraws this wallet\'s staking rewards. They cannot be withdrawn while the wallet\'s voting power is not delegated, for example because its DRep retired. Delegate the voting power to any DRep, Abstain or No Confidence in the <a href="#/governance/delegate">Governance Center</a>. The rewards can be withdrawn once the delegation is confirmed.',
    description:
      "Shown when a transaction that withdraws staking rewards is refused, by the wallet before submission or by the node, because the wallet's reward account has no vote delegation. HTML. The link opens the voting power delegation form.",
  },
});

export type Balances = {
  walletBalance: BigNumber;
  availableBalance: BigNumber;
  rewardsBalance: BigNumber;
  // Rewards the ledger refuses to release; absent means none.
  heldRewardsBalance?: BigNumber;
};

export const handleNotEnoughMoneyError = (
  error: ErrorType,
  balance: Balances
) => {
  let notEnoughMoneyError;

  const {
    walletBalance,
    availableBalance,
    rewardsBalance,
    heldRewardsBalance,
  } = balance;

  if (walletBalance.gt(availableBalance)) {
    // 1. Amount exceeds availableBalance due to pending transactions:
    // - walletBalance > availableBalance
    // = show "Cannot calculate fees while there are pending transactions."
    notEnoughMoneyError = 'canNotCalculateTransactionFees';
  } else if (
    !walletBalance.isZero() &&
    walletBalance.isEqualTo(rewardsBalance)
  ) {
    // 2. Wallet contains only rewards:
    // - walletBalance === rewardsBalance
    // = show "Cannot send from a wallet that contains only rewards balances."
    notEnoughMoneyError = 'inputsDepleted';
  } else if (heldRewardsBalance?.gt(0)) {
    // 3. The only further funds are staking rewards the ledger holds back:
    // - heldRewardsBalance > 0
    // = show "Not enough ada. Only funds other than this wallet's staking rewards count..."
    notEnoughMoneyError = 'notEnoughFundsRewardsHeld';
  } else {
    // 4. Amount exceeds walletBalance:
    // - walletBalance === availableBalance
    // = show "Not enough Ada. Try sending a smaller amount."
    notEnoughMoneyError = 'notEnoughFundsForTransaction';
  }

  // ApiError with logging showcase
  throw new ApiError(error, {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'boolean' is not assignable to type 'Record<s... Remove this comment to see the full error message
    logError: true,
    msg: 'AdaApi::calculateTransactionFee error',
  })
    .set(notEnoughMoneyError, true)
    .where('code', 'not_enough_money')
    .set('utxoTooSmall', true, {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'exec' does not exist on type '{}'.
      minimumAda: get(
        /(Expected min coin value: +)([0-9]+.[0-9]+)/.exec(error.message),
        2,
        0
      ),
    })
    .where('code', 'utxo_too_small')
    .set('invalidAddress')
    .where('code', 'bad_request')
    .inc('message', 'Unable to decode Address')
    .result();
};
