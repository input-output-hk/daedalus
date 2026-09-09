import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'transaction.approval.title',
    defaultMessage: '!!!Review transaction',
  },
  wallet: {
    id: 'transaction.approval.wallet',
    defaultMessage: '!!!Wallet: {value}',
  },
  network: { id: 'transaction.approval.network', defaultMessage: '!!!{value}' },
  fee: { id: 'transaction.approval.fee', defaultMessage: '!!!Transaction fee' },
  collateral: {
    id: 'transaction.approval.collateral',
    defaultMessage: '!!!Collateral at risk',
  },
  upTo: { id: 'transaction.approval.upTo', defaultMessage: '!!!up to {value}' },
  changes: {
    id: 'transaction.approval.changes',
    defaultMessage: '!!!Your wallet changes',
  },
  changeHelp: {
    id: 'transaction.approval.changeHelp',
    defaultMessage:
      '!!!Net change to spendable assets if the transaction succeeds',
  },
  leaving: {
    id: 'transaction.approval.leaving',
    defaultMessage: '!!!Leaving your wallet',
  },
  coming: {
    id: 'transaction.approval.coming',
    defaultMessage: '!!!Coming into your wallet',
  },
  noneLeaving: {
    id: 'transaction.approval.noneLeaving',
    defaultMessage: '!!!No net assets leaving',
  },
  noneComing: {
    id: 'transaction.approval.noneComing',
    defaultMessage: '!!!No net assets coming in',
  },
  unavailable: {
    id: 'transaction.approval.unavailable',
    defaultMessage: '!!!Wallet change unavailable',
  },
  calculation: {
    id: 'transaction.approval.calculation',
    defaultMessage: '!!!How this is calculated',
  },
  gross: {
    id: 'transaction.approval.gross',
    defaultMessage:
      '!!!Uses {inputs} from your wallet; creates {outputs} in it.',
  },
  returned: {
    id: 'transaction.approval.returned',
    defaultMessage:
      '!!!Amounts returned to your wallet are included in the net calculation.',
  },
  grossFlowHelp: {
    id: 'transaction.approval.grossFlowHelp',
    defaultMessage:
      '!!!Inputs and outputs show whole UTxOs, including returned assets. Your net change is shown above.',
    description:
      'Explains why gross input and output values can be larger than the net wallet change.',
  },
  inputs: {
    id: 'transaction.approval.inputs',
    defaultMessage: '!!!Inputs — consumed',
  },
  outputs: {
    id: 'transaction.approval.outputs',
    defaultMessage: '!!!Outputs — created',
  },
  referenceInputs: {
    id: 'transaction.approval.referenceInputs',
    defaultMessage: '!!!Reference inputs — read only, not spent',
  },
  collateralHelp: {
    id: 'transaction.approval.collateralHelp',
    defaultMessage: '!!!Collateral is at risk only if script execution fails.',
    description: 'Explains the condition under which collateral can be lost.',
  },
  collateralInputs: {
    id: 'transaction.approval.collateralInputs',
    defaultMessage: '!!!Collateral inputs — conditional spending',
    description: 'Heading for collateral inputs inspected during signing.',
  },
  collateralReturn: {
    id: 'transaction.approval.collateralReturn',
    defaultMessage: '!!!Collateral return — if scripts fail',
    description:
      'Heading for collateral returned after failed script execution.',
  },
  input: {
    id: 'transaction.approval.input',
    defaultMessage: '!!!Input {value}',
  },
  output: {
    id: 'transaction.approval.output',
    defaultMessage: '!!!Output {value}',
  },
  referenceInput: {
    id: 'transaction.approval.referenceInput',
    defaultMessage: '!!!Reference input {value}',
    description: 'Title for one read-only transaction reference input.',
  },
  collateralInput: {
    id: 'transaction.approval.collateralInput',
    defaultMessage: '!!!Collateral input {value}',
    description: 'Title for one conditional collateral input.',
  },
  collateralReturnEntry: {
    id: 'transaction.approval.collateralReturnEntry',
    defaultMessage: '!!!Collateral return {value}',
    description: 'Title for one collateral return output.',
  },
  thisWallet: {
    id: 'transaction.approval.thisWallet',
    defaultMessage: '!!!This wallet',
  },
  other: {
    id: 'transaction.approval.other',
    defaultMessage: '!!!Other address',
  },
  script: { id: 'transaction.approval.script', defaultMessage: '!!!Script' },
  returnedToWallet: {
    id: 'transaction.approval.returnedToWallet',
    defaultMessage: '!!!Returned to this wallet',
    description:
      'Ownership badge for a normal output controlled by the reviewed wallet.',
  },
  ownershipUnknown: {
    id: 'transaction.approval.ownershipUnknown',
    defaultMessage: '!!!Ownership unknown',
  },
  valueUnavailable: {
    id: 'transaction.approval.valueUnavailable',
    defaultMessage: '!!!Value unavailable',
  },
  fullAddress: {
    id: 'transaction.approval.fullAddress',
    defaultMessage: '!!!Full address',
  },
  copyAddress: {
    id: 'transaction.approval.copyAddress',
    defaultMessage: '!!!Copy address',
  },
  outpoint: {
    id: 'transaction.approval.outpoint',
    defaultMessage: '!!!Transaction input',
  },
  copyOutpoint: {
    id: 'transaction.approval.copyOutpoint',
    defaultMessage: '!!!Copy transaction input',
  },
  fullOutpoint: {
    id: 'transaction.approval.fullOutpoint',
    defaultMessage: '!!!Full transaction input',
    description:
      'Disclosure label for the complete transaction input identity.',
  },
  datum: {
    id: 'transaction.approval.datum',
    defaultMessage: '!!!Datum attached',
  },
  referenceScript: {
    id: 'transaction.approval.referenceScript',
    defaultMessage: '!!!Reference script attached',
  },
  otherActions: {
    id: 'transaction.approval.otherActions',
    defaultMessage: '!!!Other actions',
  },
  minting: {
    id: 'transaction.approval.minting',
    defaultMessage: '!!!Minting and burning',
  },
  withdrawals: {
    id: 'transaction.approval.withdrawals',
    defaultMessage: '!!!Reward withdrawals',
  },
  certificates: {
    id: 'transaction.approval.certificates',
    defaultMessage: '!!!Certificates',
  },
  governance: {
    id: 'transaction.approval.governance',
    defaultMessage: '!!!Governance',
  },
  technical: {
    id: 'transaction.approval.technical',
    defaultMessage: '!!!Technical details and exact transaction bytes',
  },
  checked: {
    id: 'transaction.approval.checked',
    defaultMessage: '!!!Transaction data checked',
  },
  item: {
    id: 'transaction.approval.item',
    defaultMessage: '!!!Transaction {current} of {total}',
  },
  dependencies: {
    id: 'transaction.approval.dependencies',
    defaultMessage: '!!!Depends on transaction {value} in this batch',
  },
  conflict: {
    id: 'transaction.approval.conflict',
    defaultMessage: '!!!Conflicts with transaction {value} in this batch',
  },
  refusal: {
    id: 'transaction.approval.refusal',
    defaultMessage: '!!!This transaction cannot be approved.',
  },
  password: {
    id: 'transaction.approval.password',
    defaultMessage: '!!!Wallet spending password',
  },
  reject: { id: 'transaction.approval.reject', defaultMessage: '!!!Reject' },
  sign: {
    id: 'transaction.approval.sign',
    defaultMessage: '!!!Sign transaction',
  },
  submit: {
    id: 'transaction.approval.submit',
    defaultMessage: '!!!Submit transaction',
  },
  signAndSend: {
    id: 'transaction.approval.signAndSend',
    defaultMessage: '!!!Sign and send',
  },
  device: {
    id: 'transaction.approval.device',
    defaultMessage: '!!!Continue on device',
  },
  deviceAndSend: {
    id: 'transaction.approval.deviceAndSend',
    defaultMessage: '!!!Continue on device and send',
  },
  signingGuidance: {
    id: 'transaction.approval.signingGuidance',
    defaultMessage:
      '!!!Signing authorizes this transaction; the dApp may submit it.',
  },
  submissionGuidance: {
    id: 'transaction.approval.submissionGuidance',
    defaultMessage: '!!!This submits the reviewed transaction.',
  },
  waiting: {
    id: 'transaction.approval.waiting',
    defaultMessage: '!!!Waiting for approval on your hardware wallet…',
  },
  signing: {
    id: 'transaction.approval.signing',
    defaultMessage: '!!!Signing transaction…',
  },
  submitting: {
    id: 'transaction.approval.submitting',
    defaultMessage: '!!!Submitting transaction {current} of {total}…',
  },
  done: {
    id: 'transaction.approval.result.done',
    defaultMessage: '!!!Done',
    description: 'Closes a completed native transaction approval result.',
  },
  submittedTitle: {
    id: 'transaction.approval.result.submittedTitle',
    defaultMessage: '!!!Transaction submitted',
    description: 'Heading shown after the transaction was submitted.',
  },
  submittedMessage: {
    id: 'transaction.approval.result.submittedMessage',
    defaultMessage:
      '!!!Submission acknowledged. Waiting for blockchain confirmation.',
    description:
      'Submission receipt status before the wallet observes blockchain inclusion.',
  },
  rejectedTitle: {
    id: 'transaction.approval.result.rejectedTitle',
    defaultMessage: '!!!Transaction failed',
    description: 'Heading for a failed transaction approval.',
  },
  rejectedMessage: {
    id: 'transaction.approval.result.rejectedMessage',
    defaultMessage:
      '!!!The transaction could not be completed. Check your transaction history before trying again.',
    description:
      'Generic failure copy that does not attribute the failure to the user.',
  },
  deviceRejectedTitle: {
    id: 'transaction.approval.result.deviceRejectedTitle',
    defaultMessage: '!!!Transaction declined on device',
    description:
      'Heading used only when the hardware wallet explicitly reports user refusal.',
  },
  deviceRejectedMessage: {
    id: 'transaction.approval.result.deviceRejectedMessage',
    defaultMessage:
      '!!!The transaction was declined on the hardware wallet. No transaction was submitted.',
    description:
      'Copy used only for the normalized hardware-wallet user-declined code.',
  },
  cancelledTitle: {
    id: 'transaction.approval.result.cancelledTitle',
    defaultMessage: '!!!Transaction cancelled',
    description: 'Heading shown after transaction approval was cancelled.',
  },
  cancelledMessage: {
    id: 'transaction.approval.result.cancelledMessage',
    defaultMessage:
      '!!!The transaction was cancelled before submission. No transaction was submitted.',
    description: 'Explains that cancellation happened before submission.',
  },
  submissionUnknownTitle: {
    id: 'transaction.approval.result.submissionUnknownTitle',
    defaultMessage: '!!!Submission status unknown',
    description:
      'Heading shown when Daedalus cannot determine whether submission succeeded.',
  },
  submissionUnknownMessage: {
    id: 'transaction.approval.result.submissionUnknownMessage',
    defaultMessage:
      '!!!Daedalus could not determine whether the transaction was submitted. Check your transaction history before trying again to avoid duplicate payments or additional fees.',
    description:
      'Warns against retrying while transaction submission status is uncertain.',
  },
  partialTitle: {
    id: 'transaction.approval.result.partialTitle',
    defaultMessage: '!!!Some transactions were submitted',
    description: 'Heading for a partially submitted transaction batch.',
  },
  partialMessage: {
    id: 'transaction.approval.result.partialMessage',
    defaultMessage:
      '!!!Some transactions were submitted, but item {failedItem} failed. Review the transaction IDs before trying again.',
    description:
      'Partial batch result. failedItem is the one-based item number that failed.',
  },
  partialCancelledMessage: {
    id: 'transaction.approval.result.partialCancelledMessage',
    defaultMessage:
      '!!!Some transactions were submitted before the batch was cancelled at item {failedItem}. Review the transaction IDs before trying again.',
    description:
      'Partial batch result when the remaining submission was cancelled.',
  },
  partialDeviceRejectedMessage: {
    id: 'transaction.approval.result.partialDeviceRejectedMessage',
    defaultMessage:
      '!!!Some transactions were submitted, but item {failedItem} was declined on the hardware wallet. Review the transaction IDs before trying again.',
    description:
      'Partial batch result used only for normalized hardware-wallet refusal.',
  },
  transactionIds: {
    id: 'transaction.approval.result.transactionIds',
    defaultMessage: '!!!Transaction IDs',
    description:
      'Heading for returned transaction IDs, including when submission status is unknown.',
  },
  transactionId: {
    id: 'transaction.approval.result.transactionId',
    defaultMessage: '!!!Transaction {value}',
    description: 'One-based label for a returned submitted transaction ID.',
  },
  copyTransactionId: {
    id: 'transaction.approval.result.copyTransactionId',
    defaultMessage: '!!!Copy transaction ID',
    description: 'Copies a returned transaction ID to the clipboard.',
  },
  receiptTitle: {
    id: 'transaction.approval.result.receiptTitle',
    defaultMessage: '!!!Transaction receipt',
    description:
      'Completed transaction approval view, including sign-only results.',
  },
  failedTitle: {
    id: 'transaction.approval.result.failedTitle',
    defaultMessage: '!!!Transaction failed',
    description: 'Receipt heading for a definite failed transaction.',
  },
  failedMessage: {
    id: 'transaction.approval.result.failedMessage',
    defaultMessage:
      '!!!The wallet backend reports this transaction failed. Check your transaction history before trying again.',
    description:
      'Definite failure rather than a lost connection or unknown submission.',
  },
  updatedTitle: {
    id: 'transaction.approval.result.updatedTitle',
    defaultMessage: '!!!Transaction status updates',
    description:
      'Batch receipt where the backend reports different terminal outcomes.',
  },
  updatedMessage: {
    id: 'transaction.approval.result.updatedMessage',
    defaultMessage:
      '!!!Check the status of each transaction below before submitting again.',
    description:
      'Does not describe a mixed-outcome batch as wholly confirmed or failed.',
  },
  confirmedTitle: {
    id: 'transaction.approval.result.confirmedTitle',
    defaultMessage: '!!!Transaction confirmed',
    description:
      'Receipt heading after the backend reports blockchain inclusion.',
  },
  confirmedMessage: {
    id: 'transaction.approval.result.confirmedMessage',
    defaultMessage: '!!!The transaction is recorded on the Cardano blockchain.',
    description:
      'Confirmation observed by the wallet backend, not device approval.',
  },
  signedTitle: {
    id: 'transaction.approval.result.signedTitle',
    defaultMessage: '!!!Transaction signed',
    description: 'Receipt heading for a sign-only dApp request.',
  },
  signedMessage: {
    id: 'transaction.approval.result.signedMessage',
    defaultMessage:
      '!!!The signature was returned to the dApp. Signing does not mean the transaction was submitted or confirmed.',
    description:
      'Makes the sign-only boundary explicit without claiming submission.',
  },
  expiredTitle: {
    id: 'transaction.approval.result.expiredTitle',
    defaultMessage: '!!!Transaction expired',
    description:
      'Receipt heading for a transaction reported expired by the backend.',
  },
  expiredMessage: {
    id: 'transaction.approval.result.expiredMessage',
    defaultMessage:
      '!!!The transaction expired before confirmation. Check your transaction history before trying again.',
    description:
      'Definitive expiry, not an elapsed-time guess or a connection error.',
  },
  viewTransaction: {
    id: 'transaction.approval.result.viewTransaction',
    defaultMessage: '!!!View transaction',
    description: 'Opens and expands this transaction in wallet history.',
  },
  reviewedDetails: {
    id: 'transaction.approval.result.reviewedDetails',
    defaultMessage: '!!!Reviewed transaction details',
    description:
      'Expands the original authorization facts below a completed receipt.',
  },
  transferAmount: {
    id: 'transaction.approval.result.transferAmount',
    defaultMessage: '!!!Amount transferred',
    description:
      'Explicit payment amount matched to a reviewed output; not change or fee.',
  },
  networkFee: {
    id: 'transaction.approval.result.networkFee',
    defaultMessage: '!!!Network fee',
    description:
      'Actual transaction fee, shown separately from transferred amount.',
  },
  walletChange: {
    id: 'transaction.approval.result.walletChange',
    defaultMessage: '!!!Net wallet change',
    description:
      'Net effect on wallet funds, distinct from the payment principal.',
  },
  withinWallet: {
    id: 'transaction.approval.result.withinWallet',
    defaultMessage: '!!!Transfer within this wallet',
    description:
      'Used only when normal input and output ownership establish a self-transfer.',
  },
  awaitingConfirmation: {
    id: 'transaction.approval.result.awaitingConfirmation',
    defaultMessage: '!!!Submitted · Awaiting confirmation',
    description:
      'Individual transaction status before blockchain inclusion is observed.',
  },
  confirmedStatus: {
    id: 'transaction.approval.result.confirmedStatus',
    defaultMessage: '!!!Confirmed',
    description: 'Individual transaction status after blockchain inclusion.',
  },
  expiredStatus: {
    id: 'transaction.approval.result.expiredStatus',
    defaultMessage: '!!!Expired',
    description:
      'Individual transaction status after a definitive backend expiry.',
  },
  failedStatus: {
    id: 'transaction.approval.result.failedStatus',
    defaultMessage: '!!!Failed',
    description:
      'Individual transaction status after a definitive backend rejection.',
  },
  confirmations: {
    id: 'transaction.approval.result.confirmations',
    defaultMessage: '!!!Confirmations: {count}',
    description: 'Block confirmation count provided by the backend.',
  },
  cancel: {
    id: 'transaction.approval.cancel',
    defaultMessage: '!!!Cancel',
    description:
      'Cancels an in-flight hardware approval before submission authorization.',
  },
  baseUnits: {
    id: 'transaction.approval.baseUnits',
    defaultMessage: '!!!{value} base units — decimals unknown',
  },
  rawUnits: {
    id: 'transaction.approval.rawUnits',
    defaultMessage: '!!!{value} base units',
    description:
      'Exact signed native asset quantity before decimal conversion.',
  },
  assetDetails: {
    id: 'transaction.approval.assetDetails',
    defaultMessage: '!!!Asset details',
    description: 'Accessible label for expanding exact native asset identity.',
  },
  entrySection: {
    id: 'transaction.approval.entrySection',
    defaultMessage: '!!!{title} ({count})',
    description: 'Expandable transaction entry group heading with entry count.',
  },
  moreAssets: {
    id: 'transaction.approval.moreAssets',
    defaultMessage: '!!!+{count} more assets',
    description:
      'Disclosure label for additional assets in a gross UTxO value.',
  },
  unnamed: {
    id: 'transaction.approval.unnamed',
    defaultMessage: '!!!Unnamed token',
  },
  flightMainnetFunds: {
    id: 'transaction.approval.flightMainnetFunds',
    defaultMessage:
      '!!!I understand that this Flight build uses real mainnet funds and the transaction is irreversible.',
  },
  undelegationNetworkSupport: {
    id: 'transaction.approval.undelegationNetworkSupport',
    defaultMessage:
      '!!!I understand that undelegated stake does not support the Cardano network.',
  },
  undelegationRewards: {
    id: 'transaction.approval.undelegationRewards',
    defaultMessage:
      '!!!I understand that undelegated stake is not eligible to earn rewards.',
  },
});
