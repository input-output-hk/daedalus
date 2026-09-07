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
  collateralEntries: {
    id: 'transaction.approval.collateralEntries',
    defaultMessage: '!!!Collateral — conditional if scripts fail',
  },
  input: {
    id: 'transaction.approval.input',
    defaultMessage: '!!!Input {value}',
  },
  output: {
    id: 'transaction.approval.output',
    defaultMessage: '!!!Output {value}',
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
    defaultMessage: '!!!Waiting for hardware wallet…',
  },
  signing: {
    id: 'transaction.approval.signing',
    defaultMessage: '!!!Signing…',
  },
  submitting: {
    id: 'transaction.approval.submitting',
    defaultMessage: '!!!Submitting transaction {current} of {total}…',
  },
  baseUnits: {
    id: 'transaction.approval.baseUnits',
    defaultMessage: '!!!{value} base units — decimals unknown',
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
