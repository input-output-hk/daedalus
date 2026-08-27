import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  signTitle: {
    id: 'dapp.transaction.approval.signTitle',
    defaultMessage: '!!!Review transaction signing request',
    description: 'Title for a CIP-30 transaction signing review.',
  },
  submitTitle: {
    id: 'dapp.transaction.approval.submitTitle',
    defaultMessage: '!!!Review transaction submission',
    description: 'Title for a CIP-30 transaction submission review.',
  },
  sign: {
    id: 'dapp.transaction.approval.sign',
    defaultMessage: '!!!Sign transaction',
    description: 'Approve button for transaction signing.',
  },
  submit: {
    id: 'dapp.transaction.approval.submit',
    defaultMessage: '!!!Submit transaction',
    description: 'Approve button for transaction submission.',
  },
  reject: {
    id: 'dapp.transaction.approval.reject',
    defaultMessage: '!!!Reject',
    description: 'Reject a transaction request.',
  },
  identity: {
    id: 'dapp.transaction.approval.identity',
    defaultMessage: '!!!Request identity',
    description: 'Heading for dApp, wallet, and network identity.',
  },
  effects: {
    id: 'dapp.transaction.approval.effects',
    defaultMessage: '!!!Transaction effects',
    description: 'Heading for all decoded transaction effects.',
  },
  verification: {
    id: 'dapp.transaction.approval.verification',
    defaultMessage: '!!!Exact-byte verification',
    description: 'Heading for hashes and exact CBOR.',
  },
  bodyHash: {
    id: 'dapp.transaction.approval.bodyHash',
    defaultMessage: '!!!Signing body hash',
    description: 'Label for exact transaction body hash.',
  },
  bodyCbor: {
    id: 'dapp.transaction.approval.bodyCbor',
    defaultMessage: '!!!Exact transaction body CBOR',
    description: 'Label for exact transaction body bytes.',
  },
  outerDigest: {
    id: 'dapp.transaction.approval.outerDigest',
    defaultMessage: '!!!Submitted envelope digest',
    description: 'Label for exact full transaction envelope digest.',
  },
  outerCbor: {
    id: 'dapp.transaction.approval.outerCbor',
    defaultMessage: '!!!Exact submitted envelope CBOR',
    description: 'Label for exact full transaction envelope bytes.',
  },
  witnesses: {
    id: 'dapp.transaction.approval.witnesses',
    defaultMessage: '!!!Exact witness set CBOR',
    description: 'Label for exact transaction witness-set bytes.',
  },
  auxiliary: {
    id: 'dapp.transaction.approval.auxiliary',
    defaultMessage: '!!!Exact auxiliary data CBOR',
    description: 'Label for exact auxiliary-data bytes.',
  },
  isValid: {
    id: 'dapp.transaction.approval.isValid',
    defaultMessage: '!!!Submitted isValid: {value}',
    description: 'Exact outer transaction isValid value.',
  },
  auxiliaryHash: {
    id: 'dapp.transaction.approval.auxiliaryHash',
    defaultMessage: '!!!Verified auxiliary-data hash',
    description: 'Label for the verified auxiliary-data commitment.',
  },
  scriptDataHash: {
    id: 'dapp.transaction.approval.scriptDataHash',
    defaultMessage: '!!!Verified script-data hash',
    description: 'Label for the verified script-data commitment.',
  },
  vkeyWitnesses: {
    id: 'dapp.transaction.approval.vkeyWitnesses',
    defaultMessage: '!!!Verified existing VKey witnesses',
    description: 'Label for existing verified VKey witnesses.',
  },
  bootstrapWitnesses: {
    id: 'dapp.transaction.approval.bootstrapWitnesses',
    defaultMessage: '!!!Verified existing bootstrap witnesses',
    description: 'Label for existing verified bootstrap witnesses.',
  },
  maximumCollateral: {
    id: 'dapp.transaction.approval.maximumCollateral',
    defaultMessage: '!!!Maximum collateral loss',
    description: 'Label for maximum collateral at risk.',
  },
  noCollateral: {
    id: 'dapp.transaction.approval.noCollateral',
    defaultMessage: '!!!No collateral at risk',
    description: 'Shown when a transaction has no collateral inputs.',
  },
  collateralWarning: {
    id: 'dapp.transaction.approval.collateralWarning',
    defaultMessage:
      '!!!The isValid flag is not signed. Review the maximum collateral loss as the worst-case signing outcome.',
    description: 'Signing warning about unsigned isValid and collateral.',
  },
  commitmentsVerified: {
    id: 'dapp.transaction.approval.commitmentsVerified',
    defaultMessage: '!!!All context and byte commitments verified',
    description: 'Successful commitment verification status.',
  },
  refusal: {
    id: 'dapp.transaction.approval.refusal',
    defaultMessage:
      '!!!This transaction cannot be approved because review data is incomplete or unsupported.',
    description: 'Fail-closed transaction review message.',
  },
  origin: {
    id: 'dapp.transaction.approval.origin',
    defaultMessage: '!!!Origin: {value}',
    description: 'Origin requesting transaction approval.',
  },
  wallet: {
    id: 'dapp.transaction.approval.wallet',
    defaultMessage: '!!!Wallet: {value}',
    description: 'Wallet selected for transaction approval.',
  },
  network: {
    id: 'dapp.transaction.approval.network',
    defaultMessage: '!!!Network: {value}',
    description: 'Network selected for transaction approval.',
  },
});
