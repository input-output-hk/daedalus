import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  signTitle: {
    id: 'dapp.batch.approval.signTitle',
    defaultMessage: '!!!Review batch signing request',
    description: 'Title for an ordered CIP-103 batch signing review.',
  },
  submitTitle: {
    id: 'dapp.batch.approval.submitTitle',
    defaultMessage: '!!!Review batch submission',
    description: 'Title for an ordered CIP-103 batch submission review.',
  },
  sign: {
    id: 'dapp.batch.approval.sign',
    defaultMessage: '!!!Sign all transactions',
    description: 'Approve button for CIP-103 batch signing.',
  },
  submit: {
    id: 'dapp.batch.approval.submit',
    defaultMessage: '!!!Submit all transactions',
    description: 'Approve button for CIP-103 batch submission.',
  },
  itemPosition: {
    id: 'dapp.batch.approval.itemPosition',
    defaultMessage: '!!!Item {current} of {total}',
    description: 'Ordered position of one transaction in a CIP-103 batch.',
  },
  itemReady: {
    id: 'dapp.batch.approval.itemReady',
    defaultMessage: '!!!Ready for approval',
    description: 'Status for a supported batch item.',
  },
  itemBlocked: {
    id: 'dapp.batch.approval.itemBlocked',
    defaultMessage: '!!!Cannot be approved',
    description: 'Status for an unsupported or incomplete batch item.',
  },
  refusal: {
    id: 'dapp.batch.approval.refusal',
    defaultMessage:
      '!!!Item {current} cannot be approved. No host or hardware confirmation will start.',
    description:
      'Fail-closed batch review message naming the first blocked item.',
  },
  dependencies: {
    id: 'dapp.batch.approval.dependencies',
    defaultMessage: '!!!Dependencies',
    description:
      'Heading for current-batch and pending transaction dependencies.',
  },
  noDependencies: {
    id: 'dapp.batch.approval.noDependencies',
    defaultMessage: '!!!No current-batch or pending submission dependencies',
    description: 'Shown when a batch item has no dependency.',
  },
  currentDependency: {
    id: 'dapp.batch.approval.currentDependency',
    defaultMessage: '!!!{role} input depends on item {current}',
    description: 'Dependency on an earlier item in the current ordered batch.',
  },
  pendingDependency: {
    id: 'dapp.batch.approval.pendingDependency',
    defaultMessage: '!!!{role} input depends on a pending wallet submission',
    description: 'Dependency on a cardano-wallet pending submission.',
  },
  conflicts: {
    id: 'dapp.batch.approval.conflicts',
    defaultMessage: '!!!Conflicts',
    description: 'Heading for non-blocking sequential batch conflicts.',
  },
  conflict: {
    id: 'dapp.batch.approval.conflict',
    defaultMessage: '!!!{role} input is already claimed by item {current}',
    description: 'Conflict flag naming the first earlier claiming item.',
  },
  effects: {
    id: 'dapp.batch.approval.effects',
    defaultMessage: '!!!Effects for this item only',
    description: 'Heading emphasizing that effects are not aggregated.',
  },
  perItemNotice: {
    id: 'dapp.batch.approval.perItemNotice',
    defaultMessage:
      '!!!Review each item separately. Conflicting effects are never combined into a batch total.',
    description:
      'Warning against treating conflicting batch effects as one total.',
  },
  deviceSigning: {
    id: 'dapp.batch.approval.deviceSigning',
    defaultMessage:
      '!!!Hardware wallets confirm supported items on the device one at a time in this order. No witness set is released unless every item succeeds.',
    description:
      'Ordered hardware batch signing and all-or-nothing release guidance.',
  },
  softwareSigning: {
    id: 'dapp.batch.approval.softwareSigning',
    defaultMessage:
      '!!!Software wallets use this one password approval for the ordered batch. No witness set is released unless every item succeeds.',
    description: 'Single-password software batch signing guidance.',
  },
  submission: {
    id: 'dapp.batch.approval.submission',
    defaultMessage:
      '!!!After approval, every item is attempted in order. If some fail, the result identifies the hash or error for every item.',
    description: 'Attempt-all CIP-103 submission guidance.',
  },
  recovery: {
    id: 'dapp.batch.approval.recovery',
    defaultMessage:
      '!!!If Daedalus closes, unattempted items are not submitted. Pending wallet history remains the recovery record for attempted items.',
    description: 'Batch submission recovery guidance.',
  },
});
