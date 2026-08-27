import { defineMessages } from 'react-intl';

export default defineMessages({
  title: {
    id: 'dapp.collateral.title',
    defaultMessage: '!!!Preferred collateral',
    description: 'Title of the preferred collateral panel.',
  },
  convention: {
    id: 'dapp.collateral.convention',
    defaultMessage:
      '!!!Daedalus uses 5 ADA as a compatibility convention, not a protocol maximum. This preference does not reserve the output; ordinary transactions may spend it.',
    description: 'Explains the soft preferred collateral policy.',
  },
  checking: {
    id: 'dapp.collateral.checking',
    defaultMessage: '!!!Checking preferred collateral…',
    description: 'Collateral state while wallet data is loading.',
  },
  ready: {
    id: 'dapp.collateral.ready',
    defaultMessage: '!!!Preferred collateral is ready.',
    description: 'Collateral state when a suitable wallet output is selected.',
  },
  notReady: {
    id: 'dapp.collateral.notReady',
    defaultMessage: '!!!No suitable preferred collateral is available.',
    description: 'Collateral state when preparation is available.',
  },
  preparing: {
    id: 'dapp.collateral.preparing',
    defaultMessage:
      '!!!Preparation is waiting for a normal confirmed self-transfer. Complete or cancel the Send flow; Daedalus never signs or submits it automatically.',
    description: 'Collateral state while the user prepares a suitable output.',
  },
  inUse: {
    id: 'dapp.collateral.inUse',
    defaultMessage:
      '!!!Preferred collateral is referenced by a pending transaction.',
    description: 'Collateral state while a pending script transaction uses it.',
  },
  willBeSpent: {
    id: 'dapp.collateral.willBeSpent',
    defaultMessage:
      '!!!An approved ordinary transaction will spend the preferred collateral. Spending remains allowed.',
    description:
      'Warning when an ordinary transaction uses preferred collateral.',
  },
  charged: {
    id: 'dapp.collateral.charged',
    defaultMessage:
      '!!!Preferred collateral was charged by a failed script transaction. A replacement can be prepared.',
    description: 'Collateral state after collateral was charged.',
  },
  stale: {
    id: 'dapp.collateral.stale',
    defaultMessage:
      '!!!The preferred collateral is no longer available. Clear it or prepare a replacement.',
    description: 'Collateral state when the preferred output is unavailable.',
  },
  prepare: {
    id: 'dapp.collateral.prepare',
    defaultMessage: '!!!Prepare collateral',
    description: 'Button opening the normal confirmed Send flow.',
  },
  cancelPreparation: {
    id: 'dapp.collateral.cancelPreparation',
    defaultMessage: '!!!Cancel preparation',
    description: 'Button cancelling a pending collateral preparation intent.',
  },
  clear: {
    id: 'dapp.collateral.clear',
    defaultMessage: '!!!Clear preference',
    description: 'Button clearing preferred collateral without a transaction.',
  },
  repair: {
    id: 'dapp.collateral.repair',
    defaultMessage: '!!!Repair collateral settings',
    description: 'Button repairing corrupt collateral preference metadata.',
  },
  failed: {
    id: 'dapp.collateral.failed',
    defaultMessage: '!!!Preferred collateral could not be updated.',
    description: 'Privacy-safe collateral action failure.',
  },
});
