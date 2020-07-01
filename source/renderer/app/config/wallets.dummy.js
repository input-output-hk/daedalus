import BigNumber from 'bignumber.js';

export default [
  {
    id: 'd8b627b6f0cc6009bf12b1c3ca1c38dd1c',
    addressPoolGap: 20,
    name: 'Dummy1',
    amount: new BigNumber('1000000'),
    availableAmount: new BigNumber('1000000'),
    reward: new BigNumber('0'),
    passwordUpdateDate: new Date('2020-07-01T08:27:12.237Z'),
    syncState: {
      status: 'ready',
    },
    isLegacy: false,
    delegatedStakePoolId: null,
    delegationStakePoolStatus: 'not_delegating',
    lastDelegationStakePoolId: null,
    pendingDelegations: [],
    hasPassword: true,
  },
  {
    id: '0a4115e8e38027dae6b15913cfe9ef4',
    addressPoolGap: 20,
    name: 'Dummy2',
    amount: new BigNumber('2000000'),
    availableAmount: new BigNumber('2000000'),
    reward: new BigNumber('0'),
    passwordUpdateDate: new Date('2020-07-01T08:27:12.380Z'),
    syncState: {
      status: 'ready',
    },
    isLegacy: false,
    delegatedStakePoolId: null,
    delegationStakePoolStatus: 'not_delegating',
    lastDelegationStakePoolId: null,
    pendingDelegations: [],
    hasPassword: true,
  },
];
