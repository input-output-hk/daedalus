// @flow

export type RedeemItnRewardsStep = 'configuration' | 'confirmation' | 'result';
// @SMASH TODO - remove testing server
export type SmashServerType =
  | 'testingKnown'
  | 'iohk'
  | 'adaPools'
  | 'custom'
  | 'direct'
  | 'none';
export type DelegationAction = 'join' | 'quit';
