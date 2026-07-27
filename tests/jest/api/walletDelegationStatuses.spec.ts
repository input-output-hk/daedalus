import { WalletDelegationStatuses } from '../../../source/renderer/app/domains/Wallet';

// The wire value for dual delegation is 'delegating_and_voting'; the constant
// export name is intentionally kept unchanged.
describe('WalletDelegationStatuses wire literals', () => {
  it('pins VOTING_AND_DELEGATING to the delegating_and_voting wire literal', () => {
    expect(WalletDelegationStatuses.VOTING_AND_DELEGATING).toBe(
      'delegating_and_voting'
    );
  });

  it('pins the remaining statuses to their wire literals', () => {
    expect(WalletDelegationStatuses.DELEGATING).toBe('delegating');
    expect(WalletDelegationStatuses.NOT_DELEGATING).toBe('not_delegating');
    expect(WalletDelegationStatuses.VOTING).toBe('voting');
  });
});
