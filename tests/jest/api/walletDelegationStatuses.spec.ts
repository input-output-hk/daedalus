import { WalletDelegationStatuses } from '../../../source/renderer/app/domains/Wallet';

describe('WalletDelegationStatuses wire literals', () => {
  it('pins VOTING_AND_DELEGATING to the voting_and_delegating wire literal', () => {
    expect(WalletDelegationStatuses.VOTING_AND_DELEGATING).toBe(
      'voting_and_delegating'
    );
  });

  it('pins the remaining statuses to their wire literals', () => {
    expect(WalletDelegationStatuses.DELEGATING).toBe('delegating');
    expect(WalletDelegationStatuses.NOT_DELEGATING).toBe('not_delegating');
    expect(WalletDelegationStatuses.VOTING).toBe('voting');
  });
});
