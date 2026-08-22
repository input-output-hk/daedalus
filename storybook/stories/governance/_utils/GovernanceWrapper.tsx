import React from 'react';
import type Wallet from '../../../../source/renderer/app/domains/Wallet';
import type { AppDRepDirectoryEntry } from '../../../../source/renderer/app/stores/GovernanceStore';
import { makeDRepIndex, makeGovernanceWallets } from './fixtures';
import type { CurrentVoteOption } from './fixtures';

export type GovernanceStoryFixtures = {
  wallets: Wallet[];
  drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>;
};

type Props = {
  option: CurrentVoteOption;
  children: (fixtures: GovernanceStoryFixtures) => React.ReactNode;
};

// The React key is the option id verbatim: changing the knob must remount the
// subtree so VotingPowerDelegation drops the form state it holds locally.
export default function GovernanceWrapper({ option, children }: Props) {
  return (
    <React.Fragment key={option}>
      {children({
        wallets: makeGovernanceWallets(option),
        drepIndex: makeDRepIndex(option),
      })}
    </React.Fragment>
  );
}
