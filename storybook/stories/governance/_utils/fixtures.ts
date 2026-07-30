import BigNumber from 'bignumber.js';
import { select } from '@storybook/addon-knobs';
import Wallet, {
  WalletSyncStateStatuses,
} from '../../../../source/renderer/app/domains/Wallet';
import { LOVELACES_PER_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import type {
  WalletSyncState,
  WalletVotingTarget,
} from '../../../../source/renderer/app/api/wallets/types';
import type { DRepIdentity } from '../../../../source/common/types/governance.types';
import type { AppDRepDirectoryEntry } from '../../../../source/renderer/app/stores/GovernanceStore';

export type CurrentVoteOption =
  | 'noDelegation'
  | 'drepVerified'
  | 'drepUnverified'
  | 'abstain'
  | 'noConfidence';

export const currentVoteOptions: Record<string, CurrentVoteOption> = {
  'Not delegated (warning)': 'noDelegation',
  'DRep — verified anchor': 'drepVerified',
  'DRep — unverified anchor': 'drepUnverified',
  Abstain: 'abstain',
  'No Confidence': 'noConfidence',
};

export function useCurrentVoteKnob(): CurrentVoteOption {
  return select('Current vote (mock)', currentVoteOptions, 'noDelegation');
}

// The unverified pair is copied from the committed CurrentVoteSummary story;
// the verified pair encodes the Cardano Academy preprod DRep key hash. Both
// stay lower-case: the drepIndex lookup canonicalizes to lower-case CIP-129.
const VERIFIED_CIP129 =
  'drep1ytnglv2y7s8dxpmylw35egsum63yqzcm0upvkf7qffg4hhqnhj0yh';
const VERIFIED_CIP105 =
  'drep_vkh1u68mz385pmfswe8m5dx2y8x75fqqkxmlqt9j0sz229dac0zl65v';
const VERIFIED_CREDENTIAL_HEX =
  'e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc';

const UNVERIFIED_CIP129 =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const UNVERIFIED_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const UNVERIFIED_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';

const VERIFIED_DREP: DRepIdentity = {
  raw: VERIFIED_CIP129,
  cip129: VERIFIED_CIP129,
  cip105: VERIFIED_CIP105,
  credentialHex: VERIFIED_CREDENTIAL_HEX,
  credentialType: 'key',
};

const UNVERIFIED_DREP: DRepIdentity = {
  raw: UNVERIFIED_CIP129,
  cip129: UNVERIFIED_CIP129,
  cip105: UNVERIFIED_CIP105,
  credentialHex: UNVERIFIED_CREDENTIAL_HEX,
  credentialType: 'key',
};

export function resolveCurrentVote(
  option: CurrentVoteOption
): WalletVotingTarget | null {
  switch (option) {
    case 'drepVerified':
      return { kind: 'drep', drep: VERIFIED_DREP, source: 'onchain' };
    case 'drepUnverified':
      return { kind: 'drep', drep: UNVERIFIED_DREP, source: 'onchain' };
    case 'abstain':
      return { kind: 'abstain' };
    case 'noConfidence':
      return { kind: 'no_confidence' };
    case 'noDelegation':
    default:
      return null;
  }
}

type WalletSeed = {
  id: string;
  name: string;
  lovelace: string;
  hasPassword: boolean;
  isHardwareWallet: boolean;
  syncState: WalletSyncState;
  votingTarget: WalletVotingTarget | null;
};

const buildWallet = ({ lovelace, ...rest }: WalletSeed): Wallet =>
  new Wallet({
    ...rest,
    addressPoolGap: 20,
    amount: new BigNumber(lovelace).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(lovelace).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(0),
    assets: { available: [], total: [] },
    passwordUpdateDate: new Date(),
    isLegacy: false,
    discovery: 'random',
    delegatedStakePoolId: null,
  });

export function makeGovernanceWallets(option: CurrentVoteOption): Wallet[] {
  return [
    buildWallet({
      id: 'governance-wallet-1',
      name: 'Governance wallet',
      lovelace: '125000000000',
      hasPassword: true,
      isHardwareWallet: false,
      syncState: { status: WalletSyncStateStatuses.READY },
      votingTarget: resolveCurrentVote(option),
    }),
    buildWallet({
      id: 'governance-wallet-2',
      name: 'Ledger governance wallet',
      lovelace: '58000000000',
      hasPassword: false,
      isHardwareWallet: true,
      syncState: { status: WalletSyncStateStatuses.READY },
      votingTarget: null,
    }),
    buildWallet({
      id: 'governance-wallet-3',
      name: 'Syncing wallet',
      lovelace: '42000000000',
      hasPassword: true,
      isHardwareWallet: false,
      syncState: {
        status: WalletSyncStateStatuses.SYNCING,
        progress: { quantity: 50, unit: 'percentage' },
      },
      votingTarget: null,
    }),
  ];
}

export function makeDRepIndex(
  option: CurrentVoteOption
): Map<string, AppDRepDirectoryEntry> {
  const index = new Map<string, AppDRepDirectoryEntry>();

  if (option === 'drepVerified') {
    index.set(VERIFIED_CIP129, {
      drepId: VERIFIED_CIP129,
      votingPower: new BigNumber('4500000000000'),
      status: 'active',
      drepActivity: 30,
      // Real preprod on-chain anchor pair from the epoch-295 drep-state sample.
      anchor: {
        url: 'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld',
        hash: '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1',
      },
      verifiedName: null,
      doNotList: false,
    });
  }

  if (option === 'drepUnverified') {
    index.set(UNVERIFIED_CIP129, {
      drepId: UNVERIFIED_CIP129,
      votingPower: new BigNumber('120000000000'),
      status: 'active',
      drepActivity: 4,
      anchor: null,
      verifiedName: null,
      doNotList: false,
    });
  }

  return index;
}
