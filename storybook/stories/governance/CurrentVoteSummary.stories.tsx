import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, select } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import CurrentVoteSummary from '../../../source/renderer/app/components/voting/voting-governance/CurrentVoteSummary';
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';

const PANEL_STYLE = {
  margin: '0 auto',
  maxWidth: 640,
  padding: 24,
};

// Checksum-verified vectors copied byte-for-byte from the committed wallet
// fixtures. Bech32 is case-insensitive, so never re-case or re-derive them.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';

const CURRENT_VOTE_OPTIONS = {
  'Not delegated (warning)': 'noDelegation',
  'DRep — unverified anchor': 'drepUnverified',
  Abstain: 'abstain',
  'No Confidence': 'noConfidence',
};

const resolveCurrentVote = (option: string): WalletVotingTarget | null => {
  switch (option) {
    case 'drepUnverified':
      return {
        kind: 'drep',
        drep: {
          raw: KEY_CIP129,
          cip129: KEY_CIP129,
          cip105: KEY_CIP105,
          credentialHex: KEY_CREDENTIAL_HEX,
          credentialType: 'key',
        },
        source: 'onchain',
      };
    case 'abstain':
      return { kind: 'abstain' };
    case 'noConfidence':
      return { kind: 'no_confidence' };
    case 'noDelegation':
    default:
      return null;
  }
};

// Locale is intentionally not wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / Current Vote Summary', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  .add('Core states', () => {
    const option = select(
      'Current vote (mock)',
      CURRENT_VOTE_OPTIONS,
      'noDelegation'
    );
    return (
      <div style={PANEL_STYLE}>
        <CurrentVoteSummary
          key={option}
          currentVote={resolveCurrentVote(option)}
        />
      </div>
    );
  });
