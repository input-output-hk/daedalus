import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, select } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import CurrentDRepSummary from '../../../source/renderer/app/components/voting/voting-governance/CurrentDRepSummary';
import type { DRepDelegation } from '../../../source/renderer/app/api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import { resolveCurrentVote, useCurrentVoteKnob } from './_utils/fixtures';

const PANEL_STYLE = {
  margin: '0 auto',
  maxWidth: 640,
  padding: 24,
};

type DRepStatusOption = 'none' | 'active' | 'expiring' | 'inactive';

const DREP_STATUS_OPTIONS: Record<string, DRepStatusOption> = {
  'No record yet': 'none',
  Active: 'active',
  'Expiring soon': 'expiring',
  Inactive: 'inactive',
};

const DREP_ACTIVITY_BY_STATUS: Record<DRepStatusOption, number | null> = {
  none: null,
  active: 30,
  expiring: 4,
  inactive: 0,
};

const resolveDRepEntry = (
  statusOption: DRepStatusOption,
  currentDRep: DRepDelegation | null
): AppDRepDirectoryEntry | null => {
  if (
    statusOption === 'none' ||
    currentDRep == null ||
    currentDRep.kind !== 'drep'
  ) {
    return null;
  }
  return {
    drepId: currentDRep.drep.cip129 ?? currentDRep.drep.raw,
    votingPower: null,
    status: statusOption === 'inactive' ? 'inactive' : 'active',
    drepActivity: DREP_ACTIVITY_BY_STATUS[statusOption],
    anchor: null,
    verifiedName: null,
    doNotList: false,
  };
};

// Locale is intentionally not wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / Current DRep Summary', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  .add('Core states', () => {
    const option = useCurrentVoteKnob();
    const statusOption = select(
      'DRep status (mock)',
      DREP_STATUS_OPTIONS,
      'none'
    );
    const currentDRep = resolveCurrentVote(option);
    return (
      <div style={PANEL_STYLE}>
        <CurrentDRepSummary
          key={option}
          currentDRep={currentDRep}
          drepEntry={resolveDRepEntry(statusOption, currentDRep)}
        />
      </div>
    );
  });
