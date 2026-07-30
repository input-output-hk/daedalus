import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepCategoryBadge from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';
import type { DRepCategorySource } from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';
import type { DRepCohortContext } from '../../../source/renderer/app/stores/GovernanceStore';

const ROW_STYLE = {
  display: 'flex',
  flexWrap: 'wrap' as const,
  gap: 16,
  padding: 24,
};

const highValueEntry: DRepCategorySource = {
  drepActivity: 20,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k1',
  votingPower: new BigNumber('900000000'),
};

const thresholdEntry: DRepCategorySource = {
  drepActivity: 9,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k2',
  votingPower: new BigNumber('600000000'),
};

const primaryEntry: DRepCategorySource = {
  drepActivity: 20,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k3',
  votingPower: new BigNumber('400000000'),
};

const nonMetadataEntry: DRepCategorySource = {
  drepActivity: 20,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
  votingPower: new BigNumber('300000000'),
};

const verifiedMetadataIds = new Set([
  highValueEntry.drepId,
  primaryEntry.drepId,
]);

// The median sits below the high-value entry's power and above the primary
// entry's, so both classifications are visible at once.
const inCohortContext: DRepCohortContext = {
  medianVotingPower: new BigNumber('500000000'),
  memberIds: new Set([
    highValueEntry.drepId,
    thresholdEntry.drepId,
    primaryEntry.drepId,
    nonMetadataEntry.drepId,
  ]),
  verifiedMetadataIds,
};

const outOfCohortContext: DRepCohortContext = {
  medianVotingPower: null,
  memberIds: null,
  verifiedMetadataIds,
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / DRep Category Badge', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  .add('All categories', () => {
    const cohort = boolean('In default cohort', true)
      ? inCohortContext
      : outOfCohortContext;
    return (
      <div style={ROW_STYLE}>
        <DRepCategoryBadge entry={highValueEntry} cohort={cohort} />
        <DRepCategoryBadge entry={thresholdEntry} cohort={cohort} />
        <DRepCategoryBadge entry={primaryEntry} cohort={cohort} />
        <DRepCategoryBadge entry={nonMetadataEntry} cohort={cohort} />
      </div>
    );
  });
