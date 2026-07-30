import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepDetail from '../../../source/renderer/app/components/governance/drep-detail/DRepDetail';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import type {
  AnchorEnrichEntry,
  AppDRepDirectoryEntry,
  DRepCohortContext,
} from '../../../source/renderer/app/stores/GovernanceStore';
import { AnchorFetchErrorType } from '../../../source/common/types/governance.types';

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 720,
  padding: 24,
};

const STATUS_OPTIONS = {
  Active: 'active',
  Inactive: 'inactive',
};

const ANCHOR_STATE_OPTIONS = {
  Verified: 'verified',
  'Verified — prose only': 'verified-minimal',
  Unavailable: 'unavailable',
  'Not requested': 'none',
};

const anchorStateFor = (
  choice: string,
  entry: AppDRepDirectoryEntry
): AnchorEnrichEntry | null => {
  const hash = entry.anchor?.hash;
  if (!hash) return null;
  if (choice === 'verified') {
    return {
      state: 'verified',
      hash,
      host: 'governance-preview.example.org',
      content: {
        givenName: 'Daedalus Preview DRep',
        objectives:
          'Advocate for treasury discipline and predictable protocol parameter changes.',
        motivations:
          'Long-term stake pool operator with an interest in governance participation.',
        qualifications:
          'Five years operating Cardano infrastructure; contributor to two CIPs.',
        references: [
          {
            type: 'link',
            label: 'Public blog',
            uri: 'https://governance-preview.example.org/blog',
          },
          {
            type: 'identity',
            label: 'Social profile',
            uri: 'https://governance-preview.example.org/profile',
          },
          {
            type: 'other',
            label: null,
            uri: 'https://governance-preview.example.org/misc',
          },
        ],
        paymentAddress: 'addr1qxpreviewstatedpaymentaddressvalue',
        doNotList: false,
      },
    };
  }
  if (choice === 'verified-minimal') {
    return {
      state: 'verified',
      hash,
      host: 'governance-preview.example.org',
      content: {
        givenName: null,
        objectives:
          'Advocate for treasury discipline and predictable protocol parameter changes.',
        motivations: null,
        qualifications: null,
        references: [],
        paymentAddress: null,
        doNotList: false,
      },
    };
  }
  if (choice === 'unavailable') {
    return {
      state: 'unavailable',
      hash,
      reason: AnchorFetchErrorType.HttpStatus,
    };
  }
  return null;
};

const withAnchorEntry: AppDRepDirectoryEntry = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  verifiedName: null,
  drepActivity: 34,
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k4',
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
};

const withoutAnchorEntry: AppDRepDirectoryEntry = {
  ...withAnchorEntry,
  anchor: null,
  drepId: 'drep1xj23tk3y_qyv7c9m2z89w3t8mvk9e2uwc3q8u6j7r2x5y9w0p1',
};

const storyCohort: DRepCohortContext = {
  medianVotingPower: new BigNumber('1000000'),
  memberIds: new Set([withAnchorEntry.drepId]),
  verifiedMetadataIds: new Set([withAnchorEntry.drepId]),
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderDetail = (
  entry: AppDRepDirectoryEntry | null,
  refreshState: GovernanceRefreshState = GovernanceRefreshState.Loaded,
  votingPowerState: VotingPowerEnrichState = VotingPowerEnrichState.Loaded,
  anchorState: AnchorEnrichEntry | null = null
) => (
  <div style={CENTERED_STYLE}>
    <DRepDetail
      anchorState={anchorState}
      cohort={storyCohort}
      entry={entry}
      onBackToDirectory={action('onBackToDirectory')}
      onOpenExternalLink={action('onOpenExternalLink')}
      onSelectForDelegation={action('onSelectForDelegation')}
      refreshState={refreshState}
      votingPowerState={votingPowerState}
    />
  </div>
);

const drepStoryDecorator = (story: () => React.ReactNode) => (
  <StoryProvider>
    <StoryDecorator>{story()}</StoryDecorator>
  </StoryProvider>
);

storiesOf('Governance / DRep Detail', module)
  .addDecorator(drepStoryDecorator)
  .addDecorator(withKnobs)
  .add('Loaded — with anchor', () => {
    const entry = {
      ...withAnchorEntry,
      drepActivity: number('Remaining epochs (drepActivity)', 34, {
        max: 60,
        min: 0,
        range: true,
        step: 1,
      }),
      status: select(
        'Status',
        STATUS_OPTIONS,
        'active'
      ) as AppDRepDirectoryEntry['status'],
    };
    return renderDetail(
      entry,
      GovernanceRefreshState.Loaded,
      VotingPowerEnrichState.Loaded,
      anchorStateFor(
        select('Anchor state', ANCHOR_STATE_OPTIONS, 'verified'),
        entry
      )
    );
  })
  .add('Loaded — no anchor', () => renderDetail(withoutAnchorEntry))
  .add('Ranking unavailable', () =>
    renderDetail(
      { ...withAnchorEntry, votingPower: null },
      GovernanceRefreshState.Loaded,
      VotingPowerEnrichState.Failed
    )
  )
  .add('Loading', () => renderDetail(null, GovernanceRefreshState.Loading))
  .add('Not found', () => renderDetail(null, GovernanceRefreshState.Loaded));
