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
} from '../../../source/renderer/app/stores/GovernanceStore';
import type {
  AppDRepDetail,
} from '../../../source/renderer/app/stores/GovernanceStore';

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 720,
  padding: 24,
};

const STATUS_OPTIONS = {
  Active: 'active',
  Inactive: 'inactive',
};

const withAnchorEntry: AppDRepDetail = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  verifiedName: 'Daedalus Preview DRep',
  doNotList: false,
  drepActivity: 34,
  drepId: 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras',
  status: 'active',
  votingPower: new BigNumber('23137980123456'),
  metadata: {
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
  },
};

const withoutAnchorEntry: AppDRepDetail = {
  ...withAnchorEntry,
  anchor: null,
  metadata: null,
  drepId: 'drep1ygpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqsahpxyl',
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderDetail = (
  entry: AppDRepDetail | null,
  refreshState: GovernanceRefreshState = GovernanceRefreshState.Loaded
) => (
  <div style={CENTERED_STYLE}>
    <DRepDetail
      entry={entry}
      refreshState={refreshState}
      onBackToDirectory={action('onBackToDirectory')}
      onOpenExternalLink={action('onOpenExternalLink')}
      onSelectForDelegation={action('onSelectForDelegation')}
      onToggleFavorite={action('onToggleFavorite')}
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
    const entry: AppDRepDetail = {
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
      ) as AppDRepDetail['status'],
    };
    return renderDetail(entry);
  })
  .add('Loaded — no anchor', () => renderDetail(withoutAnchorEntry))
  .add('Ranking unavailable', () =>
    renderDetail({ ...withAnchorEntry, votingPower: null })
  )
  .add('Loading', () => renderDetail(null, GovernanceRefreshState.Loading))
  .add('Not found', () => renderDetail(null, GovernanceRefreshState.Loaded));
