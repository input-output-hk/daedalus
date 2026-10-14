import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import GovernanceShell from './_utils/GovernanceShell';
import { ROUTES } from '../../../source/renderer/app/routes-config';
import { LARGEST_KNOWN_DREP_METADATA } from './_utils/drepPopulation';
import DRepDetail from '../../../source/renderer/app/components/governance/drep-detail/DRepDetail';
import { GovernanceRefreshState } from '../../../source/renderer/app/stores/GovernanceStore';
import type { AppDRepDetail } from '../../../source/renderer/app/stores/GovernanceStore';

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
    additionalFields: [],
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
// Measured against mainnet on 2026-08-20: 1,062 registered DReps holding
// 5.257B ADA between them. Without the totals there is no share to compute, so
// the detail view would state none.
const TOTAL_DREP_STAKE = new BigNumber('5257000000000000');

// Preprod and mainnet: 432,000 slots of one second, so an epoch is five days.
const EPOCH_LENGTH = 432000;
const SLOT_LENGTH = 1;

// Every field at its own observed mainnet maximum on one page: the worst case
// the chain can currently produce, not a plausible one.
const largestKnownEntry: AppDRepDetail = {
  ...withAnchorEntry,
  verifiedName: LARGEST_KNOWN_DREP_METADATA.verifiedName,
  metadata: {
    objectives: LARGEST_KNOWN_DREP_METADATA.objectives,
    motivations: LARGEST_KNOWN_DREP_METADATA.motivations,
    qualifications: LARGEST_KNOWN_DREP_METADATA.qualifications,
    paymentAddress: LARGEST_KNOWN_DREP_METADATA.paymentAddress,
    references: LARGEST_KNOWN_DREP_METADATA.references,
    additionalFields: [],
  },
};

const renderDetail = (
  entry: AppDRepDetail | null,
  refreshState: GovernanceRefreshState = GovernanceRefreshState.Loaded,
  { isFavorite = false }: { isFavorite?: boolean } = {}
) => (
  <GovernanceShell activeTab={ROUTES.GOVERNANCE.DREPS}>
    <DRepDetail
      entry={entry}
      refreshState={refreshState}
      totalDRepStake={TOTAL_DREP_STAKE}
      epochLength={EPOCH_LENGTH}
      slotLength={SLOT_LENGTH}
      isFavorite={isFavorite}
      onBackToDirectory={action('onBackToDirectory')}
      onOpenExternalLink={action('onOpenExternalLink')}
      onSelectForDelegation={action('onSelectForDelegation')}
      onToggleFavorite={action('onToggleFavorite')}
    />
  </GovernanceShell>
);

storiesOf('Governance / DRep Detail', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
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
  // An inactive DRep: its voting power is not counted until it votes again, so
  // the status is the thing a delegator most needs to see here.
  .add('Loaded — inactive DRep', () =>
    renderDetail({
      ...withAnchorEntry,
      status: 'inactive',
      drepActivity: 0,
    })
  )
  // The favourite state changes the action at the top of the page, so it needs
  // a story of its own.
  .add('Loaded — already a favorite', () =>
    renderDetail(withAnchorEntry, GovernanceRefreshState.Loaded, {
      isFavorite: true,
    })
  )
  // Review item 4: a DRep that provided only some of its CIP-119 fields must
  // not show labelled rows with nothing in them, and must not show a
  // References block it never populated.
  .add('Loaded — partial metadata', () =>
    renderDetail({
      ...withAnchorEntry,
      metadata: {
        objectives:
          'Advocate for treasury discipline and predictable protocol parameter changes.',
        motivations: null,
        qualifications: null,
        paymentAddress: null,
        references: [],
        additionalFields: [],
      },
    })
  )
  // The vocabulary DReps actually invent, and the shapes that break layouts.
  // Every key here is either observed on mainnet or a payload the parser has to
  // refuse: a data URI under a key nobody thought to ban, a URL where nothing
  // may become clickable, a collection with no key-value shape at all, and a
  // single unbroken token long enough to push a column off the page.
  .add('Loaded — additional metadata fields', () =>
    renderDetail({
      ...withAnchorEntry,
      metadata: {
        ...withAnchorEntry.metadata,
        additionalFields: [
          {
            key: 'bio',
            value: {
              kind: 'text' as const,
              text: "I'm a Cardano contributor and stake pool operator, active in Catalyst since fund 8 and voting on every governance action since the Chang hard fork.",
            },
          },
          {
            key: 'email',
            value: { kind: 'text' as const, text: 'drep@example.org' },
          },
          {
            key: 'title',
            value: { kind: 'text' as const, text: 'drep.collective - DRep' },
          },
          {
            key: 'rationale',
            value: {
              kind: 'text' as const,
              text: 'My rationale for decision-making starts from the constitution and the long-term health of the treasury rather than from short-term price.',
            },
          },
          {
            key: 'nationality',
            value: { kind: 'text' as const, text: 'Japan' },
          },
          {
            key: 'security',
            value: {
              kind: 'text' as const,
              text: 'DRep keys secured by airgapped hardware.',
            },
          },
          // A URL, which stays text: references is the only field anything
          // becomes clickable from.
          {
            key: 'url',
            value: {
              kind: 'text' as const,
              text: 'https://example.org/drep/profile/2026/statement',
            },
          },
          // No spaces anywhere, so nothing but overflow-wrap can break it.
          {
            key: 'proofOfWorkStatementIdentifierWithNoWordBreaksAnywhereAtAll',
            value: {
              kind: 'text' as const,
              text: 'aVeryLongUnbrokenTokenWithNoSpacesOrHyphensThatMustNotPushTheColumnOffTheRightEdgeOfTheCardOrTheDialogItIsRenderedInsideOfAnywhereAtAll',
            },
          },
          // A key shaped like markup and a claim shaped like an endorsement:
          // both render as the DRep's own words, in the quieter block, under
          // the names they chose.
          {
            key: '<script>alert(1)</script>',
            value: { kind: 'text' as const, text: 'Still just text' },
          },
          {
            key: 'verifiedBy',
            value: { kind: 'text' as const, text: 'Cardano Foundation' },
          },
          // A multi-sig DRep publishing who signs for it. The shape is the
          // content: flattening it loses which name went with which title.
          {
            key: 'members',
            value: {
              kind: 'list' as const,
              items: [
                {
                  kind: 'group' as const,
                  fields: [
                    {
                      key: 'name',
                      value: {
                        kind: 'text' as const,
                        text: 'Sebastien Guillemot セバ',
                      },
                    },
                    {
                      key: 'title',
                      value: { kind: 'text' as const, text: 'Treasurer' },
                    },
                    {
                      key: 'company',
                      value: {
                        kind: 'text' as const,
                        text: 'Example Stake Pool Ltd',
                      },
                    },
                  ],
                },
                {
                  kind: 'group' as const,
                  fields: [
                    {
                      key: 'name',
                      value: {
                        kind: 'text' as const,
                        text: 'Δημήτριος Παπαδόπουλος',
                      },
                    },
                    {
                      key: 'title',
                      value: { kind: 'text' as const, text: 'Secretary' },
                    },
                    {
                      key: 'affiliations',
                      value: {
                        kind: 'list' as const,
                        items: [
                          { kind: 'text' as const, text: 'Catalyst circle' },
                          {
                            kind: 'text' as const,
                            text: 'Interim Constitutional Committee',
                          },
                        ],
                      },
                    },
                  ],
                },
              ],
            },
          },
          { key: 'threshold', value: { kind: 'text' as const, text: '2' } },
          {
            key: '不信任の理由',
            value: {
              kind: 'text' as const,
              text: '説明はこちらに記載しています。',
            },
          },
        ],
      },
    })
  )
  // What the parser refuses, shown as the empty block it produces. A data URI
  // under any key, a collection with no key-value shape, and a nested object
  // all reach the renderer as nothing, so the block does not appear at all
  // rather than appearing broken.
  .add('Loaded — additional fields the parser refuses', () =>
    renderDetail({
      ...withAnchorEntry,
      metadata: {
        ...withAnchorEntry.metadata,
        additionalFields: [],
      },
    })
  )
  // Verified, and empty. The document matched the on-chain hash and turned out
  // to carry none of the fields a profile is built from, which is a different
  // fact from having failed to verify and reads differently.
  .add('Loaded — anchor verified but empty', () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: {
        objectives: null,
        motivations: null,
        qualifications: null,
        paymentAddress: null,
        references: [],
        additionalFields: [],
      },
    })
  )
  // Verified, and not empty, though every field a profile is built from is
  // absent. A document may carry only its author's own vocabulary, and that is
  // still something published.
  .add('Loaded — only fields no standard defines', () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: {
        objectives: null,
        motivations: null,
        qualifications: null,
        paymentAddress: null,
        references: [],
        additionalFields: [
          {
            key: 'twitter',
            value: { kind: 'text' as const, text: '@example_drep' },
          },
          {
            key: 'telegram',
            value: { kind: 'text' as const, text: 't.me/example_drep' },
          },
        ],
      },
    })
  )
  // Not verified. The wallet holds no content for this anchor, which is what
  // both a hash mismatch and an unreachable host produce: a DRep registered a
  // URL and a hash, and nothing matching came back.
  .add('Loaded — anchor could not be verified', () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: null,
    })
  )
  // The same outcome reached a different way. Our fetcher accepts https alone,
  // so an ipfs:// anchor is never retrieved at all and the wallet's own attempt
  // is all there is; when that fails too, the page has a URL nobody can open.
  .add('Loaded — anchor on an unreachable scheme', () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: null,
      anchor: {
        url: 'ipfs://bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi',
        hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
      },
    })
  )
  .add('Ranking unavailable', () =>
    renderDetail({ ...withAnchorEntry, votingPower: null })
  )
  // The worst case mainnet can currently produce: 3,374 characters of
  // motivations, a 76-character name with emoji, eight references and an
  // 80-character reference label, each taken from its own observed maximum.
  .add('Largest known metadata', () => renderDetail(largestKnownEntry))
  .add('Loading', () => renderDetail(null, GovernanceRefreshState.Loading))
  .add('Not found', () => renderDetail(null, GovernanceRefreshState.Loaded));
