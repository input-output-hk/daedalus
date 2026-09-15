import React from 'react';
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

export default {
  title: 'Governance / DRep Detail',

  decorators: [
    (story) => (
      <StoryProvider>
        <StoryDecorator>{story()}</StoryDecorator>
      </StoryProvider>
    ),
    withKnobs,
  ],
};

export const LoadedWithAnchor = {
  render: () => {
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
  },

  name: 'Loaded — with anchor',
};

export const LoadedNoAnchor = {
  render: () => renderDetail(withoutAnchorEntry),
  name: 'Loaded — no anchor',
};

export const LoadedInactiveDRep = {
  render: () =>
    renderDetail({
      ...withAnchorEntry,
      status: 'inactive',
      drepActivity: 0,
    }),

  name: 'Loaded — inactive DRep',
};

export const LoadedAlreadyAFavorite = {
  render: () =>
    renderDetail(withAnchorEntry, GovernanceRefreshState.Loaded, {
      isFavorite: true,
    }),

  name: 'Loaded — already a favorite',
};

export const LoadedPartialMetadata = {
  render: () =>
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
    }),

  name: 'Loaded — partial metadata',
};

export const LoadedAdditionalMetadataFields = {
  render: () =>
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
    }),

  name: 'Loaded — additional metadata fields',
};

export const LoadedAdditionalFieldsTheParserRefuses = {
  render: () =>
    renderDetail({
      ...withAnchorEntry,
      metadata: {
        ...withAnchorEntry.metadata,
        additionalFields: [],
      },
    }),

  name: 'Loaded — additional fields the parser refuses',
};

export const LoadedAnchorVerifiedButEmpty = {
  render: () =>
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
    }),

  name: 'Loaded — anchor verified but empty',
};

export const LoadedOnlyFieldsNoStandardDefines = {
  render: () =>
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
    }),

  name: 'Loaded — only fields no standard defines',
};

export const LoadedAnchorCouldNotBeVerified = {
  render: () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: null,
    }),

  name: 'Loaded — anchor could not be verified',
};

export const LoadedAnchorOnAnUnreachableScheme = {
  render: () =>
    renderDetail({
      ...withAnchorEntry,
      verifiedName: null,
      metadata: null,
      anchor: {
        url: 'ipfs://bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi',
        hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
      },
    }),

  name: 'Loaded — anchor on an unreachable scheme',
};

export const RankingUnavailable = {
  render: () => renderDetail({ ...withAnchorEntry, votingPower: null }),

  name: 'Ranking unavailable',
};

export const LargestKnownMetadata = {
  render: () => renderDetail(largestKnownEntry),
  name: 'Largest known metadata',
};

export const _Loading = () =>
  renderDetail(null, GovernanceRefreshState.Loading);

export const NotFound = {
  render: () => renderDetail(null, GovernanceRefreshState.Loaded),
  name: 'Not found',
};
