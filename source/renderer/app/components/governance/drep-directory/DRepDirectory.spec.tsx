import React from 'react';
import BigNumber from 'bignumber.js';
import { Cardano } from '@cardano-sdk/core';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  within,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import DRepDirectory from './DRepDirectory';
import { DEFAULT_DREP_COHORT_CRITERIA } from '../_shared/drepCohort';
import type {
  DRepCohortCriteria,
  DRepCohortCriterion,
} from '../_shared/drepCohort';
import {
  GovernanceRefreshState,
  AppDRepDirectoryEntry,
} from '../../../stores/GovernanceStore';
import { logger } from '../../../utils/logging';

// jsdom's Uint8Array constructor lives in a different realm than Node's
// Buffer, so the SDK's bech32 encoder rejects Buffer payloads; point the
// suite's global at Node's realm (decode paths are unaffected).
(global as { Uint8Array: unknown }).Uint8Array = Object.getPrototypeOf(
  Buffer.prototype
).constructor;

const baseEntries: AppDRepDirectoryEntry[] = [
  {
    anchor: null,
    verifiedName: null,
    doNotList: false,
    drepActivity: 12,
    drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
    status: 'active',
    votingPower: new BigNumber('23137980123456'),
  },
];

const buildEntry = (suffix: number): AppDRepDirectoryEntry => ({
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: (suffix % 20) + 1,
  drepId: `drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k${String(
    suffix
  ).padStart(4, '0')}`,
  status: (['active', 'inactive'] as const)[suffix % 2],
  votingPower: new BigNumber(`${23137980123456 + suffix * 1000}`),
});

const paginatedEntries: AppDRepDirectoryEntry[] = Array.from(
  { length: 30 },
  (_, i) => buildEntry(i + 1)
);

// Distinct from the first hash byte so a prefix of one id never matches another.
const credHash = (n: number) =>
  n.toString(16).padStart(2, '0').repeat(28).slice(0, 56);
const realDrepId = (n: number): string =>
  String(
    Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );
const realCip105Id = (n: number): string =>
  String(
    Cardano.DRepID.cip105FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );
const realEntry = (
  n: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: 20,
  drepId: realDrepId(n),
  status: 'active',
  votingPower: new BigNumber(`${1_000_000_000 - n}`),
  ...overrides,
});

const renderComponent = ({
  suggestedDReps = baseEntries,
  allDReps = suggestedDReps,
  favoriteDRepIds = new Set<string>(),
  favoriteEntries = [] as AppDRepDirectoryEntry[],
  listViewMode = undefined as 'cards' | 'table' | undefined,
  onListViewModeChange = jest.fn(),
  onToggleFavorite = jest.fn(),
  view = 'directory' as const,
  onBackToDirectory = jest.fn(),
  isStaleFavoriteEntry = undefined as
    | ((entry: AppDRepDirectoryEntry) => boolean)
    | undefined,
  error = null,
  isNodeInSync = true,
  onReroll = jest.fn(),
  onLoadAllDReps = jest.fn(),
  cohortCriteria = DEFAULT_DREP_COHORT_CRITERIA,
  onCohortCriteriaChange = undefined as
    | ((criteria: DRepCohortCriteria) => void)
    | undefined,
  relaxedCohortCriteria = [] as DRepCohortCriterion[],
  refreshState = GovernanceRefreshState.Loaded,
  locale = 'en-US',
  onSelectForDelegation = jest.fn(),
  onViewDetails = jest.fn(),
  syncProgress = 100,
}: {
  suggestedDReps?: AppDRepDirectoryEntry[];
  allDReps?: AppDRepDirectoryEntry[];
  favoriteDRepIds?: Set<string>;
  favoriteEntries?: AppDRepDirectoryEntry[];
  listViewMode?: 'cards' | 'table';
  onListViewModeChange?: jest.Mock;
  onToggleFavorite?: jest.Mock;
  view?: 'directory' | 'favorites';
  onBackToDirectory?: jest.Mock;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  error?: { message: string; type: string; details?: string } | null;
  isNodeInSync?: boolean;
  onReroll?: jest.Mock;
  onLoadAllDReps?: jest.Mock;
  cohortCriteria?: DRepCohortCriteria;
  onCohortCriteriaChange?: (criteria: DRepCohortCriteria) => void;
  relaxedCohortCriteria?: DRepCohortCriterion[];
  refreshState?: GovernanceRefreshState;
  locale?: string;
  onSelectForDelegation?: jest.Mock;
  onViewDetails?: jest.Mock;
  syncProgress?: number | null;
} = {}) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale={locale} messages={messages}>
        <DRepDirectory
          suggestedDReps={suggestedDReps}
          allDReps={allDReps}
          favoriteDRepIds={favoriteDRepIds}
          favoriteEntries={favoriteEntries}
          listViewMode={listViewMode}
          onListViewModeChange={onListViewModeChange}
          onToggleFavorite={onToggleFavorite}
          view={view}
          onBackToDirectory={onBackToDirectory}
          isStaleFavoriteEntry={isStaleFavoriteEntry}
          error={error}
          isNodeInSync={isNodeInSync}
          onReroll={onReroll}
          onLoadAllDReps={onLoadAllDReps}
          cohortCriteria={cohortCriteria}
          onCohortCriteriaChange={onCohortCriteriaChange}
          relaxedCohortCriteria={relaxedCohortCriteria}
          lastFetchedAt={Date.now() - 60_000}
          onRefresh={jest.fn()}
          onSelectForDelegation={onSelectForDelegation}
          onViewDetails={onViewDetails}
          refreshState={refreshState}
          syncProgress={syncProgress}
        />
      </IntlProvider>
    </ThemeProvider>
  );
};

describe('DRepDirectory', () => {
  afterEach(cleanup);

  it('renders the loaded bare list with voting power and status', () => {
    renderComponent();

    // No page title: the governance tab bar already names the directory.
    expect(screen.queryByText('!!!DRep Directory')).toBeNull();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
    expect(screen.getAllByText('!!!Active')[0]).toBeInTheDocument();
  });

  it('renders the DRep name on directory cards and no source label at all', () => {
    renderComponent({
      suggestedDReps: [
        { ...baseEntries[0], verifiedName: 'Daedalus Test DRep' },
      ],
    });

    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();
    // Provenance labels belong to the detail view, where on-chain fields sit
    // beside anchor-derived ones and the distinction does work. Every field on
    // a directory card is on-chain, so a label beside one of them implied a
    // contrast that does not exist.
    expect(
      screen.queryByText('!!!Verified off-chain content')
    ).not.toBeInTheDocument();
    expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
  });

  it('renders the empty state when no DReps are available', () => {
    renderComponent({ suggestedDReps: [] });

    expect(screen.getByText('!!!No DReps found on-chain.')).toBeInTheDocument();
    expect(
      screen.getAllByRole('button', { name: '!!!Retry' })[0]
    ).toBeInTheDocument();
  });

  it('renders the blocking error state when no retained list exists', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        message: 'Cardano node socket path is not available.',
        type: 'SOCKET_UNAVAILABLE',
      },
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText('!!!Could not load DRep data.')
    ).toBeInTheDocument();
    expect(
      screen.getByText('Cardano node socket path is not available.')
    ).toBeInTheDocument();
  });

  it('renders the actionable error details in the blocking error state', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        details: 'Missing: --mainnet | --testnet-magic NATURAL',
        message: 'DRep state query failed.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText('!!!Could not load DRep data.')
    ).toBeInTheDocument();
    expect(screen.getByText('DRep state query failed.')).toBeInTheDocument();
    expect(
      screen.getByText('Missing: --mainnet | --testnet-magic NATURAL')
    ).toBeInTheDocument();
  });

  it('replaces the retained-data banner text with the snapshot-age copy', () => {
    renderComponent({
      error: {
        message:
          'Showing the last successful directory snapshot while refresh retries.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(
      screen.getByText(/Showing last successful snapshot from a minute ago/)
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Retry')).toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('keeps raw query text out of the retained-snapshot banner', () => {
    renderComponent({
      error: {
        details: 'Missing: --mainnet | --testnet-magic NATURAL',
        message:
          'Showing the last successful directory snapshot while refresh retries.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(
      screen.queryByText('Missing: --mainnet | --testnet-magic NATURAL')
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        'Showing the last successful directory snapshot while refresh retries.'
      )
    ).not.toBeInTheDocument();
    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('shows the retained-snapshot banner when the refresh times out', () => {
    renderComponent({
      error: {
        message: 'DRep registration query timed out.',
        type: 'TIMEOUT',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(screen.getByText(/Couldn't refresh DRep data/)).toBeInTheDocument();
    expect(
      screen.queryByText('DRep registration query timed out.')
    ).not.toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('offers no pagination controls over a long list', () => {
    renderComponent({ suggestedDReps: paginatedEntries });

    // The list is windowed and scrolled, the way the stake pools lists are,
    // so there are no pages to step through.
    expect(
      screen.queryByRole('button', { name: '!!!Previous' })
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: '!!!Next' })
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Page \d+ of/)).not.toBeInTheDocument();
  });

  it('renders the directory in ja-JP locale', () => {
    renderComponent({ locale: 'ja-JP' });

    expect(screen.queryByText('!!!DRepディレクトリ')).toBeNull();
    // The sort options name voting power too, so match the card's own label.
    expect(screen.getAllByText('!!!投票権:').length).toBeGreaterThan(0);
    expect(screen.getAllByText('!!!アクティブ')[0]).toBeInTheDocument();
  });

  it('renders the first-load skeleton list instead of a directory row', () => {
    const { container } = renderComponent({
      suggestedDReps: [],
      refreshState: GovernanceRefreshState.Loading,
    });

    expect(screen.getByLabelText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(container.querySelectorAll('.skeletonCard')).toHaveLength(25);
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!No DReps found on-chain.')
    ).not.toBeInTheDocument();
  });

  it('renders only the cards a windowed list needs, not all 30', () => {
    renderComponent({ suggestedDReps: paginatedEntries });

    const cards = document.querySelectorAll('[class~="card"]');
    // Some cards, but not the whole list: that is the point of windowing.
    expect(cards.length).toBeGreaterThan(0);
    expect(cards.length).toBeLessThan(paginatedEntries.length);
  });

  it('tiles the cards into a grid rather than one per row', () => {
    const { container } = renderComponent({
      suggestedDReps: paginatedEntries,
    });

    const rows = container.querySelectorAll('[class~="cardRow"]');
    expect(rows.length).toBeGreaterThan(0);
    expect(rows[0].querySelectorAll('[class~="card"]').length).toBeGreaterThan(
      1
    );
  });

  it('invokes onSelectForDelegation with the row DRep ID when the Select CTA is clicked', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({ onSelectForDelegation });

    fireEvent.click(screen.getAllByRole('button', { name: '!!!Delegate' })[0]);

    expect(onSelectForDelegation).toHaveBeenCalledTimes(1);
    expect(onSelectForDelegation).toHaveBeenCalledWith(baseEntries[0].drepId);
  });

  it('invokes onViewDetails with the row DRep ID when the View details CTA is clicked', () => {
    const onViewDetails = jest.fn();
    renderComponent({ onViewDetails });

    fireEvent.click(
      screen.getAllByRole('button', { name: '!!!View details' })[0]
    );

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(baseEntries[0].drepId);
  });

  it('renders the persistent syncing banner with the floored live sync %', () => {
    renderComponent({ isNodeInSync: false, syncProgress: 87.6 });

    expect(
      screen.getByText(
        '!!!Your node is still syncing (87%). The DRep list may be incomplete until sync completes.'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('renders 0% in the syncing banner when syncProgress is null mid-boot', () => {
    renderComponent({ isNodeInSync: false, syncProgress: null });

    expect(screen.getByText(/still syncing \(0%\)/)).toBeInTheDocument();
  });

  it('does not render the syncing banner when the node is in sync', () => {
    renderComponent();

    expect(screen.queryByText(/still syncing/)).not.toBeInTheDocument();
  });

  it('falls back to the noSync empty state when syncing yields zero DReps', () => {
    renderComponent({
      suggestedDReps: [],
      isNodeInSync: false,
      syncProgress: 42,
    });

    expect(
      screen.getByText(
        '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!No DReps found on-chain.')
    ).not.toBeInTheDocument();
  });

  it('falls back to the noSync empty state on an availability failure while syncing', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        message: 'Cardano node socket path is not available.',
        type: 'SOCKET_UNAVAILABLE',
      },
      isNodeInSync: false,
      refreshState: GovernanceRefreshState.Failed,
      syncProgress: 42,
    });

    expect(
      screen.getByText(
        /DRep data becomes available once the node reaches the tip/
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
  });

  it('keeps the retained list without the fallback when syncing with data present', () => {
    renderComponent({ isNodeInSync: false, syncProgress: 42 });

    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
    expect(
      screen.queryByText(/DRep data becomes available/)
    ).not.toBeInTheDocument();
  });

  it('renders the selfnode empty state instead of the raw query error', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText('!!!DRep data unavailable on selfnode')
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/unavailable in selfnode mode/)
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Could not load DRep data.')
    ).not.toBeInTheDocument();
  });

  it('prefers the selfnode empty state over the noSync fallback while the node is syncing', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      isNodeInSync: false,
      refreshState: GovernanceRefreshState.Failed,
      syncProgress: 42,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/DRep data becomes available once the node reaches/)
    ).not.toBeInTheDocument();
  });

  it('renders no directory row on the selfnode path even with a retained list', () => {
    renderComponent({
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(
      screen.getByText(
        '!!!DRep directory data is unavailable on the selfnode cluster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
  });

  it('renders the selfnode empty state in ja-JP', () => {
    renderComponent({
      suggestedDReps: [],
      error: {
        message:
          'DRep data is unavailable in selfnode mode. A synced node is required.',
        type: 'SELFNODE_CLI_UNSUPPORTED',
      },
      locale: 'ja-JP',
      refreshState: GovernanceRefreshState.Failed,
    });

    expect(
      screen.getByText(
        '!!!selfnodeクラスターではDRepディレクトリのデータを利用できません。'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!DRepデータ利用不可')).toBeInTheDocument();
  });

  it('shows the unavailable tooltip when voting power is null', () => {
    renderComponent({
      suggestedDReps: [{ ...baseEntries[0], votingPower: null }],
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
  });

  it('badges verified metadata but says nothing when there is none', () => {
    renderComponent();

    // A card marks what is exceptional. Almost no DRep publishes verified
    // metadata, so the badge is worth carrying; its absence is the ordinary
    // case and needs no marker of its own.
    expect(screen.queryByText('!!!No metadata')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Verified')).not.toBeInTheDocument();
  });

  it('leaves the expiry badge off a DRep that is not lapsing soon', () => {
    // baseEntries[0] has 12 epochs remaining. dRepActivity is 20, so that is
    // 60 of a DRep's 100 days and well above the six-epoch threshold.
    renderComponent();

    expect(screen.queryByText('!!!Expiring soon')).toBeNull();
  });

  it('shows the expiry badge when lapsing soon', () => {
    renderComponent({ suggestedDReps: [realEntry(1, { drepActivity: 4 })] });

    expect(screen.getByText('!!!Expiring soon')).toBeInTheDocument();
  });

  it('leaves the expiry badge off a DRep that has already lapsed', () => {
    renderComponent({
      suggestedDReps: [realEntry(1, { drepActivity: 4, status: 'inactive' })],
    });

    // Already inactive, so there is nothing left to expire; the status badge
    // carries the whole story.
    expect(screen.queryByText('!!!Expiring soon')).not.toBeInTheDocument();
    expect(screen.getByLabelText('!!!Inactive')).toBeInTheDocument();
  });

  it('shows the min-length hint below 8 post-HRP characters and leaves the list unfiltered', () => {
    renderComponent({ suggestedDReps: [realEntry(1), realEntry(2)] });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
    fireEvent.change(input, { target: { value: 'drep1abcdefg' } });

    expect(
      screen.getByText('!!!Enter at least 8 characters to search by ID')
    ).toBeInTheDocument();
    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('filters by prefix at 8 characters and never auto-selects, even on Enter with one match', () => {
    const onViewDetails = jest.fn();
    renderComponent({
      suggestedDReps: [realEntry(1), realEntry(2)],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
    const uniquePrefix = realDrepId(1).slice(0, 'drep1'.length + 20);
    fireEvent.change(input, { target: { value: uniquePrefix } });
    fireEvent.keyDown(input, { key: 'Enter', code: 'Enter' });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();
  });

  it('shows a search result as the same card as any other', () => {
    const { container } = renderComponent({
      suggestedDReps: [realEntry(1), realEntry(2)],
    });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
      }
    );

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    // One ID, in the current form: a search must not turn the card into a
    // taller shape than the grid reserves room for. DRepIdDisplay's own spec
    // covers the CIP-105 disclosure.
    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('!!!(CIP-105)')).toBeNull();
  });

  it('keeps exactly one ID form on a directory row', () => {
    const { container } = renderComponent({ suggestedDReps: [realEntry(1)] });

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy DRep ID' })
    ).toBeInTheDocument();
  });

  it('hands the CIP-129 id to delegation from a search-result row', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({
      suggestedDReps: [realEntry(1), realEntry(2)],
      onSelectForDelegation,
    });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
      }
    );
    fireEvent.click(screen.getByText('!!!Delegate'));

    expect(onSelectForDelegation).toHaveBeenCalledWith(realDrepId(1));
  });

  it('opens the detail view once for an exact CIP-129 match', () => {
    const onViewDetails = jest.fn();
    renderComponent({ suggestedDReps: [realEntry(1)], onViewDetails });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: realDrepId(1) },
      }
    );

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('canonicalizes an exact CIP-105 match to the CIP-129 detail id', () => {
    const onViewDetails = jest.fn();
    renderComponent({ suggestedDReps: [realEntry(1)], onViewDetails });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: realCip105Id(1) },
      }
    );

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('shows the invalid-ID error for a full-form string with a bad checksum and never navigates', () => {
    const onViewDetails = jest.fn();
    renderComponent({ suggestedDReps: [realEntry(1)], onViewDetails });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: `drep1${'q'.repeat(51)}` },
      }
    );

    expect(screen.getByText('!!!Invalid DRep ID')).toBeInTheDocument();
    expect(onViewDetails).not.toHaveBeenCalled();
    expect(
      screen.getAllByText(/No DReps match your filters/)[0]
    ).toBeInTheDocument();
  });

  it('reaches non-suggested entries through show-all', () => {
    const suggestedEntry = realEntry(1);
    const extraEntry1 = realEntry(2);
    const extraEntry2 = realEntry(3, { drepActivity: 3 });
    renderComponent({
      suggestedDReps: [suggestedEntry],
      allDReps: [suggestedEntry, extraEntry1, extraEntry2],
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByRole('button', { name: '!!!Show all DReps' }));

    expect(screen.getAllByText('!!!View details')).toHaveLength(3);
  });

  it('finds and opens a non-suggested entry by ID with show-all off', () => {
    const onViewDetails = jest.fn();
    const suggestedEntry = realEntry(1);
    const nonSuggestedEntry = realEntry(2);
    renderComponent({
      suggestedDReps: [suggestedEntry],
      allDReps: [suggestedEntry, nonSuggestedEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
    fireEvent.change(input, {
      target: { value: realDrepId(2).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();

    fireEvent.change(input, { target: { value: realDrepId(2) } });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(2));
  });

  it('surfaces a doNotList entry through show-all', () => {
    const suggestedEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      suggestedDReps: [suggestedEntry],
      allDReps: [suggestedEntry, optedOutEntry],
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByRole('button', { name: '!!!Show all DReps' }));

    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('opens a doNotList entry from an exact DRep ID with show-all off', () => {
    const onViewDetails = jest.fn();
    const suggestedEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      suggestedDReps: [suggestedEntry],
      allDReps: [suggestedEntry, optedOutEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
    fireEvent.change(input, {
      target: { value: realDrepId(5).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();

    fireEvent.change(input, { target: { value: realDrepId(5) } });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(5));
  });

  it('applies facet filters through the native selects', () => {
    renderComponent({
      suggestedDReps: [
        realEntry(1),
        realEntry(2, { status: 'inactive', drepActivity: 0 }),
      ],
    });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'inactive' },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('shows the sort-bias disclosure only while voting-power-descending is active', () => {
    renderComponent({ suggestedDReps: [realEntry(1)] });

    fireEvent.click(screen.getByRole('button', { name: '!!!Show all DReps' }));
    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'votingPowerDesc' },
    });

    expect(screen.getByText(/Sorted by voting power/)).toBeInTheDocument();

    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'recommended' },
    });

    expect(
      screen.queryByText(/Sorted by voting power/)
    ).not.toBeInTheDocument();
  });

  it('offers no voting-power sort when no DRep reports a figure', () => {
    const withoutPower = { votingPower: null };
    renderComponent({
      suggestedDReps: [realEntry(1, withoutPower)],
      allDReps: [realEntry(1, withoutPower), realEntry(2, withoutPower)],
    });

    const sortControl = screen.getByLabelText('!!!Sort');
    const options = within(sortControl)
      .getAllByRole('option')
      .map((option) => (option as HTMLOptionElement).value);

    expect(options).not.toContain('votingPowerDesc');
    expect(options).not.toContain('votingPowerAsc');
    expect(options).toContain('expiryAsc');
  });

  it('offers the voting-power sort when a single DRep reports a figure', () => {
    renderComponent({
      suggestedDReps: [realEntry(1, { votingPower: null })],
      allDReps: [realEntry(1, { votingPower: null }), realEntry(2)],
    });

    const options = within(screen.getByLabelText('!!!Sort'))
      .getAllByRole('option')
      .map((option) => (option as HTMLOptionElement).value);

    expect(options).toContain('votingPowerDesc');
    expect(options).toContain('votingPowerAsc');
  });

  it('leaves the banner alone when show-all only widens the list', () => {
    renderComponent({
      suggestedDReps: [realEntry(1)],
      allDReps: [realEntry(1), realEntry(2)],
    });

    fireEvent.click(screen.getByRole('button', { name: '!!!Show all DReps' }));

    // Widening the list narrows nothing and reorders nothing: with randomized
    // still selected the order is still randomized, so there is no caveat to
    // state about it.
    expect(screen.queryByText(/matching your filters/)).not.toBeInTheDocument();
  });

  it('states the filtered count once a filter actually narrows the list', () => {
    renderComponent({
      suggestedDReps: [realEntry(1), realEntry(2, { status: 'inactive' })],
    });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'active' },
    });

    expect(
      screen.getByText(/Showing 1 DReps matching your filters/)
    ).toBeInTheDocument();
  });

  it('recovers from zero results via the Clear filters action', () => {
    renderComponent({ suggestedDReps: [realEntry(1)] });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'inactive' },
    });
    expect(
      screen.getAllByText(/No DReps match your filters/)[0]
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Clear filters'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('renders the search surface in ja-JP', () => {
    renderComponent({ locale: 'ja-JP' });

    expect(screen.getByPlaceholderText('!!!DRep IDで検索')).toBeInTheDocument();
    // Two controls drive show-all, the filter-bar checkbox and the footer
    // button, and they carry the same words because they do the same thing.
    expect(screen.getAllByText('!!!すべてのDRepを表示')).toHaveLength(2);
  });

  describe('name search', () => {
    it('filters entries by verified name substring', () => {
      renderComponent({
        suggestedDReps: [
          realEntry(1, { verifiedName: 'Cardano Foundation DRep' }),
          realEntry(2, { verifiedName: 'IOHK Governance' }),
        ],
      });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        { target: { value: 'cardano' } }
      );

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    });

    it('applies the name filter case-insensitively', () => {
      renderComponent({
        suggestedDReps: [
          realEntry(1, { verifiedName: 'Alice DRep' }),
          realEntry(2, { verifiedName: 'Bob DRep' }),
        ],
      });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        { target: { value: 'ALICE' } }
      );

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    });

    it('excludes ID-only entries (no verified name) from name results', () => {
      renderComponent({
        suggestedDReps: [
          realEntry(1, { verifiedName: 'Named DRep' }),
          realEntry(2), // verifiedName: null
        ],
      });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        { target: { value: 'Named' } }
      );

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    });

    it('shows the no-results empty state for an unmatched name query', () => {
      renderComponent({
        suggestedDReps: [realEntry(1, { verifiedName: 'Known DRep' })],
      });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        { target: { value: 'UnknownName' } }
      );

      expect(
        document.querySelector('[data-variant="noResults"]')
      ).not.toBeNull();
      expect(screen.queryByText('!!!View details')).not.toBeInTheDocument();
    });

    it('does not activate name search for a single character (below minimum)', () => {
      renderComponent({
        suggestedDReps: [realEntry(1, { verifiedName: 'Alpha DRep' })],
      });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        { target: { value: 'A' } }
      );

      // 'belowMinimum' leaves the suggested list visible
      expect(screen.getByText('!!!View details')).toBeInTheDocument();
      // Sentinel cards are still shown (not in search mode)
      expect(screen.getByText('Abstain')).toBeInTheDocument();
    });
  });

  describe('reroll and sentinel cards', () => {
    it('renders the reroll button in the default (non-search, non-show-all) view', () => {
      const onReroll = jest.fn();
      renderComponent({ onReroll });

      expect(
        screen.getByText('!!!Show different suggestions')
      ).toBeInTheDocument();
    });

    it('invokes onReroll when the reroll button is clicked', () => {
      const onReroll = jest.fn();
      renderComponent({ onReroll });

      fireEvent.click(screen.getByText('!!!Show different suggestions'));

      expect(onReroll).toHaveBeenCalledTimes(1);
    });

    it('renders Abstain and No Confidence sentinel cards in the default view', () => {
      renderComponent();

      expect(screen.getByText('Abstain')).toBeInTheDocument();
      expect(screen.getByText('No Confidence')).toBeInTheDocument();
    });

    it('keeps sentinel cards available while searching', () => {
      renderComponent({ suggestedDReps: [realEntry(1)] });

      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        {
          target: { value: 'no_confidence' },
        }
      );

      // Standing options, not list members: nothing done to the list withdraws
      // them. They sit below the results under their own heading rather than
      // among them, so they are not mistaken for matches.
      expect(screen.getByText('Abstain')).toBeInTheDocument();
      expect(screen.getByText('No Confidence')).toBeInTheDocument();
    });

    it('keeps sentinel cards in show-all mode', () => {
      renderComponent({ suggestedDReps: [realEntry(1)] });

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Show all DReps' })
      );

      // Abstain and No Confidence are standing options rather than members of
      // the suggested cohort, so widening the list must not withdraw them.
      expect(screen.getByText('Abstain')).toBeInTheDocument();
      expect(screen.getByText('No Confidence')).toBeInTheDocument();
    });

    it('hides the reroll control in show-all mode', () => {
      renderComponent({ suggestedDReps: [realEntry(1)] });

      fireEvent.click(
        screen.getByRole('button', { name: '!!!Show all DReps' })
      );

      // Rerolling picks a fresh cohort, which means nothing once every DRep
      // is already listed.
      expect(
        screen.queryByText('!!!Show different suggestions')
      ).not.toBeInTheDocument();
    });

    it('routes Abstain select to onSelectForDelegation("abstain")', () => {
      const onSelectForDelegation = jest.fn();
      renderComponent({ onSelectForDelegation });

      // The sentinel card Select buttons are distinct from the DRep card CTAs
      const sentinelButtons = screen.getAllByRole('button', {
        name: '!!!Select',
      });
      fireEvent.click(sentinelButtons[0]);

      expect(onSelectForDelegation).toHaveBeenCalledWith('abstain');
    });

    it('routes No Confidence select to onSelectForDelegation("no_confidence")', () => {
      const onSelectForDelegation = jest.fn();
      renderComponent({ onSelectForDelegation });

      const sentinelButtons = screen.getAllByRole('button', {
        name: '!!!Select',
      });
      fireEvent.click(sentinelButtons[1]);

      expect(onSelectForDelegation).toHaveBeenCalledWith('no_confidence');
    });

    it('falls back to the no-results empty state without naming the sentinel in the body', () => {
      renderComponent({ suggestedDReps: [realEntry(1)] });

      // 'no_confidence' (13 chars) triggers search → no results for a DRep query
      fireEvent.change(
        screen.getByPlaceholderText('!!!Search by DRep ID or name'),
        {
          target: { value: 'no_confidence' },
        }
      );

      const emptyState = document.querySelector('[data-variant="noResults"]');
      expect(emptyState).not.toBeNull();
      expect(emptyState.textContent).not.toContain('No Confidence');
      expect(screen.queryByText('!!!View details')).not.toBeInTheDocument();
    });

    it('keeps every directory and favorites string free of the sentinel labels in both locales', () => {
      const catalogs: Record<string, string>[] = [translations, jaTranslations];
      const namespaces = [
        'governance.drepDirectory.',
        'governance.drepFavorites.',
      ];

      // The sentinel cards' own descriptions are the one place in the
      // directory that must name what Abstain and No Confidence do. Every
      // other string stays clear of the labels so a DRep search can never
      // appear to have matched one of them.
      const sentinelOwnCopy = [
        'governance.drepDirectory.abstain.description',
        'governance.drepDirectory.noConfidence.description',
      ];

      catalogs.forEach((catalog) => {
        const labels = [
          catalog['voting.governance.abstain'],
          catalog['voting.governance.noConfidence'],
        ];
        const conflicting = Object.keys(catalog)
          .filter((key) => namespaces.some((ns) => key.startsWith(ns)))
          .filter((key) => !sentinelOwnCopy.includes(key))
          .filter((key) =>
            labels.some((label) => catalog[key].includes(label))
          );
        expect(conflicting).toEqual([]);
      });
    });

    it('routes no sentinel literal into a logger sink while searching', () => {
      const SENTINEL_QUERIES = ['no_confidence'];
      const debugSpy = jest
        .spyOn(logger, 'debug')
        .mockImplementation(() => undefined);
      const infoSpy = jest
        .spyOn(logger, 'info')
        .mockImplementation(() => undefined);
      const warnSpy = jest
        .spyOn(logger, 'warn')
        .mockImplementation(() => undefined);
      const errorSpy = jest
        .spyOn(logger, 'error')
        .mockImplementation(() => undefined);

      renderComponent({ suggestedDReps: [realEntry(1)] });

      const input = screen.getByPlaceholderText('!!!Search by DRep ID or name');
      SENTINEL_QUERIES.forEach((query) => {
        fireEvent.change(input, { target: { value: query } });
      });

      const logged = JSON.stringify([
        debugSpy.mock.calls,
        infoSpy.mock.calls,
        warnSpy.mock.calls,
        errorSpy.mock.calls,
      ]);
      SENTINEL_QUERIES.forEach((query) => {
        expect(logged).not.toContain(query);
      });

      jest.restoreAllMocks();
    });
  });

  describe('favorites', () => {
    // Mirrors DRepIdDisplay's first8…last6 truncation; the exact truncated
    // string matches only the visible <code>, never the hidden tooltip copy.
    const truncatedDrepId = (n: number): string => {
      const id = realDrepId(n);
      return `${id.slice(0, 8)}…${id.slice(-6)}`;
    };

    it('renders the favorite toggle unpressed and fires onToggleFavorite with the row id', () => {
      const onToggleFavorite = jest.fn();
      renderComponent({ suggestedDReps: [realEntry(1)], onToggleFavorite });

      const toggle = screen.getByRole('button', { name: /Add to favorites/ });
      expect(toggle).toHaveAttribute('aria-pressed', 'false');
      fireEvent.click(toggle);
      expect(onToggleFavorite).toHaveBeenCalledTimes(1);
      expect(onToggleFavorite).toHaveBeenCalledWith(realDrepId(1));
    });

    it('shows the pressed state and remove label for favorited rows', () => {
      renderComponent({
        suggestedDReps: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
      });

      const toggle = screen.getByRole('button', {
        name: /Remove from favorites/,
      });
      expect(toggle).toHaveAttribute('aria-pressed', 'true');
    });

    it('renders favorited entries outside the suggested list in the favorites view', () => {
      renderComponent({
        suggestedDReps: [realEntry(1)],
        allDReps: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(2)]),
        view: 'favorites',
      });

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
      expect(screen.queryByText(truncatedDrepId(1))).not.toBeInTheDocument();
      expect(screen.getByText(/DReps you've favorited/)).toBeInTheDocument();
    });

    it('offers search and the view toggle but not show-all in favorites', () => {
      renderComponent({
        suggestedDReps: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
        view: 'favorites',
      });

      // A favourites list can grow long enough to need finding and
      // rearranging. Widening it to every DRep is what the directory tab is
      // for, so show-all stays out.
      expect(
        screen.getByPlaceholderText(/Search by DRep ID/)
      ).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: '!!!Table view' })
      ).toBeInTheDocument();
      expect(screen.queryByText(/Show all DReps/)).not.toBeInTheDocument();
    });

    it('searches within the favorites rather than the whole directory', () => {
      renderComponent({
        suggestedDReps: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(1)]),
        view: 'favorites',
      });

      fireEvent.change(screen.getByPlaceholderText(/Search by DRep ID/), {
        target: { value: realDrepId(2).slice(0, 'drep1'.length + 20) },
      });

      // A favourite that does not match yields no results rather than pulling
      // the non-favourite it matches back into the list.
      expect(screen.queryByText('!!!View details')).not.toBeInTheDocument();
    });

    it('shows the noFavorites empty state with a working back-to-directory action', () => {
      const onBackToDirectory = jest.fn();
      renderComponent({
        suggestedDReps: [realEntry(1)],
        view: 'favorites',
        onBackToDirectory,
      });

      expect(screen.getByText(/No favorites yet/)).toBeInTheDocument();
      expect(
        screen.getByText(
          /appear here\. Favorites are stored on this device only\./
        )
      ).toBeInTheDocument();
      fireEvent.click(screen.getByText(/Back to directory/));
      expect(onBackToDirectory).toHaveBeenCalledTimes(1);
    });

    it('renders the stale caption only for entries the injected predicate marks stale', () => {
      renderComponent({
        suggestedDReps: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
        view: 'favorites',
        isStaleFavoriteEntry: (entry: AppDRepDirectoryEntry) =>
          entry.drepId === realDrepId(2),
      });

      expect(
        screen.getAllByText(/not actively accepting delegation/)
      ).toHaveLength(1);
    });

    it('never renders the stale caption in the directory view', () => {
      renderComponent({
        suggestedDReps: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
        isStaleFavoriteEntry: () => true,
      });

      expect(
        screen.queryByText(/not actively accepting delegation/)
      ).not.toBeInTheDocument();
    });

    it('captions a doNotList favorite through the real predicate and keeps its status badge', () => {
      renderComponent({
        suggestedDReps: [realEntry(1), realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
        view: 'favorites',
      });

      expect(
        screen.getAllByText(/not actively accepting delegation/)
      ).toHaveLength(1);
      // Active is the norm and carries no badge, so neither card shows one.
      expect(screen.queryByLabelText('!!!Active')).not.toBeInTheDocument();
      expect(screen.getAllByText('!!!View details')).toHaveLength(2);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
    });

    it('renders no caption for a doNotList favorite in the directory view', () => {
      renderComponent({
        suggestedDReps: [realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(2)]),
      });

      expect(
        screen.queryByText(/not actively accepting delegation/)
      ).not.toBeInTheDocument();
      expect(screen.getByText('!!!View details')).toBeInTheDocument();
    });

    it('renders the favorites empty-state copy in ja-JP', () => {
      renderComponent({
        suggestedDReps: [realEntry(1)],
        view: 'favorites',
        locale: 'ja-JP',
      });

      expect(
        screen.getByText(/お気に入りはまだありません/)
      ).toBeInTheDocument();
    });
  });
});

describe('DRepDirectory pinned favorites', () => {
  const favorite = realEntry(7, { verifiedName: 'Pinned DRep' });

  it('pins favorites above the cohort without any mode switch', () => {
    // Review item 2: the cohort is a random twenty, so a favorite was usually
    // absent from it, and reaching one meant toggling Show All as well.
    renderComponent({
      favoriteEntries: [favorite],
      favoriteDRepIds: new Set([favorite.drepId]),
    });

    expect(screen.getByText('!!!Your favorites (1)')).toBeInTheDocument();
    expect(screen.getByText('Pinned DRep')).toBeInTheDocument();
  });

  it('renders no favorites group when there are none', () => {
    renderComponent();

    expect(screen.queryByText(/!!!Your favorites/)).toBeNull();
  });

  it('does not show a favorite twice when it is already in the cohort', () => {
    const inCohort = realEntry(1, { verifiedName: 'Already Listed' });
    renderComponent({
      suggestedDReps: [inCohort],
      favoriteEntries: [inCohort],
      favoriteDRepIds: new Set([inCohort.drepId]),
    });

    // Pinned, and removed from the cohort beneath, so it appears exactly once
    // and always in the same place.
    expect(screen.getByText(/!!!Your favorites/)).toBeInTheDocument();
    expect(screen.getAllByText('Already Listed')).toHaveLength(1);
  });

  it('leaves the group out of the favorites view, which is already all favorites', () => {
    renderComponent({
      view: 'favorites',
      favoriteEntries: [favorite],
      favoriteDRepIds: new Set([favorite.drepId]),
    });

    expect(screen.queryByText(/!!!Your favorites \(/)).toBeNull();
  });

  it('leaves the group out while searching', () => {
    renderComponent({
      favoriteEntries: [favorite],
      favoriteDRepIds: new Set([favorite.drepId]),
    });

    fireEvent.change(
      screen.getByPlaceholderText('!!!Search by DRep ID or name'),
      {
        target: { value: 'drep1abcdefgh' },
      }
    );

    expect(screen.queryByText(/!!!Your favorites \(/)).toBeNull();
  });
});

describe('DRepDirectory view modes', () => {
  it('shows cards by default, matching the stake pools default', () => {
    renderComponent();

    expect(screen.queryByRole('table')).toBeNull();
    expect(
      screen.getByLabelText('!!!Card view').getAttribute('aria-pressed')
    ).toBe('true');
  });

  it('switches to a table and back', () => {
    renderComponent();

    fireEvent.click(screen.getByLabelText('!!!Table view'));
    expect(screen.getByRole('table')).toBeInTheDocument();
    expect(
      screen.getByLabelText('!!!Table view').getAttribute('aria-pressed')
    ).toBe('true');

    fireEvent.click(screen.getByLabelText('!!!Card view'));
    expect(screen.queryByRole('table')).toBeNull();
  });

  it('carries no metadata column, the name column having already said it', () => {
    renderComponent();
    fireEvent.click(screen.getByLabelText('!!!Table view'));

    // The only thing the app can determine about a DRep's metadata is whether
    // a verified name came back, and the name column shows that name. A tick
    // beside it restated one field in terms of another.
    expect(
      screen.queryByRole('columnheader', { name: '!!!Metadata' })
    ).not.toBeInTheDocument();
    expect(
      screen.queryByLabelText(/!!!(No verified metadata|Verified metadata)/)
    ).not.toBeInTheDocument();
  });

  it('keeps every binding signal in the table view', () => {
    renderComponent({ suggestedDReps: [realEntry(1, { drepActivity: 3 })] });
    fireEvent.click(screen.getByLabelText('!!!Table view'));

    expect(screen.getAllByText('!!!Active')[0]).toBeInTheDocument();
    expect(screen.getByText('!!!Expiring soon')).toBeInTheDocument();
    expect(
      screen.getByRole('columnheader', { name: '!!!Voting power' })
    ).toBeInTheDocument();
  });
});

describe('DRepDirectory stored view preference', () => {
  it('opens in the stored view rather than the default', () => {
    renderComponent({ listViewMode: 'table' });

    expect(screen.getByRole('table')).toBeInTheDocument();
  });

  it('reports a change so it can be persisted', () => {
    const onListViewModeChange = jest.fn();
    renderComponent({ onListViewModeChange });

    fireEvent.click(screen.getByLabelText('!!!Table view'));

    expect(onListViewModeChange).toHaveBeenCalledWith('table');
  });

  it('still toggles without anything storing the choice', () => {
    // The component has to work on its own, in Storybook and in tests, not
    // only with a store wired behind it.
    renderComponent();

    fireEvent.click(screen.getByLabelText('!!!Table view'));
    expect(screen.getByRole('table')).toBeInTheDocument();
  });
});

describe('DRepDirectory suggestion criteria', () => {
  afterEach(cleanup);

  it('loads the full list on open, not only when a control widens the view', () => {
    // The cohort is selected here, from that list. Waiting for show-all or a
    // search would mean the directory opens on nothing to select from.
    const onLoadAllDReps = jest.fn();
    renderComponent({ onLoadAllDReps });

    expect(onLoadAllDReps).toHaveBeenCalled();
  });

  it('does not offer the criteria panel when nothing can act on a change', () => {
    renderComponent();

    expect(
      screen.queryByText('!!!Suggestion criteria')
    ).not.toBeInTheDocument();
  });

  it('reveals the criteria behind a disclosure rather than in the filter strip', () => {
    renderComponent({ onCohortCriteriaChange: jest.fn() });

    expect(screen.queryByText(/drawn at random/)).not.toBeInTheDocument();
    fireEvent.click(screen.getByText('!!!Suggestion criteria'));

    expect(screen.getByText(/drawn at random/)).toBeInTheDocument();
    expect(screen.getByLabelText('!!!Suggestions shown')).toHaveValue('20');
    expect(screen.getByLabelText('!!!Voting power under')).toHaveValue('0.015');
  });

  it('reports a criterion the user turns off', () => {
    const onCohortCriteriaChange = jest.fn();
    renderComponent({ onCohortCriteriaChange });
    fireEvent.click(screen.getByText('!!!Suggestion criteria'));

    fireEvent.click(screen.getByText('!!!Verified metadata'));

    expect(onCohortCriteriaChange).toHaveBeenCalledWith({
      ...DEFAULT_DREP_COHORT_CRITERIA,
      requireVerifiedMetadata: false,
    });
  });

  it('reports a changed cohort size as a number, not the select string', () => {
    const onCohortCriteriaChange = jest.fn();
    renderComponent({ onCohortCriteriaChange });
    fireEvent.click(screen.getByText('!!!Suggestion criteria'));

    fireEvent.change(screen.getByLabelText('!!!Suggestions shown'), {
      target: { value: '50' },
    });

    expect(onCohortCriteriaChange).toHaveBeenCalledWith({
      ...DEFAULT_DREP_COHORT_CRITERIA,
      size: 50,
    });
  });

  it('reports removing the voting power ceiling as no ceiling at all', () => {
    const onCohortCriteriaChange = jest.fn();
    renderComponent({ onCohortCriteriaChange });
    fireEvent.click(screen.getByText('!!!Suggestion criteria'));

    fireEvent.change(screen.getByLabelText('!!!Voting power under'), {
      target: { value: 'none' },
    });

    expect(onCohortCriteriaChange).toHaveBeenCalledWith({
      ...DEFAULT_DREP_COHORT_CRITERIA,
      maxVotingPowerShare: null,
    });
  });

  it('shows the criteria the panel was given rather than the defaults', () => {
    renderComponent({
      onCohortCriteriaChange: jest.fn(),
      cohortCriteria: {
        ...DEFAULT_DREP_COHORT_CRITERIA,
        size: 10,
        maxVotingPowerShare: null,
      },
    });
    fireEvent.click(screen.getByText('!!!Suggestion criteria'));

    expect(screen.getByLabelText('!!!Suggestions shown')).toHaveValue('10');
    expect(screen.getByLabelText('!!!Voting power under')).toHaveValue('none');
  });

  it('says which criteria were relaxed to fill the cohort', () => {
    renderComponent({
      relaxedCohortCriteria: ['verifiedMetadata', 'votingPowerShare'],
    });

    expect(
      screen.getByText(/verified metadata, !!!voting power limit/)
    ).toBeInTheDocument();
  });

  it('says nothing when every criterion held', () => {
    renderComponent({ relaxedCohortCriteria: [] });

    expect(screen.queryByText(/Too few DReps met/)).not.toBeInTheDocument();
  });

  it('drops both the criteria and the relaxation notice outside the cohort view', () => {
    // Neither belongs to a list of every DRep or to a set of search results:
    // nothing is being suggested there, so there is nothing to state.
    renderComponent({
      onCohortCriteriaChange: jest.fn(),
      relaxedCohortCriteria: ['verifiedMetadata'],
      suggestedDReps: [realEntry(1)],
    });
    expect(screen.getByText('!!!Suggestion criteria')).toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: '!!!Show all DReps' }));

    expect(
      screen.queryByText('!!!Suggestion criteria')
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Too few DReps met/)).not.toBeInTheDocument();
  });
});
