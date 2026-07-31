import React from 'react';
import BigNumber from 'bignumber.js';
import { Cardano } from '@cardano-sdk/core';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import DRepDirectory from './DRepDirectory';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
  AppDRepDirectoryEntry,
  DRepCohortContext,
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
  drepList = baseEntries,
  showAllList = drepList,
  drepIndex = new Map(showAllList.map((e) => [e.drepId, e])),
  top35DRepIds = new Set<string>(),
  favoriteDRepIds = new Set<string>(),
  onToggleFavorite = jest.fn(),
  view = 'directory' as const,
  onBackToDirectory = jest.fn(),
  isStaleFavoriteEntry = undefined as
    | ((entry: AppDRepDirectoryEntry) => boolean)
    | undefined,
  error = null,
  isNodeInSync = true,
  isCohortActive = false,
  cohort = {
    medianVotingPower: null,
    memberIds: null,
    verifiedMetadataIds: new Set<string>(),
  } as DRepCohortContext,
  onReshuffle = jest.fn(),
  refreshState = GovernanceRefreshState.Loaded,
  locale = 'en-US',
  onSelectForDelegation = jest.fn(),
  onViewDetails = jest.fn(),
  syncProgress = 100,
  votingPowerState = VotingPowerEnrichState.Loaded,
}: {
  drepList?: AppDRepDirectoryEntry[];
  showAllList?: AppDRepDirectoryEntry[];
  drepIndex?: Map<string, AppDRepDirectoryEntry>;
  top35DRepIds?: Set<string>;
  favoriteDRepIds?: Set<string>;
  onToggleFavorite?: jest.Mock;
  view?: 'directory' | 'favorites';
  onBackToDirectory?: jest.Mock;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  error?: { message: string; type: string; details?: string } | null;
  isNodeInSync?: boolean;
  isCohortActive?: boolean;
  cohort?: DRepCohortContext;
  onReshuffle?: jest.Mock;
  refreshState?: GovernanceRefreshState;
  locale?: string;
  onSelectForDelegation?: jest.Mock;
  onViewDetails?: jest.Mock;
  syncProgress?: number | null;
  votingPowerState?: VotingPowerEnrichState;
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
          drepList={drepList}
          showAllList={showAllList}
          drepIndex={drepIndex}
          top35DRepIds={top35DRepIds}
          favoriteDRepIds={favoriteDRepIds}
          onToggleFavorite={onToggleFavorite}
          view={view}
          onBackToDirectory={onBackToDirectory}
          isStaleFavoriteEntry={isStaleFavoriteEntry}
          error={error}
          isNodeInSync={isNodeInSync}
          isCohortActive={isCohortActive}
          cohort={cohort}
          onReshuffle={onReshuffle}
          lastFetchedAt={Date.now() - 60_000}
          onRefresh={jest.fn()}
          onSelectForDelegation={onSelectForDelegation}
          onViewDetails={onViewDetails}
          refreshState={refreshState}
          syncProgress={syncProgress}
          votingPowerState={votingPowerState}
        />
      </IntlProvider>
    </ThemeProvider>
  );
};

describe('DRepDirectory', () => {
  afterEach(cleanup);

  it('renders the loaded bare list with voting power and status', () => {
    renderComponent();

    expect(screen.getByText('!!!DRep Directory')).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
    expect(screen.getAllByText('!!!Active')[0]).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
  });

  it('renders no verified off-chain content on directory cards', () => {
    renderComponent({
      drepList: [{ ...baseEntries[0], verifiedName: 'Daedalus Test DRep' }],
    });

    expect(screen.queryByText('Daedalus Test DRep')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Verified off-chain content')
    ).not.toBeInTheDocument();
    expect(screen.getAllByText('!!!On-chain').length).toBeGreaterThan(0);
  });

  it('renders the empty state when no DReps are available', () => {
    renderComponent({ drepList: [] });

    expect(screen.getByText('!!!No DReps found on-chain.')).toBeInTheDocument();
    expect(
      screen.getAllByRole('button', { name: '!!!Retry' })[0]
    ).toBeInTheDocument();
  });

  it('renders the blocking error state when no retained list exists', () => {
    renderComponent({
      drepList: [],
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
      drepList: [],
      error: {
        details: 'Missing: --mainnet | --testnet-magic NATURAL',
        message: 'DRep state query failed.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Failed,
    });

    // The generic message and the normalized error.message both render.
    expect(
      screen.getByText('!!!Could not load DRep data.')
    ).toBeInTheDocument();
    expect(screen.getByText('DRep state query failed.')).toBeInTheDocument();
    // The actionable CLI stderr (error.details) is surfaced to the user.
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

  it('renders pagination controls with 30 entries (page 1 of 2)', () => {
    renderComponent({ drepList: paginatedEntries });

    // Page info shows page 1 of 2
    expect(screen.getByText('!!!Page 1 of 2')).toBeInTheDocument();
    // Previous button is disabled on page 1
    const prevButton = screen.getByRole('button', { name: '!!!Previous' });
    expect(prevButton).toBeDisabled();
    // Next button is enabled
    const nextButton = screen.getByRole('button', { name: '!!!Next' });
    expect(nextButton).not.toBeDisabled();
  });

  it('renders the directory in ja-JP locale', () => {
    renderComponent({ locale: 'ja-JP' });

    // Title and labels render in Japanese (text may have trailing colons)
    expect(screen.getByText('!!!DRepディレクトリ')).toBeInTheDocument();
    expect(screen.getByText(/投票権/)).toBeInTheDocument();
    expect(screen.getAllByText('!!!アクティブ')[0]).toBeInTheDocument();
    expect(screen.getByText('!!!オンチェーン')).toBeInTheDocument();
  });

  it('renders the first-load skeleton list instead of a directory row', () => {
    const { container } = renderComponent({
      drepList: [],
      refreshState: GovernanceRefreshState.Loading,
    });

    expect(screen.getByLabelText('!!!Loading DRep data…')).toBeInTheDocument();
    expect(container.querySelectorAll('.skeletonCard')).toHaveLength(25);
    expect(screen.queryByText('!!!Voting power:')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!No DReps found on-chain.')
    ).not.toBeInTheDocument();
  });

  it('navigates to page 2 and shows disabled Next when on last page', () => {
    renderComponent({ drepList: paginatedEntries });

    // Click Next to navigate to page 2
    const nextButton = screen.getByRole('button', { name: '!!!Next' });
    nextButton.click();

    // Page info shows page 2 of 2
    expect(screen.getByText('!!!Page 2 of 2')).toBeInTheDocument();

    // Next button is now disabled on the last page
    expect(screen.getByRole('button', { name: '!!!Next' })).toBeDisabled();

    // Previous button is now enabled
    expect(
      screen.getByRole('button', { name: '!!!Previous' })
    ).not.toBeDisabled();
  });

  it('displays 25 entries per page when paginated', () => {
    renderComponent({ drepList: paginatedEntries });

    // With 30 entries and pageSize=25, page 1 shows 25 cards
    const cards = document.querySelectorAll('[class*="card"]');
    expect(cards.length).toBe(25);
  });

  it('invokes onSelectForDelegation with the row DRep ID when the Select CTA is clicked', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({ onSelectForDelegation });

    fireEvent.click(
      screen.getAllByRole('button', { name: '!!!Select for delegation' })[0]
    );

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
    // The soft warning never hides the data underneath it.
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
    renderComponent({ drepList: [], isNodeInSync: false, syncProgress: 42 });

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
      drepList: [],
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
      drepList: [],
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
      drepList: [],
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
      drepList: [],
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

  it('drives the — tooltip by enrich state and shows the rankingUnavailable banner on stake failure', () => {
    const { unmount } = renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      votingPowerState: VotingPowerEnrichState.Loading,
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Loading voting power…'
    );
    expect(
      screen.queryByText(/Voting power data unavailable/)
    ).not.toBeInTheDocument();
    unmount();

    renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      votingPowerState: VotingPowerEnrichState.Failed,
    });

    expect(screen.getByText('—')).toHaveAttribute(
      'title',
      '!!!Stake distribution unavailable this refresh.'
    );
    expect(
      screen.getByText(
        '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.'
      )
    ).toBeInTheDocument();
  });

  it('renders the cohort banner line and Reshuffle control when the cohort is active', () => {
    renderComponent({ isCohortActive: true });

    expect(
      screen.getByText(
        '!!!Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power.'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Reshuffle order')).toBeInTheDocument();
  });

  it('invokes onReshuffle when the Reshuffle control is clicked', () => {
    const onReshuffle = jest.fn();
    renderComponent({ isCohortActive: true, onReshuffle });

    fireEvent.click(screen.getByText('!!!Reshuffle order'));

    expect(onReshuffle).toHaveBeenCalledTimes(1);
  });

  it('makes no cohort claim while ranking is unavailable', () => {
    renderComponent({
      drepList: [{ ...baseEntries[0], votingPower: null }],
      isCohortActive: false,
      votingPowerState: VotingPowerEnrichState.Failed,
    });

    expect(screen.queryByText(/Default view shows/)).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Reshuffle order')).not.toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.'
      )
    ).toBeInTheDocument();
  });

  it('renders the cohort banner in ja-JP', () => {
    renderComponent({ isCohortActive: true, locale: 'ja-JP' });

    expect(screen.getByText(/最大200の適格なDRep/)).toBeInTheDocument();
    expect(screen.getByText('!!!順序をシャッフル')).toBeInTheDocument();
  });

  it('renders exactly one category badge per card (snapshot)', () => {
    renderComponent();

    // baseEntries[0]: drepActivity 12, anchor null -> Threshold window edge.
    expect(
      screen.getAllByText(/^!!!(High value|Primary|Threshold|Non-metadata)$/)
    ).toHaveLength(1);
    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toMatchSnapshot();
  });

  it('renders the high value badge for an in-cohort verified entry above the median', () => {
    renderComponent({
      cohort: {
        medianVotingPower: new BigNumber('1000000'),
        memberIds: new Set([baseEntries[0].drepId]),
        verifiedMetadataIds: new Set([baseEntries[0].drepId]),
      },
      drepList: [{ ...baseEntries[0], drepActivity: 20 }],
    });

    expect(screen.getByText('!!!High value')).toBeInTheDocument();
  });

  it('shows the min-length hint below 8 post-HRP characters and leaves the list unfiltered', () => {
    renderComponent({ drepList: [realEntry(1), realEntry(2)] });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    fireEvent.change(input, { target: { value: 'drep1abcdefg' } });

    expect(
      screen.getByText('!!!Enter at least 8 characters to search by ID')
    ).toBeInTheDocument();
    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('filters by prefix at 8 characters and never auto-selects, even on Enter with one match', () => {
    const onViewDetails = jest.fn();
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
    const uniquePrefix = realDrepId(1).slice(0, 'drep1'.length + 20);
    fireEvent.change(input, { target: { value: uniquePrefix } });
    fireEvent.keyDown(input, { key: 'Enter', code: 'Enter' });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(onViewDetails).not.toHaveBeenCalled();
  });

  it('stacks both ID forms on a search-result row', () => {
    const { container } = renderComponent({
      drepList: [realEntry(1), realEntry(2)],
    });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
    expect(container.querySelectorAll('code')).toHaveLength(2);
    expect(screen.getByText('!!!(CIP-105)')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy CIP-105 DRep ID' })
    ).toBeInTheDocument();
  });

  it('keeps exactly one ID form on a cohort row', () => {
    const { container } = renderComponent({ drepList: [realEntry(1)] });

    expect(container.querySelectorAll('code')).toHaveLength(1);
    expect(screen.queryByText('!!!(CIP-105)')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: '!!!Copy DRep ID' })
    ).toBeInTheDocument();
  });

  it('hands the CIP-129 id to delegation from a search-result row', () => {
    const onSelectForDelegation = jest.fn();
    renderComponent({
      drepList: [realEntry(1), realEntry(2)],
      onSelectForDelegation,
    });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1).slice(0, 'drep1'.length + 20) },
    });
    fireEvent.click(screen.getByText('!!!Select for delegation'));

    expect(onSelectForDelegation).toHaveBeenCalledWith(realDrepId(1));
  });

  it('opens the detail view once for an exact CIP-129 match', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realDrepId(1) },
    });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('canonicalizes an exact CIP-105 match to the CIP-129 detail id', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: realCip105Id(1) },
    });

    expect(onViewDetails).toHaveBeenCalledTimes(1);
    expect(onViewDetails).toHaveBeenCalledWith(realDrepId(1));
  });

  it('shows the invalid-ID error for a full-form string with a bad checksum and never navigates', () => {
    const onViewDetails = jest.fn();
    renderComponent({ drepList: [realEntry(1)], onViewDetails });

    fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
      target: { value: `drep1${'q'.repeat(51)}` },
    });

    expect(screen.getByText('!!!Invalid DRep ID')).toBeInTheDocument();
    expect(onViewDetails).not.toHaveBeenCalled();
    // FormattedMessage splits the noResults copy across nested nodes; take
    // the first match instead of requiring a single element.
    expect(
      screen.getAllByText(/No DReps match your filters/)[0]
    ).toBeInTheDocument();
  });

  it('reaches top-35 and non-cohort entries through show-all', () => {
    // drepList (the cohort) holds only entry 1; the full list adds a top-35
    // and a sub-floor entry that must surface when show-all is on.
    const cohortEntry = realEntry(1);
    const top35Entry = realEntry(2);
    const subFloorEntry = realEntry(3, { drepActivity: 3 });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, top35Entry, subFloorEntry],
      top35DRepIds: new Set([top35Entry.drepId]),
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(3);
  });

  it('finds and opens a non-cohort entry by ID with show-all off', () => {
    // Entry 2 exists only in showAllList (and, via the harness default, in
    // drepIndex) - never in the cohort. Search must run over the full
    // membership and exact-match lookup over the index, or non-cohort DReps
    // are unreachable without the show-all toggle.
    const onViewDetails = jest.fn();
    const cohortEntry = realEntry(1);
    const nonCohortEntry = realEntry(2);
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, nonCohortEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
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
    // The store drops the opted-out entry from the cohort but never from
    // showAllList, so the escape hatch must still reach it.
    const cohortEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, optedOutEntry],
    });

    expect(screen.getAllByText('!!!View details')).toHaveLength(1);

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(screen.getAllByText('!!!View details')).toHaveLength(2);
  });

  it('opens a doNotList entry from an exact DRep ID with show-all off', () => {
    const onViewDetails = jest.fn();
    const cohortEntry = realEntry(4);
    const optedOutEntry = realEntry(5, { doNotList: true });
    renderComponent({
      drepList: [cohortEntry],
      showAllList: [cohortEntry, optedOutEntry],
      onViewDetails,
    });

    const input = screen.getByPlaceholderText('!!!Search by DRep ID');
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
      drepList: [
        realEntry(1),
        realEntry(2, { status: 'inactive', drepActivity: 0 }),
      ],
    });

    fireEvent.change(screen.getByLabelText('!!!Status'), {
      target: { value: 'inactive' },
    });

    // Card count is the unambiguous signal (the select option shares the
    // '!!!Inactive' label with the badge).
    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('excludes the top-35 under show-all via the exclusion toggle', () => {
    const top35Entry = realEntry(1);
    const rest = realEntry(2);
    renderComponent({
      drepList: [rest],
      showAllList: [top35Entry, rest],
      top35DRepIds: new Set([top35Entry.drepId]),
    });

    fireEvent.click(screen.getByText('!!!Show all DReps'));
    expect(screen.getAllByText('!!!View details')).toHaveLength(2);

    fireEvent.click(screen.getByText('!!!Exclude the 35 largest'));
    expect(screen.getAllByText('!!!View details')).toHaveLength(1);
  });

  it('shows the sort-bias disclosure only while voting-power-descending is active', () => {
    renderComponent({ drepList: [realEntry(1)] });

    fireEvent.click(screen.getByText('!!!Show all DReps'));
    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'votingPowerDesc' },
    });

    expect(screen.getByText(/Sorted by voting power/)).toBeInTheDocument();

    fireEvent.change(screen.getByLabelText('!!!Sort'), {
      target: { value: 'randomized' },
    });

    expect(
      screen.queryByText(/Sorted by voting power/)
    ).not.toBeInTheDocument();
  });

  it('switches the banner to the filtered line with a live count under show-all', () => {
    renderComponent({
      drepList: [realEntry(1)],
      showAllList: [realEntry(1), realEntry(2)],
      isCohortActive: true,
    });

    expect(screen.getByText(/Default view shows/)).toBeInTheDocument();

    fireEvent.click(screen.getByText('!!!Show all DReps'));

    expect(
      screen.getByText(/Showing 2 DReps matching your filters/)
    ).toBeInTheDocument();
    expect(screen.queryByText(/Default view shows/)).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Reshuffle order')).not.toBeInTheDocument();
  });

  it('recovers from zero results via the Clear filters action', () => {
    renderComponent({ drepList: [realEntry(1)] });

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
    expect(screen.getByText('!!!すべてのDRepを表示')).toBeInTheDocument();
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
      renderComponent({ drepList: [realEntry(1)], onToggleFavorite });

      const toggle = screen.getByRole('button', { name: /Add to favorites/ });
      expect(toggle).toHaveAttribute('aria-pressed', 'false');
      fireEvent.click(toggle);
      expect(onToggleFavorite).toHaveBeenCalledTimes(1);
      expect(onToggleFavorite).toHaveBeenCalledWith(realDrepId(1));
    });

    it('shows the pressed state and remove label for favorited rows', () => {
      renderComponent({
        drepList: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
      });

      const toggle = screen.getByRole('button', {
        name: /Remove from favorites/,
      });
      expect(toggle).toHaveAttribute('aria-pressed', 'true');
    });

    it('drives the favoritedOnly facet from the Favorited checkbox via the framework predicate', () => {
      renderComponent({
        drepList: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(2)]),
      });

      expect(screen.getAllByText('!!!View details')).toHaveLength(2);
      fireEvent.click(screen.getByText(/Favorited/));

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
      expect(screen.queryByText(truncatedDrepId(1))).not.toBeInTheDocument();
    });

    it('renders favorited entries outside the cohort in the favorites view', () => {
      // Entry 2 is favorited but absent from the cohort list; the favorites
      // view draws from the full membership, so it must still render.
      renderComponent({
        drepList: [realEntry(1)],
        showAllList: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(2)]),
        view: 'favorites',
      });

      expect(screen.getAllByText('!!!View details')).toHaveLength(1);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
      expect(screen.queryByText(truncatedDrepId(1))).not.toBeInTheDocument();
      expect(screen.getByText(/DReps you've favorited/)).toBeInTheDocument();
    });

    it('hides search and filter controls in the favorites view', () => {
      renderComponent({
        drepList: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
        view: 'favorites',
      });

      expect(
        screen.queryByPlaceholderText(/Search by DRep ID/)
      ).not.toBeInTheDocument();
      expect(screen.queryByText(/Show all DReps/)).not.toBeInTheDocument();
    });

    it('shows the noFavorites empty state with a working back-to-directory action', () => {
      const onBackToDirectory = jest.fn();
      renderComponent({
        drepList: [realEntry(1)],
        view: 'favorites',
        onBackToDirectory,
      });

      expect(screen.getByText(/No favorites yet/)).toBeInTheDocument();
      // The banner's favorites line repeats the per-device sentence, so the
      // body is matched through its unique leading phrase.
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
        drepList: [realEntry(1), realEntry(2)],
        favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
        view: 'favorites',
        isStaleFavoriteEntry: (entry: AppDRepDirectoryEntry) =>
          entry.drepId === realDrepId(2),
      });

      expect(
        screen.getAllByText(/no longer in the default cohort/)
      ).toHaveLength(1);
    });

    it('never renders the stale caption in the directory view', () => {
      renderComponent({
        drepList: [realEntry(1)],
        favoriteDRepIds: new Set([realDrepId(1)]),
        isStaleFavoriteEntry: () => true,
      });

      expect(
        screen.queryByText(/no longer in the default cohort/)
      ).not.toBeInTheDocument();
    });

    it('captions a doNotList favorite through the real predicate and keeps its status badge', () => {
      renderComponent({
        drepList: [realEntry(1), realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(1), realDrepId(2)]),
        view: 'favorites',
      });

      expect(
        screen.getAllByText(/no longer in the default cohort/)
      ).toHaveLength(1);
      expect(screen.getAllByLabelText('!!!Active')).toHaveLength(2);
      expect(screen.getAllByText('!!!View details')).toHaveLength(2);
      expect(screen.getByText(truncatedDrepId(2))).toBeInTheDocument();
    });

    it('renders no caption for a doNotList favorite in the directory view', () => {
      renderComponent({
        drepList: [realEntry(2, { doNotList: true })],
        favoriteDRepIds: new Set([realDrepId(2)]),
      });

      expect(
        screen.queryByText(/no longer in the default cohort/)
      ).not.toBeInTheDocument();
      expect(screen.getByText('!!!View details')).toBeInTheDocument();
    });

    it('renders the favorites empty-state copy in ja-JP', () => {
      renderComponent({
        drepList: [realEntry(1)],
        view: 'favorites',
        locale: 'ja-JP',
      });

      expect(
        screen.getByText(/お気に入りはまだありません/)
      ).toBeInTheDocument();
    });
  });

  describe('form-only vote sentinels', () => {
    const SENTINEL_QUERIES = ['abstain', 'no_confidence'];
    const SENTINEL_LABELS = ['Abstain', 'No Confidence'];

    it('renders no row for either sentinel and never resolves one to a detail view', () => {
      const onViewDetails = jest.fn();
      const onSelectForDelegation = jest.fn();
      renderComponent({
        drepList: [realEntry(1), realEntry(2)],
        onViewDetails,
        onSelectForDelegation,
      });

      const input = screen.getByPlaceholderText('!!!Search by DRep ID');
      SENTINEL_QUERIES.forEach((query) => {
        fireEvent.change(input, { target: { value: query } });
        SENTINEL_LABELS.forEach((label) => {
          expect(screen.queryByText(label)).not.toBeInTheDocument();
        });
      });

      expect(onViewDetails).not.toHaveBeenCalled();
      expect(onSelectForDelegation).not.toHaveBeenCalled();
    });

    it('falls back to the no-results empty state whose copy names neither sentinel', () => {
      renderComponent({ drepList: [realEntry(1)] });

      fireEvent.change(screen.getByPlaceholderText('!!!Search by DRep ID'), {
        target: { value: 'no_confidence' },
      });

      const emptyState = document.querySelector('[data-variant="noResults"]');
      expect(emptyState).not.toBeNull();
      SENTINEL_LABELS.forEach((label) => {
        expect(emptyState.textContent).not.toContain(label);
      });
      expect(screen.queryByText('!!!View details')).not.toBeInTheDocument();
    });

    it('keeps every directory and favorites string free of the sentinel labels in both locales', () => {
      const catalogs: Record<string, string>[] = [translations, jaTranslations];
      const namespaces = [
        'governance.drepDirectory.',
        'governance.drepFavorites.',
      ];

      catalogs.forEach((catalog) => {
        const labels = [
          catalog['voting.governance.abstain'],
          catalog['voting.governance.noConfidence'],
        ];
        const conflicting = Object.keys(catalog)
          .filter((key) => namespaces.some((ns) => key.startsWith(ns)))
          .filter((key) =>
            labels.some((label) => catalog[key].includes(label))
          );
        expect(conflicting).toEqual([]);
      });
    });

    it('routes no sentinel literal into a logger sink while searching', () => {
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

      renderComponent({ drepList: [realEntry(1)] });

      const input = screen.getByPlaceholderText('!!!Search by DRep ID');
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
});
