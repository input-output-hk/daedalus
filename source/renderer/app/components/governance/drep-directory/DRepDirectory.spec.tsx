import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepDirectory from './DRepDirectory';
import {
  GovernanceRefreshState,
  AppDRepDirectoryEntry,
} from '../../../stores/GovernanceStore';

const baseEntries: AppDRepDirectoryEntry[] = [
  {
    anchor: null,
    drepActivity: 12,
    drepId: 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b',
    status: 'active',
    votingPower: new BigNumber('23137980123456'),
  },
];

const buildEntry = (suffix: number): AppDRepDirectoryEntry => ({
  anchor: null,
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

const renderComponent = ({
  drepList = baseEntries,
  error = null,
  isNodeInSync = true,
  refreshState = GovernanceRefreshState.Loaded,
  locale = 'en-US',
  onSelectForDelegation = jest.fn(),
  syncProgress = 100,
}: {
  drepList?: AppDRepDirectoryEntry[];
  error?: { message: string; type: string; details?: string } | null;
  isNodeInSync?: boolean;
  refreshState?: GovernanceRefreshState;
  locale?: string;
  onSelectForDelegation?: jest.Mock;
  syncProgress?: number | null;
} = {}) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepDirectory
        drepList={drepList}
        error={error}
        isNodeInSync={isNodeInSync}
        lastFetchedAt={Date.now() - 60_000}
        onRefresh={jest.fn()}
        onSelectForDelegation={onSelectForDelegation}
        refreshState={refreshState}
        syncProgress={syncProgress}
      />
    </IntlProvider>
  );
};

describe('DRepDirectory', () => {
  afterEach(cleanup);

  it('renders the loaded bare list with voting power and status', () => {
    renderComponent();

    expect(screen.getByText('!!!DRep Directory')).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
    expect(screen.getByText('!!!Active')).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
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

  it('keeps the retained list visible with a non-blocking error banner after refresh failure', () => {
    renderComponent({
      error: {
        message:
          'Showing the last successful directory snapshot while refresh retries.',
        type: 'QUERY_FAILED',
      },
      refreshState: GovernanceRefreshState.Loaded,
    });

    expect(
      screen.getByText('!!!Could not load DRep data.')
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Showing the last successful directory snapshot while refresh retries.'
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Voting power:')).toBeInTheDocument();
  });

  it('surfaces actionable error details in the non-blocking error banner', () => {
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
      screen.getByText('Missing: --mainnet | --testnet-magic NATURAL')
    ).toBeInTheDocument();
    // Retained list stays visible alongside the actionable banner.
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
    expect(screen.getByText('DRepディレクトリ')).toBeInTheDocument();
    expect(screen.getByText(/投票権/)).toBeInTheDocument();
    expect(screen.getByText('アクティブ')).toBeInTheDocument();
    expect(screen.getByText('オンチェーン')).toBeInTheDocument();
  });

  it('renders a loading indicator when in Loading refresh state', () => {
    renderComponent({
      drepList: [],
      refreshState: GovernanceRefreshState.Loading,
    });

    expect(screen.getByText('!!!Loading DRep data…')).toBeInTheDocument();
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
});
