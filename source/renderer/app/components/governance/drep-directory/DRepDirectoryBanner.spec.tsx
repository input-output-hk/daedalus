import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import DRepDirectoryBanner from './DRepDirectoryBanner';

const renderBanner = ({
  isRefreshing = false,
  locale = 'en-US',
  isFilteredView,
  displayedCount,
  isFavoritesView,
  favoritesCount,
}: {
  isRefreshing?: boolean;
  locale?: string;
  isFilteredView?: boolean;
  displayedCount?: number;
  isFavoritesView?: boolean;
  favoritesCount?: number;
} = {}) => {
  const messages = translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepDirectoryBanner
        isRefreshing={isRefreshing}
        lastFetchedAt={Date.now() - 60_000}
        onRefresh={jest.fn()}
        isFilteredView={isFilteredView}
        displayedCount={displayedCount}
        isFavoritesView={isFavoritesView}
        favoritesCount={favoritesCount}
      />
    </IntlProvider>
  );
};

describe('DRepDirectoryBanner', () => {
  afterEach(cleanup);

  it('renders the refresh button without repeating the page name', () => {
    renderBanner();

    // Refresh is an icon; its wording lives in the accessible name.
    expect(
      screen.getByRole('button', { name: '!!!Refresh' })
    ).toBeInTheDocument();
    // The governance tab bar names the page; the banner must not say it again.
    expect(screen.queryByRole('heading')).not.toBeInTheDocument();
  });

  it('shows the filtered line when isFilteredView is true', () => {
    renderBanner({ isFilteredView: true, displayedCount: 7 });

    expect(
      screen.getByText('!!!Showing 7 DReps matching your filters.')
    ).toBeInTheDocument();
  });

  it('replaces the filtered line with the favorites line in favorites mode', () => {
    renderBanner({
      isFavoritesView: true,
      favoritesCount: 3,
      isFilteredView: true,
      displayedCount: 9,
    });

    expect(
      screen.getByText(
        /3 DReps you've favorited\. Favorites are stored on this device only\./
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/matching your filters/)).not.toBeInTheDocument();
  });

  it('renders the refreshing badge beside the last-updated timestamp', () => {
    renderBanner({ isRefreshing: true });

    expect(screen.getByText(/Last updated/)).toBeInTheDocument();
    expect(screen.getByText('!!!Refreshing…')).toBeInTheDocument();
  });

  it('renders no refreshing badge while no refresh is in flight', () => {
    renderBanner();

    expect(screen.getByText(/Last updated/)).toBeInTheDocument();
    expect(screen.queryByText('!!!Refreshing…')).not.toBeInTheDocument();
  });
});
