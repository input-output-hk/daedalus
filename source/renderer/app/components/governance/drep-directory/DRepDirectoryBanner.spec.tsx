import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepDirectoryBanner from './DRepDirectoryBanner';

const renderBanner = ({
  isCohortActive = true,
  locale = 'en-US',
  showSource,
}: {
  isCohortActive?: boolean;
  locale?: string;
  showSource?: boolean;
} = {}) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepDirectoryBanner
        isCohortActive={isCohortActive}
        isRefreshing={false}
        lastFetchedAt={Date.now() - 60_000}
        onRefresh={jest.fn()}
        onReshuffle={jest.fn()}
        showSource={showSource}
      />
    </IntlProvider>
  );
};

describe('DRepDirectoryBanner', () => {
  afterEach(cleanup);

  it('renders the BMVG citation beneath the cohort line by default', () => {
    renderBanner();

    expect(
      screen.getByText(/Default view shows up to 200/)
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis.'
      )
    ).toBeInTheDocument();
  });

  it('hides the citation only via the story-only showSource flag', () => {
    renderBanner({ showSource: false });

    expect(
      screen.getByText(/Default view shows up to 200/)
    ).toBeInTheDocument();
    expect(screen.queryByText(/Beyond MVG/)).not.toBeInTheDocument();
  });

  it('renders neither cohort line nor citation when the cohort is inactive', () => {
    renderBanner({ isCohortActive: false });

    expect(
      screen.queryByText(/Default view shows up to 200/)
    ).not.toBeInTheDocument();
    expect(screen.queryByText(/Beyond MVG/)).not.toBeInTheDocument();
  });

  it('renders the citation in ja-JP', () => {
    renderBanner({ locale: 'ja-JP' });

    expect(screen.getByText(/Beyond MVG/)).toBeInTheDocument();
    expect(screen.getByText(/ワンクリック委任分析/)).toBeInTheDocument();
  });
});
