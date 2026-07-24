import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepCategoryBadge, { getDRepCategory } from './DRepCategoryBadge';
import type { DRepCategorySource } from './DRepCategoryBadge';

const baseEntry: DRepCategorySource = {
  anchor: {
    hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
    url: 'https://governance-preview.example.org/dreps/1.json',
  },
  drepActivity: 20,
  status: 'active',
};

const renderBadge = (entry: DRepCategorySource, locale = 'en-US') => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepCategoryBadge entry={entry} />
    </IntlProvider>
  );
};

describe('getDRepCategory', () => {
  it('categorizes an entry with an anchor outside the threshold window as primary', () => {
    expect(getDRepCategory(baseEntry)).toBe('primary');
  });

  it('categorizes an anchor-less entry outside the threshold window as nonMetadata', () => {
    expect(getDRepCategory({ ...baseEntry, anchor: null })).toBe('nonMetadata');
  });

  it('applies threshold across the whole 7-12 epoch window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 7 })).toBe(
      'threshold'
    );
    expect(getDRepCategory({ ...baseEntry, drepActivity: 12 })).toBe(
      'threshold'
    );
  });

  it('gives threshold priority over primary for a 7-12 epoch entry with metadata', () => {
    // The binding tie-break: metadata never demotes the expiry warning.
    expect(getDRepCategory({ ...baseEntry, drepActivity: 10 })).toBe(
      'threshold'
    );
  });

  it('gives threshold priority over nonMetadata inside the window', () => {
    expect(
      getDRepCategory({ ...baseEntry, anchor: null, drepActivity: 8 })
    ).toBe('threshold');
  });

  it('leaves 6 and 13 remaining epochs outside the threshold window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 13 })).toBe('primary');
    expect(
      getDRepCategory({ ...baseEntry, anchor: null, drepActivity: 6 })
    ).toBe('nonMetadata');
  });

  it('treats null drepActivity as outside the threshold window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: null })).toBe(
      'primary'
    );
  });
});

describe('DRepCategoryBadge', () => {
  afterEach(cleanup);

  it('renders the primary label with its explanatory tooltip', () => {
    renderBadge(baseEntry);

    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Inside the default Recommended view with verified metadata.'
    );
  });

  it('renders the threshold label with its tooltip', () => {
    renderBadge({ ...baseEntry, drepActivity: 9 });

    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Inside the default Recommended view but approaching expiry — review before delegating.'
    );
  });

  it('renders the nonMetadata label with its tooltip', () => {
    renderBadge({ ...baseEntry, anchor: null });

    expect(
      screen.getByText('!!!Non-metadata').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Eligible for delegation but has no verified off-chain metadata yet.'
    );
  });

  it('renders category labels in ja-JP', () => {
    renderBadge({ ...baseEntry, drepActivity: 9 }, 'ja-JP');

    expect(screen.getByText('!!!しきい値')).toBeInTheDocument();
  });
});
