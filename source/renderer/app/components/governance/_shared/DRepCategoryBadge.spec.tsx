import React from 'react';
import fs from 'fs';
import path from 'path';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import DRepCategoryBadge, { getDRepCategory } from './DRepCategoryBadge';
import type { DRepCategorySource } from './DRepCategoryBadge';

const baseEntry: DRepCategorySource = {
  drepActivity: 20,
  verifiedName: null,
};

const renderBadge = (entry: DRepCategorySource, locale = 'en-US') => {
  const messages = translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepCategoryBadge entry={entry} />
    </IntlProvider>
  );
};

describe('getDRepCategory', () => {
  it('classifies a verified entry as primary', () => {
    expect(getDRepCategory({ ...baseEntry, verifiedName: 'Some DRep' })).toBe(
      'primary'
    );
  });

  it('classifies an entry without a verified name as nonMetadata', () => {
    expect(getDRepCategory(baseEntry)).toBe('nonMetadata');
  });

  it('applies threshold across the whole 7-12 epoch window', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 7 })).toBe(
      'threshold'
    );
    expect(getDRepCategory({ ...baseEntry, drepActivity: 12 })).toBe(
      'threshold'
    );
  });

  it('gives threshold priority over primary for a verified entry in the window', () => {
    expect(
      getDRepCategory({ drepActivity: 9, verifiedName: 'Some DRep' })
    ).toBe('threshold');
  });

  it('leaves 6 and 13 remaining epochs outside the threshold window', () => {
    expect(
      getDRepCategory({ verifiedName: 'Some DRep', drepActivity: 13 })
    ).toBe('primary');
    expect(getDRepCategory({ ...baseEntry, drepActivity: 6 })).toBe(
      'nonMetadata'
    );
  });

  it('treats null drepActivity as outside the threshold window', () => {
    expect(
      getDRepCategory({ verifiedName: 'Some DRep', drepActivity: null })
    ).toBe('primary');
    expect(getDRepCategory({ verifiedName: null, drepActivity: null })).toBe(
      'nonMetadata'
    );
  });
});

describe('DRepCategoryBadge', () => {
  afterEach(cleanup);

  it('renders the primary label with its explanatory tooltip', () => {
    renderBadge({ verifiedName: 'Some DRep', drepActivity: 20 });

    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toHaveAttribute('title', '!!!Has verified off-chain metadata.');
  });

  it('renders the threshold label with its tooltip', () => {
    renderBadge({ verifiedName: null, drepActivity: 9 });

    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Approaching expiry — review before delegating.'
    );
  });

  it('renders the nonMetadata label with its tooltip', () => {
    renderBadge(baseEntry);

    expect(
      screen.getByText('!!!Non-metadata').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Eligible for delegation but has no verified off-chain metadata yet.'
    );
  });
});

describe('category badge isolation', () => {
  const readSource = (relative: string) =>
    fs.readFileSync(path.resolve(__dirname, relative), 'utf8');

  const CONSUMERS_THAT_MUST_NOT_READ_THE_CATEGORY = [
    '../drep-directory/helpers.ts',
    '../drep-directory/DRepDirectory.tsx',
    '../drep-directory/DRepDirectoryList.tsx',
    '../drep-directory/DRepDirectoryFilters.tsx',
    '../drep-directory/DRepDirectorySearch.tsx',
  ];

  it('keeps ordering, filtering and search helpers free of the badge module', () => {
    CONSUMERS_THAT_MUST_NOT_READ_THE_CATEGORY.forEach((relative) => {
      const source = readSource(relative);
      expect(source).not.toMatch(
        /getDRepCategory|DRepCategoryBadge|DRepCategory\b/
      );
    });
  });

  it('keeps the governance store free of the badge module', () => {
    expect(readSource('../../../stores/GovernanceStore.ts')).not.toMatch(
      /getDRepCategory|DRepCategoryBadge|DRepCategory\b/
    );
  });
});
