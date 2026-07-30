import React from 'react';
import fs from 'fs';
import path from 'path';
import { IntlProvider } from 'react-intl';
import BigNumber from 'bignumber.js';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import DRepCategoryBadge, { getDRepCategory } from './DRepCategoryBadge';
import type { DRepCategorySource } from './DRepCategoryBadge';
import type { DRepCohortContext } from '../../../stores/GovernanceStore';

const DREP_ID = 'drep1yg7s8vuv87f8a8f5d0m9yk4p5xqw6r4s3t2u1v9w8x7y6z5a4b';

const baseEntry: DRepCategorySource = {
  drepActivity: 20,
  drepId: DREP_ID,
  votingPower: new BigNumber('900'),
};

const outOfCohort: DRepCohortContext = {
  medianVotingPower: null,
  memberIds: new Set<string>(),
  verifiedMetadataIds: new Set<string>(),
};

const inCohortVerified: DRepCohortContext = {
  medianVotingPower: new BigNumber('500'),
  memberIds: new Set([DREP_ID]),
  verifiedMetadataIds: new Set([DREP_ID]),
};

const noCohort: DRepCohortContext = { ...outOfCohort, memberIds: null };

const renderBadge = (
  entry: DRepCategorySource,
  cohort: DRepCohortContext,
  locale = 'en-US'
) => {
  const messages = locale === 'ja-JP' ? jaTranslations : translations;
  return render(
    <IntlProvider locale={locale} messages={messages}>
      <DRepCategoryBadge entry={entry} cohort={cohort} />
    </IntlProvider>
  );
};

describe('getDRepCategory', () => {
  it('classifies an in-cohort verified entry above the median as highValue', () => {
    expect(getDRepCategory(baseEntry, inCohortVerified)).toBe('highValue');
  });

  it('does not classify an entry equal to the median as highValue', () => {
    expect(
      getDRepCategory(
        { ...baseEntry, votingPower: new BigNumber('500') },
        inCohortVerified
      )
    ).toBe('primary');
  });

  it('does not classify an entry without voting power as highValue', () => {
    expect(
      getDRepCategory({ ...baseEntry, votingPower: null }, inCohortVerified)
    ).toBe('primary');
  });

  it('does not classify as highValue when the cohort has no median', () => {
    expect(
      getDRepCategory(baseEntry, {
        ...inCohortVerified,
        medianVotingPower: null,
      })
    ).toBe('primary');
  });

  it('gives highValue priority over the threshold window', () => {
    expect(
      getDRepCategory({ ...baseEntry, drepActivity: 10 }, inCohortVerified)
    ).toBe('highValue');
  });

  it('applies threshold across the whole 7-12 epoch window', () => {
    expect(
      getDRepCategory({ ...baseEntry, drepActivity: 7 }, outOfCohort)
    ).toBe('threshold');
    expect(
      getDRepCategory({ ...baseEntry, drepActivity: 12 }, outOfCohort)
    ).toBe('threshold');
  });

  it('gives threshold priority over primary for a verified entry in the window', () => {
    expect(
      getDRepCategory(
        { ...baseEntry, drepActivity: 9 },
        { ...outOfCohort, verifiedMetadataIds: new Set([DREP_ID]) }
      )
    ).toBe('threshold');
  });

  it('applies threshold to an out-of-cohort entry', () => {
    expect(getDRepCategory({ ...baseEntry, drepActivity: 8 }, noCohort)).toBe(
      'threshold'
    );
  });

  it('classifies a verified in-cohort entry at or below the median as primary', () => {
    expect(
      getDRepCategory(
        { ...baseEntry, votingPower: new BigNumber('400') },
        inCohortVerified
      )
    ).toBe('primary');
  });

  it('classifies a verified out-of-cohort entry as primary', () => {
    expect(
      getDRepCategory(baseEntry, {
        ...outOfCohort,
        verifiedMetadataIds: new Set([DREP_ID]),
      })
    ).toBe('primary');
  });

  it('classifies an entry without verified metadata as nonMetadata', () => {
    expect(getDRepCategory(baseEntry, outOfCohort)).toBe('nonMetadata');
    // An on-chain anchor reference is not verified metadata.
    const withOnchainAnchor = {
      ...baseEntry,
      anchor: {
        hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
        url: 'https://governance-preview.example.org/dreps/1.json',
      },
    } as DRepCategorySource;
    expect(getDRepCategory(withOnchainAnchor, outOfCohort)).toBe('nonMetadata');
  });

  it('leaves 6 and 13 remaining epochs outside the threshold window', () => {
    expect(
      getDRepCategory(
        { ...baseEntry, drepActivity: 13 },
        { ...outOfCohort, verifiedMetadataIds: new Set([DREP_ID]) }
      )
    ).toBe('primary');
    expect(
      getDRepCategory({ ...baseEntry, drepActivity: 6 }, outOfCohort)
    ).toBe('nonMetadata');
  });

  it('treats null drepActivity as outside the threshold window', () => {
    expect(
      getDRepCategory(
        { ...baseEntry, drepActivity: null },
        { ...outOfCohort, verifiedMetadataIds: new Set([DREP_ID]) }
      )
    ).toBe('primary');
  });

  it('classifies the same entry as highValue in cohort and primary out of cohort', () => {
    expect(getDRepCategory(baseEntry, inCohortVerified)).toBe('highValue');
    expect(
      getDRepCategory(baseEntry, {
        ...inCohortVerified,
        memberIds: new Set<string>(),
      })
    ).toBe('primary');
  });

  it('never returns highValue while the cohort is inactive', () => {
    expect(
      getDRepCategory(baseEntry, {
        medianVotingPower: new BigNumber('500'),
        memberIds: null,
        verifiedMetadataIds: new Set([DREP_ID]),
      })
    ).toBe('primary');
  });
});

describe('DRepCategoryBadge', () => {
  afterEach(cleanup);

  it('renders the highValue label with its explanatory tooltip', () => {
    renderBadge(baseEntry, inCohortVerified);

    expect(
      screen.getByText('!!!High value').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.'
    );
  });

  it('renders the primary label with its explanatory tooltip', () => {
    renderBadge(baseEntry, {
      ...outOfCohort,
      verifiedMetadataIds: new Set([DREP_ID]),
    });

    expect(
      screen.getByText('!!!Primary').closest('span[title]')
    ).toHaveAttribute('title', '!!!Has verified off-chain metadata.');
  });

  it('renders the threshold label with its tooltip', () => {
    renderBadge({ ...baseEntry, drepActivity: 9 }, outOfCohort);

    expect(
      screen.getByText('!!!Threshold').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Approaching expiry — review before delegating.'
    );
  });

  it('renders the nonMetadata label with its tooltip', () => {
    renderBadge(baseEntry, outOfCohort);

    expect(
      screen.getByText('!!!Non-metadata').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Eligible for delegation but has no verified off-chain metadata yet.'
    );
  });

  it('renders category labels in ja-JP', () => {
    renderBadge(baseEntry, inCohortVerified, 'ja-JP');
    renderBadge({ ...baseEntry, drepActivity: 9 }, outOfCohort, 'ja-JP');

    expect(screen.getByText('!!!高価値')).toBeInTheDocument();
    expect(screen.getByText('!!!しきい値')).toBeInTheDocument();
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
