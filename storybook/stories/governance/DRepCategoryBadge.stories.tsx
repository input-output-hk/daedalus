import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepCategoryBadge from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';
import type { DRepCategorySource } from '../../../source/renderer/app/components/governance/_shared/DRepCategoryBadge';

const ROW_STYLE = {
  display: 'flex',
  flexWrap: 'wrap' as const,
  gap: 16,
  padding: 24,
};

const anchor = {
  hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
  url: 'https://governance-preview.example.org/dreps/1.json',
};

const primaryEntry: DRepCategorySource = {
  anchor,
  drepActivity: 20,
  status: 'active',
};

const thresholdEntry: DRepCategorySource = {
  anchor,
  drepActivity: 9,
  status: 'active',
};

const nonMetadataEntry: DRepCategorySource = {
  anchor: null,
  drepActivity: 20,
  status: 'active',
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / DRep Category Badge', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('All categories', () => (
    <div style={ROW_STYLE}>
      <DRepCategoryBadge entry={primaryEntry} />
      <DRepCategoryBadge entry={thresholdEntry} />
      <DRepCategoryBadge entry={nonMetadataEntry} />
    </div>
  ));
