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

const highValueEntry: DRepCategorySource = {
  drepActivity: 20,
  verifiedName: 'Cardano Foundation DRep',
};

const thresholdEntry: DRepCategorySource = {
  drepActivity: 9,
  verifiedName: null,
};

const primaryEntry: DRepCategorySource = {
  drepActivity: 20,
  verifiedName: 'IOHK Governance',
};

const nonMetadataEntry: DRepCategorySource = {
  drepActivity: 20,
  verifiedName: null,
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
      <DRepCategoryBadge entry={highValueEntry} />
      <DRepCategoryBadge entry={thresholdEntry} />
      <DRepCategoryBadge entry={primaryEntry} />
      <DRepCategoryBadge entry={nonMetadataEntry} />
    </div>
  ));
