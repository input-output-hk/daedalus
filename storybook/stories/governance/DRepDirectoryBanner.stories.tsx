import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import DRepDirectoryBanner from '../../../source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner';

const CENTERED_STYLE = {
  margin: '0 auto',
  maxWidth: 960,
  padding: 24,
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
const renderBanner = (props: {
  isCohortActive: boolean;
  showSource?: boolean;
}) => (
  <div style={CENTERED_STYLE}>
    <DRepDirectoryBanner
      isCohortActive={props.isCohortActive}
      isRefreshing={false}
      lastFetchedAt={Date.now() - 3 * 60 * 1000}
      onRefresh={action('onRefresh')}
      onReshuffle={action('onReshuffle')}
      showSource={props.showSource}
    />
  </div>
);

storiesOf('Governance / DRep Directory Banner', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('Cohort active — with BMVG citation (default)', () =>
    renderBanner({ isCohortActive: true })
  )
  .add('Cohort active — without citation slot', () =>
    renderBanner({ isCohortActive: true, showSource: false })
  )
  .add('Cohort inactive', () => renderBanner({ isCohortActive: false }));
