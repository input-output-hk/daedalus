// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, date } from '@storybook/addon-knobs';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';
import Delegation from '../../source/renderer/app/components/staking/Delegation';

const defaultStartDateTime = new Date('Jun 01 2019');
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);
  return new Date(stringTimestamp).toISOString();
};

storiesOf('DelegationScreens', module)
  .addDecorator((story, context) => {
    const storyWithKnobs = withKnobs(story, context);

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory={CATEGORIES_BY_NAME.DELEGATION.route}
            storyName={context.story}
          >
            {storyWithKnobs}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  // ====== Stories ======

  .add('Start of decentralisation notification', () => (
    <div>
      <Delegation
        currentLocale="en-US"
        startDateTime={startDateTimeKnob(
          'Delegation Start DateTime',
          defaultStartDateTime
        )}
      />
    </div>
  ));
