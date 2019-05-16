// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number } from '@storybook/addon-knobs';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';
import Delegation from '../../source/renderer/app/components/staking/Delegation';

const timeLeft = (78 * 60 + 10) * 60 * 1000;

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
        timeLeft={number('Time Left (miliseconds)', timeLeft, {
          min: 0,
          step: 1000,
        })}
      />
    </div>
  ));
