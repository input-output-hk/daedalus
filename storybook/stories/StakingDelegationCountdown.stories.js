// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, date, number } from '@storybook/addon-knobs';
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';
import { CATEGORIES_BY_NAME } from '../../source/renderer/app/config/sidebarConfig';
import StakingDelegationCountdown from '../../source/renderer/app/components/staking/delegation-countdown/StakingDelegationCountdown';

const defaultPercentage = 10;
const defaultStartDateTime = new Date('Jun 01 2019');
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);
  return new Date(stringTimestamp).toISOString();
};

storiesOf('DelegationScreens', module)
  .addDecorator((story, context) => {
    const storyWithKnobs = withKnobs(story, context);
    const getItemFromContext = () =>
      context.story
        .replace('Decentralization Progress', 'info')
        .toLocaleLowerCase();

    let activeSidebarCategory = null;
    if (context.story === 'Decentralization Progress') {
      activeSidebarCategory = CATEGORIES_BY_NAME.DELEGATION_PROGRESS.route;
    } else {
      activeSidebarCategory = CATEGORIES_BY_NAME.DELEGATION.route;
    }

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory={CATEGORIES_BY_NAME.STAKING.route}
            storyName={context.story}
          >
            {context.story === 'Decentralization Progress' ? (
              <DelegationProgressWithNavigation
                isActiveScreen={item => item === getItemFromContext()}
                onDelegationProgressNavItemClick={linkTo(
                  'DelegationScreens',
                  () => 'Decentralization Progress'
                )}
              >
                {storyWithKnobs}
              </DelegationProgressWithNavigation>
            ) : (
              storyWithKnobs
            )}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  // ====== Stories ======

  // .add('Start of decentralization notification', () => (
  //   <Delegation
  //     currentLocale="en-US"
  //     startDateTime={startDateTimeKnob(
  //       'Delegation Start DateTime',
  //       defaultStartDateTime
  //     )}
  //   />
  // ))
  // .add('Decentralization Progress', () => (
  //   <DelegationProgress
  //     percentage={number('Percentage', defaultPercentage, {
  //       min: 0,
  //       max: 100,
  //       step: 1,
  //       range: true,
  //     })}
  //   />
  .add('Start of decentralisation notification', () => (
    <div>
      <StakingDelegationCountdown
        currentLocale="en-US"
        startDateTime={startDateTimeKnob(
          'Delegation Start DateTime',
          defaultStartDateTime
        )}
      />
    </div>
  ));
