// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';

import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';

import SettingsLayout from '../../source/renderer/app/components/settings/SettingsLayout';
import SettingsMenu from '../../source/renderer/app/components/settings/menu/SettingsMenu';

import SupportSettings from '../../source/renderer/app/components/settings/categories/SupportSettings';

const getMenu = (activeItem: string) => (
  <SettingsMenu
    onItemClick={(route) => linkTo('SettingsScreens', route.substr(10))}
    isActiveItem={(route: string) => !!activeItem.match(route.substr(10))}
  />
);

const issues = [
  {
    title: 'SERIOUS PROBLEM'
  },
  {
    title: 'EVEN MORE SERIOUS PROBLEM'
  },
];

const getIssuesDetectedOptions = (isAnalyzing: boolean, issuesFound: number) => (
  !isAnalyzing
    ? issues.slice(0, issuesFound)
    : null
);

storiesOf('SettingsScreens')

  .addDecorator((story, context) => {

    const storyWithKnobs = withKnobs(story, context);

    return (
      (context.story.indexOf('screen only') === -1)
        ? (
          <StoryDecorator>
            <StoryProvider>
              <StoryLayout
                activeSidebarCategory="/settings"
              >
                <SettingsLayout menu={getMenu(context.story)}>
                  { storyWithKnobs }
                </SettingsLayout>
              </StoryLayout>
            </StoryProvider>
          </StoryDecorator>
        )
        : <StoryDecorator>{ storyWithKnobs }</StoryDecorator>
    )
  })

  // ====== Stories ======

  .add('support', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
      issuesDetected={getIssuesDetectedOptions(boolean('Is analyzing', false), number('Issues found', 2, { min: 0, max: 2 }))}
    />
  ))

  .add('support - screen only', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
      issuesDetected={getIssuesDetectedOptions(boolean('Is analyzing', false), number('Issues found', 2, { min: 0, max: 2 }))}
    />
  ));
