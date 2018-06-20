// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, boolean, number } from '@storybook/addon-knobs';

import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';

import SettingsLayout from '../../source/renderer/app/components/settings/SettingsLayout';
import SettingsMenu from '../../source/renderer/app/components/settings/menu/SettingsMenu';

import SupportSettings from '../../source/renderer/app/components/settings/categories/SupportSettings';

import getIssuesDetectedOptions from './support/getIssuesDetectedOptions';

const getMenu = (activeItem: string) => (
  <SettingsMenu
    onItemClick={(route) => linkTo('SettingsScreens', route.substr(10))}
    isActiveItem={(route: string) => !!activeItem.match(route.substr(10))}
  />
);

storiesOf('SettingsScreens', module)

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
    );
  })

  // ====== Stories ======

  .add('support', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
      isAnalyzingIssues={!!boolean('isAnalyzingIssues', true)}
      issuesDetected={getIssuesDetectedOptions(number('Issues found', 2, { min: 0, max: 9 }))}
    />
  ));
