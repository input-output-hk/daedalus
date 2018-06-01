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

const getMenu = (activeItem: string) => (
  <SettingsMenu
    onItemClick={(route) => linkTo('SettingsScreens', route.substr(10))}
    isActiveItem={(route: string) => !!activeItem.match(route.substr(10))}
  />
);

const issues = [
  { id: 227, category: 'Daedalus', subCategory: 'Installation', title: 'Your computer time is out of sync.' },
  { id: 228, category: 'Daedalus', subCategory: 'Connection', title: 'Local block data is corrupted.' },
  { id: 229, category: 'Daedalus', subCategory: 'Operation', title: 'Launching node without admin rights.' },
  { id: 230, category: 'Daedalus', subCategory: 'Installation', title: 'File(s) missing.' },
  { id: 231, category: 'Daedalus', subCategory: 'Installation', title: 'Not enough space to store block data.' },
  { id: 234, category: 'Daedalus', subCategory: 'Operation', title: 'Network error.' },
  { id: 235, category: 'Daedalus', subCategory: 'Installation', title: 'User name contains non-Latin characters.' },
  { id: 236, category: 'Daedalus', subCategory: 'Operation', title: '‘open.lock’ file has been corrupted.' },
  { id: 237, category: 'Daedalus', subCategory: 'Operation', title: 'Firewall is blocking connection' },
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
    );
  })

  // ====== Stories ======

  .add('support', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
      issuesDetected={getIssuesDetectedOptions(boolean('Is analyzing', false), number('Issues found', 2, { min: 0, max: 9 }))}
    />
  ))

  .add('support - screen only', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
      issuesDetected={getIssuesDetectedOptions(boolean('Is analyzing', false), number('Issues found', 2, { min: 0, max: 9 }))}
    />
  ));
