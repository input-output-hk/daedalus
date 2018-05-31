// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, text, boolean, number } from '@storybook/addon-knobs';

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

storiesOf('SettingsScreens')

  .addDecorator((story, context) => (
    (context.story.indexOf('screen only') === -1)
      ? (
        <StoryDecorator>
          <StoryProvider>
            <StoryLayout
              activeSidebarCategory="/settings"
            >
              <SettingsLayout menu={getMenu(context.story)}>
                { story() }
              </SettingsLayout>
            </StoryLayout>
          </StoryProvider>
        </StoryDecorator>
      )
      : <StoryDecorator>{ story() }</StoryDecorator>
  ))

  // ====== Stories ======

  .add('support', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
    />
  ))

  .add('support - screen only', () => (
    <SupportSettings
      onExternalLinkClick={() => {}}
      onSupportRequestClick={() => {}}
      onDownloadLogs={() => {}}
    />
  ));
