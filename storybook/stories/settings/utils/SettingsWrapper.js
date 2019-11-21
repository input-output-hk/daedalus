// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import StoryLayout from '../../_support/StoryLayout';
import StoryProvider from '../../_support/StoryProvider';
import StoryDecorator from '../../_support/StoryDecorator';
import SettingsLayout from '../../../../source/renderer/app/components/settings/SettingsLayout';
import SettingsMenu from '../../../../source/renderer/app/components/settings/menu/SettingsMenu';

const pageNames = {
  '/settings/index': 'General',
  '/settings/display': 'Themes',
  '/settings/terms-of-use': 'Terms of use',
  '/settings/support': 'Support',
};

/* eslint-disable react/display-name  */
export default (story: Object, context: Object) => {
  const storyWithKnobs = withKnobs(story, context);

  const menu = (
    <SettingsMenu
      onItemClick={linkTo(context.kind, item => pageNames[item])}
      isActiveItem={item => {
        const itemName = context.story
          .toLocaleLowerCase()
          .replace('index', 'general')
          .replace('themes', 'display')
          .replace(/ /g, '-');
        return item === `/settings/${itemName}`;
      }}
      showDisplaySettings
    />
  );

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout activeSidebarCategory="/settings" {...context}>
          <SettingsLayout menu={menu}>{storyWithKnobs}</SettingsLayout>
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
