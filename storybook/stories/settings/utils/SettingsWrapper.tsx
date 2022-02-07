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
  '/settings/stake-pools': 'Stake Pools',
  '/settings/display': 'Themes',
  '/settings/terms-of-service': 'Terms of service',
  '/settings/support': 'Support',
};
/* eslint-disable react/display-name  */

export default (story: Record<string, any>, context: Record<string, any>) => {
  const storyWithKnobs = withKnobs(story, context);
  const menu = (
    <SettingsMenu
      isFlight={false}
      isSyncing={false}
      currentRoute=""
      onItemClick={linkTo(context.kind, (item) => pageNames[item])}
      isActiveItem={(item) => {
        const itemName = context.story
          .toLocaleLowerCase()
          .replace('index', 'general')
          .replace('themes', 'display')
          .replace(/ /g, '-');
        return item === `/settings/${itemName}`;
      }}
    />
  );
  return (
    <StoryDecorator>
      <StoryProvider>
        {/* @ts-ignore ts-migrate(2769) FIXME: No overload matches this call. */}
        <StoryLayout activeSidebarCategory="/settings" {...context}>
          <SettingsLayout menu={menu} activePage="/settings">
            {storyWithKnobs}
          </SettingsLayout>
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};
