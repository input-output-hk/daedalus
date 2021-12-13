import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SidebarCategory from '../../../source/renderer/app/components/sidebar/SidebarCategory';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../source/renderer/app/a... Remove this comment to see the full error message
import walletsIcon from '../../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import StoryDecorator from '../_support/StoryDecorator';

const category = {
  name: 'Wallets',
  icon: walletsIcon,
  route: 'WALLETS',
};
storiesOf('Navigation|Sidebar', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Inactive', () => (
    <SidebarCategory
      category={category}
      isActive={false}
      onClick={action('categoryClicked')}
    />
  ))
  .add('Active', () => (
    <SidebarCategory
      category={category}
      isActive
      onClick={action('categoryClicked')}
    />
  ));
