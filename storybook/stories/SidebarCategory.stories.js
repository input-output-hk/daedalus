// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SidebarCategory from '../../source/renderer/app/components/sidebar/SidebarCategory';
import walletsIcon from '../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import StoryDecorator from './support/StoryDecorator';

const category = {
  name: 'Wallets',
  icon: walletsIcon,
  route: 'WALLETS',
};

storiesOf('SidebarCategory', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('inactive', () => (
    <SidebarCategory
      category={category}
      isActive={false}
      onClick={action('categoryClicked')}
    />
  ))

  .add('active', () => (
    <SidebarCategory
      category={category}
      isActive
      onClick={action('categoryClicked')}
    />
  ));
