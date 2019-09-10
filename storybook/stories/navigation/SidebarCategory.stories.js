// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SidebarCategory from '../../../source/renderer/app/components/sidebar/SidebarCategory';
import walletsIcon from '../../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';
import StoryDecorator from '../_support/StoryDecorator';

storiesOf('Navigation|Sidebar', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Inactive', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      active={false}
      onClick={action('categoryClicked')}
      className=""
    />
  ))

  .add('Active', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      active
      onClick={action('categoryClicked')}
      className=""
    />
  ));
