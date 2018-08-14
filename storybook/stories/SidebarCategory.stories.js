// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import SidebarCategory from '../../source/renderer/app/components/sidebar/SidebarCategory';
import walletsIcon from '../../source/renderer/app/assets/images/sidebar/wallet-ic.inline.svg';

storiesOf('SidebarCategory')

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('inactive', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      active={false}
      onClick={action('categoryClicked')}
      className=""
    />
  ))

  .add('active', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      active
      onClick={action('categoryClicked')}
      className=""
    />
  ));
