import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import SidebarCategory from '../app/components/sidebar/SidebarCategory';
import walletsIcon from '../app/assets/images/sidebar/wallet-ic.inline.svg';

storiesOf('SidebarCategory')

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('inactive', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      onClick={action('categoryClicked')}
    />
  ))

  .add('active', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      active
      onClick={action('categoryClicked')}
    />
  ))

  .add('minimized', () => (
    <SidebarCategory
      label="Wallets"
      icon={walletsIcon}
      minimized
      onClick={action('categoryClicked')}
    />
  ));
