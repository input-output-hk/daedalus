import React from 'react';
import { storiesOf } from '@kadira/storybook';
import SidebarMainItem from '../app/components/sidebar/SidebarMainItem';
import walletsIcon from '../app/assets/images/sidebar/wallet-ic.svg';

storiesOf('SidebarMainItem', module)

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('inactive', () => (
    <SidebarMainItem
      label="Wallets"
      icon={walletsIcon}
    />
  ))

  .add('active', () => (
    <SidebarMainItem
      label="Wallets"
      icon={walletsIcon}
      active
    />
  ))

  .add('minimized', () => (
    <SidebarMainItem
      label="Wallets"
      icon={walletsIcon}
      minimized
    />
  ));
