import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import Sidebar from '../app/components/sidebar/Sidebar';

storiesOf('Sidebar', module)

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('default', () => (
    <Sidebar route="/" />
  ))

  .add('wallets', () => (
    <Sidebar route="/wallets" />
  ))

  .add('wallets / sub', () => (
    <Sidebar
      route="/wallets/2"
      showMenu
      menus={{
        wallets: {
          items: [
            { id: '1', title: 'Main wallet', info: 'ADA' },
            { id: '2', title: 'House rent', info: '274912874,35 ADA' },
            { id: '3', title: 'Mining', info: '0,0004924712 BTC' },
            { id: '4', title: 'Shopping wallet', info: 'ADA' },
          ],
          actions: {
            onAddWallet: action('onAddWallet')
          }
        }
      }}
      onCategoryClicked={action('onCategoryClicked')}
    />
  ))

  .add('hidden', () => (<Sidebar route="" hidden />));
