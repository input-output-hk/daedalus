import React from 'react';
import { storiesOf } from '@kadira/storybook';
import Sidebar from '../app/components/sidebar/Sidebar';

storiesOf('Sidebar', module)

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('default', () => (
    <Sidebar routePath="/" />
  ))

  .add('wallets', () => (
    <Sidebar routePath="/wallets" />
  ))

  .add('wallets / sub', () => (
    <Sidebar routePath="/wallets" showSubMenus />
  ))

  .add('hidden', () => (<Sidebar routePath="" hidden />));
