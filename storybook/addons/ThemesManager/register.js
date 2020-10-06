// @flow
import React from 'react';
import { addons, types } from '@storybook/addons';
import { AddonPanel } from '@storybook/components';
import ThemesManager from './ThemesManager';

const ADDON_ID = 'themesmanager';
const PANEL_ID = `${ADDON_ID}/panel`;

addons.register(ADDON_ID, api => {
  addons.add(PANEL_ID, {
    type: types.PANEL,
    title: 'Themes Manager',
    render: ({ active, key }: Object) => (
      <AddonPanel active={active} key={key}>
        <ThemesManager api={api} />
      </AddonPanel>
    ),
  });
});
