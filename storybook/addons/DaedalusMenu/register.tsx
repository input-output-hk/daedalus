import React from 'react';
import addons, { types } from '@storybook/addons';
import DaedalusMenu from './DaedalusMenu';

const ADDON_ID = 'daedalusmenu';
const PANEL_ID = `${ADDON_ID}/panel`;

addons.register(ADDON_ID, (api) => {
  addons.add(PANEL_ID, {
    title: 'Daedalus Menu',
    type: types.TOOL,
    render: () => <DaedalusMenu api={api} />,
  });
});
