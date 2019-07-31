// @flow
import React from 'react';
import addons, { types } from '@storybook/addons';
import DaedalusMenu from './DaedalusMenu';

/* eslint-disable react/display-name  */

const ADDON_ID = 'daedalusmenu';
const PANEL_ID = `${ADDON_ID}/panel`;

addons.register(ADDON_ID, api => {
  const render = () => <DaedalusMenu api={api} />;
  addons.add(PANEL_ID, {
    type: types.TOOL,
    render,
  });
});
