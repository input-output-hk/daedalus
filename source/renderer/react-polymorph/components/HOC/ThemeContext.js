// @flow
import React from 'react';
import createReactContext, { type Context } from 'create-react-context';
import { ROOT_THEME_API } from '../../themes/API';

// components that are NOT directly nested within a ThemeProvider
// can access simple theme as "this.props.context.theme",
// same goes for "this.props.context.ROOT_THEME_API"
// if the user passes ThemeProvider a theme and/or ROOT_THEME_API,
// these default values are overwritten

type Theme = {
  skins: Object,
  theme: Object,
  ROOT_THEME_API: Object,
};

const defaultContext = {
  skins: {},
  theme: ROOT_THEME_API,
  ROOT_THEME_API,
};

export const ThemeContext: Context<Theme> = createReactContext(defaultContext);
