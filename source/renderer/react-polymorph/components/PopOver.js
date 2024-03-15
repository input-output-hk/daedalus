// @flow
import { PopperOptions } from 'popper.js';
import React, {
  ComponentType,
  ReactElement,
  ReactNode,
  StatelessFunctionalComponent,
  useContext,
} from 'react';
import { addThemeId, composeTheme } from '../utils/themes';
import { ThemeContext } from './HOC/ThemeContext';
import type { ThemeContextProp } from './HOC/withTheme';
import { IDENTIFIERS } from './index';

/**
 * PopOver UI component
 * Use cases: Tooltips, Menus, Flyovers, Bubbles etc.
 *
 * Based on https://github.com/atomiks/tippyjs-react
 * which is based on https://popper.js.org/
 *
 * Supports all props as listed here:
 * https://atomiks.github.io/tippyjs/v6/all-props/
 *
 * Known limitations:
 * Translucent backgrounds cannot be combined with borders because popper.js
 * handles the main box and arrow as two separate elements and thus the border
 * of the main box shines through the arrow bg
 */

export type PopOverProps = {
  allowHTML?: boolean,
  children?: ReactElement<any>,
  className?: string,
  contentClassName?: string,
  content: ReactNode,
  context?: ThemeContextProp,
  popperOptions: PopperOptions,
  skin?: ComponentType<any>,
  theme?: ?Object,
  themeId?: string,
  themeOverrides?: { [index: string]: string },
  themeVariables?: { [index: string]: string },
};

export function PopOver(
  props: PopOverProps
): StatelessFunctionalComponent<PopOverProps> {
  const { context, skin, theme, themeId, themeOverrides } = props;

  // Theme
  const themeContext = context || useContext(ThemeContext);
  if (!themeContext) {
    throw new Error('No theming context provided.');
  }
  const composedTheme = composeTheme(
    addThemeId(theme || themeContext.theme, themeId),
    addThemeId(themeOverrides, themeId),
    themeContext.ROOT_THEME_API
  );

  // Skin
  const PopOverSkin = skin || themeContext.skins[IDENTIFIERS.POP_OVER];
  return <PopOverSkin {...props} theme={composedTheme} />;
}

PopOver.defaultProps = {
  allowHTML: false,
  theme: null,
  themeId: IDENTIFIERS.POP_OVER,
  themeOverrides: {},
  themeVariables: {},
  popperOptions: {},
};
