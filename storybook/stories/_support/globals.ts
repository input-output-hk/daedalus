import { themesIds, locales, themeNames, localeNames, osNames } from './config';
import type { Locale } from '../../../source/common/types/locales.types';

/*
 * Reading the toolbar selections from a story.
 *
 * preview.tsx declares themeName, localeName and osName as Storybook globals,
 * so a story reads them from the second argument its render function is given:
 *
 *   render: (_args, context) => <Thing currentTheme={currentThemeOf(context)} />
 *
 * The first argument is `context.args` and never carries them, which is worth
 * saying because both shapes were in use before the CSF conversion and only one
 * of them was ever right.
 *
 * Two of the three are mapped rather than passed through. A component wants the
 * theme id `dark-blue`, not the toolbar label `DarkBlue`, and the locale code
 * `en-US`, not `English`. Doing that here keeps the mapping in one place and
 * gives the toolbar one source for its option lists.
 */

type StoryContext = {
  globals?: {
    themeName?: string;
    localeName?: string;
    osName?: string;
  };
};

export const currentThemeOf = (context: StoryContext): string =>
  themesIds[context?.globals?.themeName ?? themeNames[0]];

export const localeOf = (context: StoryContext): Locale =>
  locales[context?.globals?.localeName ?? localeNames[0]];

export const osNameOf = (context: StoryContext): string =>
  context?.globals?.osName ?? osNames[0];
