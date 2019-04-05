// @flow
/* eslint-disable no-unused-vars */
import type { ThemeColors, ThemeFonts, CreateThemeParams } from '../types';

const createReactPolymorphTheme = ({
  colors,
  fonts,
}: {
  colors: ThemeColors,
  fonts: ThemeFonts,
}): Object => {
  // deconstruct colors
  const { primary, secondary, error } = colors;

  // create all react-polymorph css vars
  return {
    autocomplete: {
      '--rp-autocomplete-bg-color': `${primary.background}`,
      '--rp-autocomplete-border': `1px solid ${primary.border}`,
      '--rp-autocomplete-border-color-opened': `${primary.focus}`,
      '--rp-autocomplete-input-text-color': `${primary.text}`,
      '--rp-autocomplete-placeholder-color': `${primary.placeholder}`,
      '--rp-autocomplete-selected-word-box-bg-color': `${secondary.focus}`,
      '--rp-autocomplete-selected-word-text-color': `${secondary.text}`,
      '--rp-autocomplete-selected-words-font-family': `${fonts.regular}`,
    },
    bubble: {
      '--rp-bubble-bg-color': `${primary.background}`,
      '--rp-bubble-border-color': `${primary.border}`,
      '--rp-bubble-border-radius': '2px',
    },
    button: {
      '--rp-button-bg-color': `${secondary.background}`,
      '--rp-button-bg-color-active': `${secondary.active}`,
      '--rp-button-bg-color-disabled': `${secondary.disabled}`,
      '--rp-button-bg-color-hover': `${secondary.hover}`,
      '--rp-button-font-family': `${fonts.medium}`,
      '--rp-button-height': '50px',
      '--rp-button-line-height': '20px',
      '--rp-button-padding': '0',
      '--rp-button-text-color': `${secondary.text}`,
      '--rp-button-text-transform': 'none',
      '--rp-button-width': '360px',
    },
    checkbox: {
      '--rp-checkbox-border': `1px solid ${secondary.border}`,
      '--rp-checkbox-border-color-disabled': `${secondary.disabled}`,
      '--rp-checkbox-check-bg-color': `${secondary.border}`,
      '--rp-checkbox-label-text-color': `${primary.text}`,
      '--rp-checkbox-label-text-color-disabled': `${primary.disabled}`,
    },
    error: {
      '--rp-theme-color-error': `${error.regular}`,
    },
    fonts: {
      '--rp-theme-font-thin': `${fonts.thin}`,
      '--rp-theme-font-light': `${fonts.light}`,
      '--rp-theme-font-medium': `${fonts.medium}`,
      '--rp-theme-font-regular': `${fonts.regular}`,
      '--rp-theme-font-bold': `${fonts.bold}`,
    },
    formfield: {
      '--rp-formfield-bg-color-disabled': 'none',
      '--rp-formfield-label-text-color': `${primary.text}`,
      '--rp-formfield-label-text-color-disabled': `${primary.disabled}`,
      '--rp-formfield-error-text-color': `${error.regular}`,
      '--rp-formfield-error-text-opacity': '0.75',
    },
    input: {
      '--rp-input-bg-color': `${primary.background}`,
      '--rp-input-bg-color-disabled': `${primary.placeholder}`,
      '--rp-input-border-color': `${primary.border}`,
      '--rp-input-border-color-disabled': `${primary.placeholder}`,
      '--rp-input-border-color-errored': `${error.regular}`,
      '--rp-input-border-color-focus': `${primary.focus}`,
      '--rp-input-line-height': '22px',
      '--rp-input-padding': '12px 20px',
      '--rp-input-placeholder-color': `${primary.placeholder}`,
      '--rp-input-placeholder-color-disabled': `${primary.placeholder}`,
      '--rp-input-text-color': `${primary.text}`,
      '--rp-input-text-color-disabled': `${primary.placeholder}`,
    },
    modal: {
      '--rp-modal-bg-color': `${primary.background}`,
      '--rp-modal-max-height': '90%',
      '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
    },
    options: {
      '--rp-option-bg-color': `${primary.background}`,
      '--rp-option-bg-color-highlighted': `${primary.hover}`,
      '--rp-option-border-color': `${primary.border}`,
      '--rp-option-checkmark-color': `${primary.text}`,
      '--rp-option-line-height': '22px',
      '--rp-option-text-color': `${primary.text}`,
      '--rp-options-border-color': `${primary.border}`,
      '--rp-options-shadow': 'none',
    },
    select: {
      '--rp-select-arrow-bg-color': `${primary.border}`,
      '--rp-select-arrow-bg-color-open': `${primary.active}`,
      '--rp-select-input-bg-color': `${primary.background}`,
      '--rp-select-input-border-color': `${primary.border}`,
      '--rp-select-input-border-color-focus': `${primary.focus}`,
      '--rp-select-input-text-color': `${primary.text}`,
    },
    switch: {
      '--rp-switch-bg-color-off': `${secondary.border}`,
      '--rp-switch-bg-color-on': `${secondary.border}`,
      '--rp-switch-label-margin': '0 30px 0 0',
      '--rp-switch-label-opacity': '0.5',
      '--rp-switch-label-text-color': `${primary.text}`,
      '--rp-switch-label-width': '100%',
      '--rp-switch-opacity-off': '0.3',
      '--rp-switch-root-margin': '0 0 30px 0',
      '--rp-switch-thumb-bg-color': `${secondary.text}`,
    },
    textarea: {
      '--rp-textarea-bg-color': `${primary.background}`,
      '--rp-textarea-bg-color-disabled': `${primary.placeholder}`,
      '--rp-textarea-border': `1px solid ${primary.border}`,
      '--rp-textarea-border-color-disabled': `${primary.placeholder}`,
      '--rp-textarea-border-color-errored': `${error.regular}`,
      '--rp-textarea-border-color-focus': `${primary.focus}`,
      '--rp-textarea-border-radius': '2px',
      '--rp-textarea-line-height': '20px',
      '--rp-textarea-placeholder-color': `${primary.placeholder}`,
      '--rp-textarea-resize': 'none',
      '--rp-textarea-text-color': `${primary.text}`,
    },
  };
};

const createDaedalusTheme = ({
  colors,
  fonts,
}: {
  colors: ThemeColors,
  fonts: ThemeFonts,
}): Object => {
  // deconstruct colors
  const { primary, secondary, error } = colors;

  // create all daedalus css vars
  return {
    fonts: {
      '--font-ultralight': `${fonts.ultralight}`,
      '--font-thin': `${fonts.thin}`,
      '--font-light': `${fonts.light}`,
      '--font-regular': `${fonts.regular}`,
      '--font-medium': `${fonts.medium}`,
      '--font-semibold': `${fonts.semibold}`,
      '--font-bold': `${fonts.bold}`,
      '--font-heavy': `${fonts.heavy}`,
      '--font-black': `${fonts.black}`,
      '--font-mono': `${fonts.mono}`,
    },
    adaRedemption: {
      '--theme-ada-redemption-headline-color': `${primary.text}`,
      '--theme-ada-redemption-instructions-color': `${primary.text}`,
      '--theme-ada-redemption-success-overlay-border-color': `${
        secondary.text
      }`,
      '--theme-ada-redemption-success-overlay-message-color': `${
        secondary.text
      }`,
      '--theme-ada-redemption-success-overlay-button-text-color': `${
        secondary.text
      }`,
      '--theme-ada-redemption-success-overlay-button-text-color-hover': `${
        secondary.background
      }`,
    },
    input: {
      '--theme-input-hint-font': `${fonts.regular}`,
    },
  };
};

export const createTheme = ({
  colors,
  fonts,
  config,
}: CreateThemeParams): Object => {
  // create react-polymorph & daedalus themes
  const rpTheme = createReactPolymorphTheme({ colors, fonts });
  const daedalusTheme = createDaedalusTheme({ colors, fonts });

  // TODO: Use colors and fonts to create all css vars for daedalus components
};
