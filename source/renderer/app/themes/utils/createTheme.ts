import chroma from 'chroma-js';
import { isEmpty, has } from 'lodash';
import { createBackgroundShades, createErrorShades } from './createShades';
import type { ThemeColors, ThemeFonts, CreateThemeParams } from '../types';

export type PartialThemeParts = {
  colors: ThemeColors;
  fonts: ThemeFonts;
};
export const updateTheme = (
  existingTheme: Record<string, any>,
  themeUpdates: Record<string, any>
) => {
  let updateEntries = [];

  for (const key in themeUpdates) {
    if (key && !isEmpty(themeUpdates[key])) {
      updateEntries = [[key, themeUpdates[key]], ...updateEntries];
    }
  }

  const updatedTheme = Object.entries(themeUpdates).reduce(
    (theme: Record<string, any>, newEntry: [string, Record<string, any>]) => {
      const [keyName, newCSSVars] = newEntry;

      if (keyName && has(theme, keyName)) {
        return { ...theme, [keyName]: { ...theme[keyName], ...newCSSVars } };
      }

      if (keyName && !has(theme, keyName)) {
        return { ...theme, [keyName]: { ...newCSSVars } };
      }

      return theme;
    },
    { ...existingTheme }
  );
  return updatedTheme;
};
// assigns values to all react-polymorph CSS variables & returns them
export const createReactPolymorphTheme = (
  themeParts: PartialThemeParts
): Record<string, any> => {
  const { colors, fonts } = themeParts;
  const { background, border, error, focus, text } = colors;
  return {
    rpAutocomplete: {
      '--rp-autocomplete-bg-color': `${background.primary.lightest}`,
      '--rp-autocomplete-border': `1px solid ${border}`,
      '--rp-autocomplete-border-color-opened': `${focus}`,
      '--rp-autocomplete-input-text-color': `${text.primary}`,
      '--rp-autocomplete-placeholder-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--rp-autocomplete-selected-word-box-bg-color': `${background.secondary.light}`,
      '--rp-autocomplete-selected-word-dialog-box-bg-color': `${background.secondary.light}`,
      '--rp-autocomplete-selected-word-dialog-text-color': `${text.secondary}`,
      '--rp-autocomplete-selected-word-text-color': `${text.secondary}`,
      '--rp-autocomplete-selected-words-font-family': `${fonts.regular}`,
      '--rp-autocomplete-required-words-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--rp-autocomplete-required-words-offset-top': '2px',
    },
    rpBubble: {
      '--rp-bubble-bg-color': `${background.primary.lightest}`,
      '--rp-bubble-border-color': `${border}`,
      '--rp-bubble-border-radius': '5px',
      '--rp-bubble-box-shadow':
        '0 4px 16px 0 rgba(0, 0, 0, 0.12), 0 0 8px 0 rgba(0, 0, 0, 0.06)',
      '--rp-bubble-arrow-bg-color': `${background.primary.regular}`,
    },
    rpButton: {
      '--rp-button-bg-color': `${background.secondary.regular}`,
      '--rp-button-bg-color-active': `${background.secondary.darkest}`,
      '--rp-button-bg-color-disabled': `${background.secondary.lightest}`,
      '--rp-button-bg-color-hover': `${background.secondary.light}`,
      '--rp-button-font-family': `${fonts.medium}`,
      '--rp-button-font-size': '14px',
      '--rp-button-height': '50px',
      '--rp-button-line-height': '20px',
      '--rp-button-padding': '0',
      '--rp-button-text-color': `${text.secondary}`,
      '--rp-button-text-color-disabled': `${text.secondary}`,
      '--rp-button-text-transform': 'none',
      '--rp-button-width': '360px',
    },
    rpCheckbox: {
      '--rp-checkbox-border': `2px solid ${background.secondary.regular}`,
      '--rp-checkbox-border-color': `${border}`,
      '--rp-checkbox-border-color-disabled': `${chroma(
        background.secondary.regular
      ).alpha(0.4)}`,
      '--rp-checkbox-check-bg-color': `${background.secondary.regular}`,
      '--rp-checkbox-check-icon-color': `${text.secondary}`,
      '--rp-checkbox-label-text-color': `${text.primary}`,
      '--rp-checkbox-label-text-color-disabled': `${chroma(text.primary).alpha(
        0.3
      )}`,
      '--rp-checkbox-size': '22px',
    },
    rpColors: {
      '--rp-theme-color-error': `${error.regular}`,
    },
    rpFonts: {
      '--rp-theme-font-thin': `${fonts.thin}`,
      '--rp-theme-font-light': `${fonts.light}`,
      '--rp-theme-font-medium': `${fonts.medium}`,
      '--rp-theme-font-regular': `${fonts.regular}`,
      '--rp-theme-font-bold': `${fonts.bold}`,
    },
    rpFormfield: {
      '--rp-formfield-bg-color-disabled': 'none',
      '--rp-formfield-label-text-color': `${text.primary}`,
      '--rp-formfield-label-text-color-disabled': `${text.primary}`,
      '--rp-formfield-error-text-color': `${error.regular}`,
      '--rp-formfield-error-text-opacity': '0.75',
    },
    rpInput: {
      '--rp-input-bg-color': `${background.primary.lightest}`,
      '--rp-input-bg-color-disabled': `${background.primary.lighter}`,
      '--rp-input-border-color': `${border}`,
      '--rp-input-border-color-disabled': `${chroma(border).alpha(0.5)}`,
      '--rp-input-border-color-errored': `${error.regular}`,
      '--rp-input-border-color-focus': `${focus}`,
      '--rp-input-line-height': '22px',
      '--rp-input-padding': '12px 20px',
      '--rp-input-placeholder-color': `${chroma(text.primary).alpha(0.5)}`,
      '--rp-input-placeholder-color-disabled': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--rp-input-text-color': `${text.primary}`,
      '--rp-input-text-color-disabled': `${chroma(text.primary).alpha(0.5)}`,
    },
    rpLink: {
      '--rp-link-color': `${background.secondary.regular}`,
    },
    rpModal: {
      '--rp-modal-bg-color': `${background.primary.lightest}`,
      '--rp-modal-max-height': '90%',
      '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
    },
    rpOptions: {
      '--rp-option-bg-color': `${background.primary.lightest}`,
      '--rp-option-bg-color-highlighted': `${background.primary.regular}`,
      '--rp-option-border-color': `${border}`,
      '--rp-option-checkmark-color': `${text.primary}`,
      '--rp-option-line-height': '22px',
      '--rp-option-text-color': `${text.primary}`,
      '--rp-options-border-color': `${border}`,
      '--rp-options-shadow': 'none',
      '--rp-option-search-highlight-background-color': `rgba(242, 162, 24, 0.3)`,
    },
    rpPopOver: {
      '--rp-pop-over-bg-color': '',
      '--rp-pop-over-text-color': '',
    },
    rpRadio: {
      '--rp-radio-border': `2px solid ${background.secondary.regular}`,
      '--rp-radio-border-color': `${background.secondary.regular}`,
      '--rp-radio-color': '#ffffff',
      '--rp-radio-label-margin': '0 0 0 10px',
      '--rp-radio-label-text-color': `${text.primary}`,
      '--rp-radio-size': '22px',
    },
    rpScrollBar: {
      '--rp-scrollbar-thumb-bg-color:': 'rgba(255, 255, 255, 0.3)',
      '--rp-scrollbar-thumb-bg-color-active': 'rgba(255, 255, 255, 0.5)',
      '--rp-scrollbar-thumb-bg-color-hover': 'rgba(255, 255, 255, 0.5)',
    },
    rpSelect: {
      '--rp-select-arrow-bg-color': `${border}`,
      '--rp-select-arrow-bg-color-open': `${focus}`,
      '--rp-select-input-bg-color': `${background.primary.lightest}`,
      '--rp-select-input-border-color': `${border}`,
      '--rp-select-input-border-color-focus': `${focus}`,
      '--rp-select-input-text-color': `${text.primary}`,
      '--rp-select-input-placeholder-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
    },
    rpStepper: {
      '--rp-stepper-bullet-background-color-disabled': `${background.primary.lightest}`,
      '--rp-stepper-bullet-border-color': `${chroma(
        background.secondary.regular
      ).alpha(0.1)}`,
      '--rpstepper-bullet-height': '12px',
      '--rpstepper-bullet-width': '12px',
      '--rp-stepper-label-color': `${text.primary}`,
      '--rp-stepper-label-color-light': `${chroma(text.primary).alpha(0.3)}`,
      '--rp-stepper-main-color': `${background.secondary.regular}`,
      '--rp-stepper-main-color-light': `${chroma(
        background.secondary.regular
      ).alpha(0.1)}`,
      '--rpstepper-stepper-step-label-bottom-margin': '6px',
      '--rpstepper-steps-bar-color-disabled': `${chroma(
        background.secondary.regular
      ).alpha(0.1)}`,
      '--rpstepper-steps-bar-top-position': '6px',
    },
    rpSwitch: {
      '--rp-switch-bg-color-off': `${background.secondary.regular}`,
      '--rp-switch-bg-color-on': `${background.secondary.regular}`,
      '--rp-switch-label-margin': '0 30px 0 0',
      '--rp-switch-label-opacity': '1',
      '--rp-switch-label-text-color': `${text.primary}`,
      '--rp-switch-label-width': '100%',
      '--rp-switch-normal-border-radius': '2px',
      '--rp-switch-opacity-off': '0.3',
      '--rp-switch-root-margin': '0 0 30px 0',
      '--rp-switch-tiny-border-radius': '1px',
      '--rp-switch-thumb-bg-color': `${text.secondary}`,
      '--rp-switch-size': '22px',
    },
    rpTextArea: {
      '--rp-textarea-bg-color': `${background.primary.regular}`,
      '--rp-textarea-bg-color-disabled': `${chroma(
        background.primary.regular
      ).alpha(0.5)}`,
      '--rp-textarea-border': `1px solid ${border}`,
      '--rp-textarea-border-color-disabled': `${chroma(border).alpha(0.5)}`,
      '--rp-textarea-border-color-errored': `${error.regular}`,
      '--rp-textarea-border-color-focus': `${focus}`,
      '--rp-textarea-border-radius': '2px',
      '--rp-textarea-line-height': '20px',
      '--rp-textarea-placeholder-color': `${chroma(text.primary).alpha(0.5)}`,
      '--rp-textarea-resize': 'none',
      '--rp-textarea-text-color': `${text.primary}`,
    },
    rpTooltip: {
      '--rp-tooltip-bg-color': `${text.primary}`,
      '--rp-tooltip-text-color': `${text.secondary}`,
    },
    rpPasswordInput: {
      '--rp-password-input-error-score-color': '#ea4c5b',
      '--rp-password-input-error-bg-color': 'rgba(234, 76, 91, 0.4)',
      '--rp-password-input-warning-score-color': '#f2a218',
      '--rp-password-input-warning-bg-color': 'rgba(242, 162, 24, 0.4)',
      '--rp-password-input-success-score-color': '#2dc06c',
      '--rp-password-input-success-bg-color': 'rgba(45, 192, 108, 0.4)',
      '--rp-password-input-tooltip-border-radius': '5px',
      '--rp-password-input-tooltip-font-family': 'var(--rp-theme-font-medium)',
    },
  };
};
// assigns values to all Daedalus CSS variables & returns them
export const createDaedalusComponentsTheme = (
  themeParts: PartialThemeParts
): Record<string, any> => {
  const { colors, fonts } = themeParts;
  const { background, border, error, focus, text } = colors;
  return {
    aboutWindow: {
      '--theme-about-window-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-about-window-header-bottom-border-color': `${chroma(
        text.secondary
      ).alpha(0.3)}`,
      '--theme-about-window-daedalus-icon-color': `${text.secondary}`,
      '--theme-about-window-cardano-icon-color': `${text.secondary}`,
      '--theme-about-window-title-varsion-color': `${text.secondary}`,
      '--theme-about-window-title-stroke-color': `${text.secondary}`,
      '--theme-about-window-content-color': `${text.secondary}`,
      '--theme-about-window-content-text-color': `${text.secondary}`,
      '--theme-about-window-content-bottom-border-color': `${chroma(
        text.secondary
      ).alpha(0.3)}`,
      '--theme-about-window-icon-close-button-color': `${background.primary.lightest}`,
      '--theme-about-window-icon-close-hover-background': `${background.secondary.dark}`,
    },
    backToTopButton: {
      '--theme-back-to-top-button-background-color': `${text.primary}`,
      '--theme-back-to-top-button-text-color': `${text.secondary}`,
      '--theme-back-to-top-button-box-shadow-color': 'rgba(0, 0, 0, 0.36)',
    },
    automaticUpdate: {
      '--theme-automatic-update-overlay-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-automatic-update-overlay-button-background-color': `${background.secondary.dark}`,
      '--theme-automatic-update-overlay-button-background-color-hover': `${text.secondary}`,
      '--theme-automatic-update-overlay-button-icon-color': `${text.secondary}`,
      '--theme-automatic-update-overlay-button-icon-color-hover': `${background.secondary.regular}`,
      '--theme-automatic-update-overlay-button-text-color-hover': `${background.secondary.regular}`,
      '--theme-automatic-update-overlay-button-border-color': `${text.secondary}`,
      '--theme-automatic-update-overlay-text-color': `${chroma(
        text.secondary
      ).alpha(0.7)}`,
      '--theme-automatic-update-overlay-text-highlight-color': `${text.secondary}`,
      '--theme-automatic-update-overlay-title-text-color': `${text.secondary}`,
      '--theme-automatic-update-overlay-button-label-color': `${text.secondary}`,
      '--theme-automatic-update-overlay-button-label-color-hover': `${text.secondary}`,
      '--theme-automatic-update-overlay-button-label-color-light': `${chroma(
        text.secondary
      ).alpha(0.8)}`,
      '--theme-automatic-update-overlay-close-button-color': `${background.primary.lightest}`,
      '--theme-automatic-update-overlay-close-button-hover-background': `${background.secondary.dark}`,
    },
    body: {
      '--theme-main-body-background-color': `${background.primary.regular}`,
      '--theme-main-body-messages-color': `${text.primary}`,
    },
    borderedBox: {
      '--theme-bordered-box-background-color': `${background.primary.lightest}`,
      '--theme-bordered-box-border': `1px solid ${border}`,
      '--theme-bordered-box-text-color': `${text.primary}`,
    },
    button: {
      '--theme-button-spinner-color': `${background.primary.lightest}`,
      '--theme-label-button-color': `${text.primary}`,
    },
    buttonAttention: {
      '--theme-button-attention-background-color': `${error.regular}`,
      '--theme-button-attention-background-color-hover': `${error.light}`,
      '--theme-button-attention-background-color-active': `${error.darkest}`,
      '--theme-button-attention-background-color-disabled': `${error.regular}`,
      '--theme-button-attention-delete-text-color': `${background.primary.regular}`,
      '--theme-button-attention-delete-text-color-disabled': `${background.primary.regular}`,
      '--theme-button-attention-text-color': `${text.secondary}`,
      '--theme-button-attention-text-color-disabled': `${text.secondary}`,
      '--theme-button-attention-outline-color': `${error.lighter}`,
    },
    buttonDisclaimer: {
      '--theme-button-disclaimer-background-color': '#ab1700',
      '--theme-button-disclaimer-background-color-hover': '#fafbfc',
      '--theme-button-disclaimer-background-color-active': '#fafbfc',
      '--theme-button-disclaimer-background-color-disabled':
        'rgba(171, 23, 0, .3)',
      '--theme-button-disclaimer-text-color-disabled':
        'rgba(250, 251, 252, .3)',
      '--theme-button-disclaimer-text-color': '#fafbfc',
      '--theme-button-disclaimer-outline-color': 'rgba(250, 251, 252, .3)',
      '--theme-button-disclaimer-border-color': '#fafbfc',
      '--theme-button-disclaimer-border-color-disabled':
        'rgba(250, 251, 252, .3)',
    },
    buttonFlat: {
      '--theme-button-flat-background-color': `${background.primary.light}`,
      '--theme-button-flat-background-color-hover': `${background.primary.lighter}`,
      '--theme-button-flat-background-color-active': `${background.primary.regular}`,
      '--theme-button-flat-background-color-disabled': `${background.primary.lighter}`,
      '--theme-button-flat-text-color-disabled': `${text.primary}`,
      '--theme-button-flat-text-color': `${text.primary}`,
      '--theme-button-flat-outline-color': `${background.primary.dark}`,
    },
    buttonPrimary: {
      '--theme-button-primary-background-color': `${background.secondary.regular}`,
      '--theme-button-primary-background-color-hover': `${background.secondary.light}`,
      '--theme-button-primary-background-color-active': `${background.secondary.darkest}`,
      '--theme-button-primary-background-color-disabled': `${background.secondary.lightest}`,
      '--theme-button-primary-text-color-disabled': `${text.secondary}`,
      '--theme-button-primary-text-color': `${text.secondary}`,
      '--theme-button-primary-outline-color': `${background.secondary.light}`,
    },
    connecting: {
      '--theme-connecting-background-color': `${background.secondary.regular}`,
      '--theme-connecting-background-color1': `${chroma(
        background.secondary.regular
      ).alpha(1)}`,
      '--theme-connecting-background-color2': `${chroma(
        background.secondary.regular
      ).alpha(0.91)}`,
      '--theme-connecting-background-color3': `${chroma(
        background.secondary.regular
      ).alpha(0.31)}`,
      '--theme-connecting-background-color4': `${chroma(
        background.secondary.regular
      ).alpha(0)}`,
      '--theme-connecting-background-color5': `${chroma(
        background.secondary.regular
      ).alpha(0)}`,
      '--theme-connecting-background-color6': `${chroma(
        background.secondary.regular
      ).alpha(0.31)}`,
      '--theme-connecting-background-color7': `${chroma(
        background.secondary.regular
      ).alpha(0.91)}`,
      '--theme-connecting-background-color8': `${chroma(
        background.secondary.regular
      ).alpha(1)}`,
      '--theme-connecting-text-color': `${text.secondary}`,
    },
    dataMigration: {
      '--theme-data-migration-layer-background-color': `${background.secondary.regular}`,
      '--theme-data-migration-layer-box-shadow-color': `${background.secondary.regular}`,
      '--theme-data-migration-layer-button-background-color': `${background.secondary.regular}`,
      '--theme-data-migration-layer-button-background-color-hover': `${background.primary.regular}`,
      '--theme-data-migration-layer-button-border-color': `${text.secondary}`,
      '--theme-data-migration-layer-button-label-color': `${text.secondary}`,
      '--theme-data-migration-layer-text-color': `${text.secondary}`,
      '--theme-data-migration-layer-text-color-hover': `${text.primary}`,
      '--theme-data-migration-layer-text-opacity-color': `${text.secondary}`,
    },
    delegationSetupWizard: {
      '--theme-delegation-steps-activation-steps-indicator-color': `${text.primary}`,
      '--theme-delegation-steps-success-description-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-delegation-steps-choose-stake-pool-checkmark-icon-color': `${border}`,
      '--theme-delegation-steps-choose-stake-pool-delegated-pools-label-color': `${text.primary}`,
      '--theme-delegation-steps-choose-stake-pool-ticker-color': `${border}`,
      '--theme-delegation-steps-choose-stake-pool-select-box-placeholder-color': `${border}`,
      '--theme-delegation-steps-choose-stake-pool-selected-checkmark-icon-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-selected-ticker-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-thumb-background-color': `${background.primary.lightest}`,
      '--theme-delegation-steps-choose-stake-pool-thumb-border-color': `${chroma(
        border
      ).alpha(0.2)}`,
      '--theme-delegation-steps-choose-stake-pool-title-color': `${text.primary}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-arrow-color': `${chroma(
        text.primary
      ).alpha(0.9)}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-background-color': `${chroma(
        text.primary
      ).alpha(0.9)}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-box-shadow':
        '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
      '--theme-delegation-steps-choose-stake-pool-tooltip-description-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-ticker-color': `${chroma(
        text.primary
      ).alpha(0.6)}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-table-label-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-table-value-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-title-color': `${text.secondary}`,
      '--theme-delegation-steps-choose-stake-pool-tooltip-url-color': '#85b6f9',
      '--theme-delegation-steps-choose-wallet-custom-value-color': `${text.primary}`,
      '--theme-delegation-steps-choose-wallet-description-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-delegation-steps-choose-wallet-description-highlighted-color': `${text.primary}`,
      '--theme-delegation-steps-choose-wallet-error-message-color': `${error.regular}`,
      '--theme-delegation-steps-choose-wallet-error-message-light-color': `${chroma(
        error.regular
      ).alpha(0.7)}`,
      '--theme-delegation-steps-choose-wallet-error-select-options-color': `${text.primary}`,
      '--theme-delegation-steps-choose-wallet-steps-indicator-color': `${text.primary}`,
      '--theme-delegation-steps-confirmation-steps-indicator-color': `${text.primary}`,
      '--theme-delegation-steps-confirmation-description-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-delegation-steps-confirmation-fees-label-color': `${text.primary}`,
      '--theme-delegation-steps-confirmation-fees-amount-color': `${error.regular}`,
      '--theme-delegation-steps-intro-content-text-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-delegation-steps-intro-divider-border-color': `${border}`,
      '--theme-delegation-steps-intro-link-color': `${background.secondary.regular}`,
      '--theme-delegation-steps-intro-list-label-color': `${text.primary}`,
      '--theme-delegation-steps-intro-list-numbers-color': `${text.primary}`,
      '--theme-delegation-steps-intro-list-optional-label-color': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-delegation-steps-not-available-description-text-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-delegation-steps-not-available-description-highlight-text-color': `${text.primary}`,
      '--theme-delegation-steps-not-available-icon-color': `${text.primary}`,
      '--theme-delegation-steps-not-available-subtitle-text-color':
        text.primary,
    },
    dialog: {
      '--theme-dialog-choice-tabs-text-color': `${text.primary}`,
      '--theme-dialog-choice-tabs-text-color-active': `${text.primary}`,
      '--theme-dialog-choice-tabs-bottom-border-color-active': `${focus}`,
      '--theme-dialog-big-button-background-color': `${background.primary.lightest}`,
      '--theme-dialog-big-button-border-color': `${border}`,
      '--theme-dialog-big-button-label-color': `${text.primary}`,
      '--theme-dialog-big-button-description-color': `${text.primary}`,
      '--theme-dialog-set-wallet-password-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.96)}`,
      '--theme-dialog-set-wallet-password-box-shadow': `${chroma(
        background.primary.regular
      ).alpha(0.25)}`,
      '--theme-dialog-set-wallet-password-message-color': `${text.primary}`,
      '--theme-dialog-set-wallet-password-title-color': `${text.primary}`,
      '--theme-dialog-set-wallet-password-button-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.15)}`,
      '--theme-dialog-set-wallet-password-button-border-color': `${text.primary}`,
      '--theme-dialog-set-wallet-password-button-color': `${text.primary}`,
      '--theme-dialog-title-color': `${text.primary}`,
      '--theme-dialog-text-color': `${text.primary}`,
      '--theme-dialog-border-color': `${border}`,
      '--theme-dialog-fullsize-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-dialog-fullsize-background-color-opaque': `${background.secondary.regular}`,
      '--theme-dialog-fullsize-button-background-color': `${text.secondary}`,
      '--theme-dialog-fullsize-button-background-color-hover': `${background.secondary.dark}`,
      '--theme-dialog-fullsize-button-border-color': `${text.secondary}`,
      '--theme-dialog-fullsize-button-icon-color': `${text.secondary}`,
      '--theme-dialog-fullsize-button-icon-color-hover': `${background.secondary.regular}`,
      '--theme-dialog-fullsize-button-label-color': `${text.secondary}`,
      '--theme-dialog-fullsize-button-label-color-hover': `${text.secondary}`,
      '--theme-dialog-fullsize-button-text-color-hover': `${background.secondary.regular}`,
      '--theme-dialog-fullsize-field-background-color': 'rgba(0,0,0, 0.1)',
      '--theme-dialog-fullsize-text-color': `${chroma(text.secondary).alpha(
        0.7
      )}`,
      '--theme-dialog-fullsize-text-highlight-color': `${text.secondary}`,
      '--theme-dialog-fullsize-title-text-color': `${text.secondary}`,
    },
    errors: {
      '--theme-color-error': `${error.regular}`,
    },
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
    hardwareWallet: {
      '--theme-hardware-wallet-status-background-color': '#fafbfc',
      '--theme-hardware-wallet-status-border-color': '#c6cdd6',
      '--theme-hardware-wallet-status-color': '#5e6066',
      '--theme-hardware-wallet-status-ready-color': '#2dc06c',
      '--theme-hardware-wallet-title-color': '#5e6066',
      '--theme-hardware-wallet-message-color': '#5e6066',
      '--theme-hardware-wallet-status-passphrase-label-color': `${chroma(
        text.secondary
      ).alpha(0.5)}`,
    },
    icon: {
      '--theme-icon-nav-color': `${text.secondary}`,
      '--theme-icon-nav-color-active': `${text.primary}`,
      '--theme-icon-sidebar-color': `${background.primary.regular}`,
      '--theme-icon-toggle-menu-color': `${background.primary.regular}`,
      '--theme-icon-node-update-notification-arrow-color': `${text.primary}`,
      '--theme-icon-add-wallet-from-sidebar-color': `${text.secondary}`,
      '--theme-icon-ada-summary-wallet-amount-symbol-color': `${text.primary}`,
      '--theme-icon-add-wallet-dialog-big-button-color': `${background.primary.darker}`,
      '--theme-icon-back-button-color': `${text.primary}`,
      '--theme-icon-close-button-color': `${text.primary}`,
      '--theme-icon-connecting-ada-api-logo-color': `${text.secondary}`,
      '--theme-icon-connecting-ada-logo-color': `${text.secondary}`,
      '--theme-icon-connecting-daedalus-logo-color': `${text.secondary}`,
      '--theme-icon-copy-address-color': `${text.primary}`,
      '--theme-icon-delegation-center-no-wallets': `${text.primary}`,
      '--theme-icon-file-upload-color': `${text.primary}`,
      '--theme-icon-syncing-ada-api-logo-color': `${text.primary}`,
      '--theme-icon-syncing-ada-logo-color': `${text.primary}`,
      '--theme-icon-syncing-daedalus-logo-color': `${text.primary}`,
      '--theme-icon-transactions-ada-symbol-color': `${text.primary}`,
      '--theme-icon-transaction-type-color': '#fafbfc',
    },
    input: {
      '--theme-input-background-color': `${background.primary.lightest}`,
      '--theme-input-border-color': `${border}`,
      '--theme-input-focus-border-color': `${focus}`,
      '--theme-input-hint-font': `${fonts.regular}`,
      '--theme-input-label-color': `${text.primary}`,
      '--theme-input-placeholder-color': `${chroma(text.primary).alpha(0.5)}`,
      '--theme-input-remove-color-light': `${error.regular}`,
      '--theme-input-right-floating-text-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--theme-input-right-floating-text-success-color': `${chroma(
        text.primary
      ).alpha(1)}`,
      '--theme-input-text-color': `${text.primary}`,
    },
    link: {
      '--theme-link-main-color': `${background.secondary.regular}`,
    },
    loading: {
      '--theme-loading-background-color': `${background.primary.regular}`,
      '--theme-loading-no-disk-space-background-color': `${background.primary.regular}`,
      '--theme-loading-no-disk-space-text-color': `${text.primary}`,
      '--theme-loading-no-disk-space-attention-icon-color': `${text.primary}`,
      '--theme-loading-status-icons-on-color': '#2dc06c',
      '--theme-loading-status-icons-off-color': '#ea4c5b',
      '--theme-loading-status-icons-unloaded-loading-color': `${text.primary}`,
      '--theme-loading-status-icons-unloaded-syncing-color': `${text.primary}`,
      '--theme-loading-status-icons-tooltip-color': `${text.primary}`,
      '--theme-loading-spinner-color': `${text.primary}`,
      '--theme-loading-spinner-medium-color': '#fff',
    },
    appUpdateOverlay: {
      '--theme-app-update-overlay-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-app-update-overlay-content-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-app-update-overlay-button-background-color': `${background.secondary.dark}`,
      '--theme-app-update-overlay-button-background-color-hover': `${text.secondary}`,
      '--theme-app-update-overlay-button-icon-color': `${text.secondary}`,
      '--theme-app-update-overlay-button-icon-color-hover': `${background.secondary.regular}`,
      '--theme-app-update-overlay-button-text-color-hover': `${background.secondary.regular}`,
      '--theme-app-update-overlay-button-border-color': `${text.secondary}`,
      '--theme-app-update-overlay-opacity-text-color': `${chroma(
        text.secondary
      ).alpha(0.7)}`,
      '--theme-app-update-overlay-text-highlight-color': `${text.secondary}`,
      '--theme-app-update-overlay-text-color': `${text.secondary}`,
      '--theme-app-update-overlay-manual-update-text-color': `${error.regular}`,
      '--theme-app-update-overlay-button-label-color': `${text.secondary}`,
      '--theme-app-update-overlay-button-label-color-hover': `${text.secondary}`,
    },
    mnemonic: {
      '--theme-backup-mnemonic-background-color': `${background.primary.light}`,
      '--theme-mnemonic-background-color': `${background.primary.regular}`,
    },
    modal: {
      '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
    },
    navDropdown: {
      '--theme-nav-dropdown-item-text-color': `${text.primary}`,
      '--theme-nav-dropdown-item-background-color': `${background.primary.lightest}`,
      '--theme-nav-dropdown-item-background-color-hover': `${chroma(
        background.primary.regular
      ).alpha(0.07)}`,
      '--theme-nav-dropdown-item-color-hover': `${text.primary}`,
    },
    navItem: {
      '--theme-nav-item-background-color': `${background.secondary.darkest}`,
      '--theme-nav-item-background-color-active': `${background.primary.lightest}`,
      '--theme-nav-item-background-color-hover': `${background.secondary.darker}`,
      '--theme-nav-item-text-color': `${text.secondary}`,
      '--theme-nav-item-text-color-active': `${text.primary}`,
    },
    network: {
      '--theme-network-window-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-network-window-text-color': `${text.secondary}`,
      '--theme-network-window-icon-close-hover-background': `${background.secondary.dark}`,
      '--theme-network-window-red-color': '#f06e05',
      '--theme-network-window-green-color': '#05f079',
      '--theme-network-window-white-color': `${text.secondary}`,
      '--theme-network-window-transparent-color': 'transparent',
      '--theme-network-window-border-color': `${chroma(text.secondary).alpha(
        0.7
      )}`,
      '--theme-network-window-button-text-color': `${text.secondary}`,
      '--theme-network-window-button-background-color': `${chroma(
        background.secondary.lightest
      ).alpha(0.4)}`,
      '--theme-network-window-button-background-color-hover': `${chroma(
        background.secondary.lightest
      ).alpha(0.6)}`,
      '--theme-network-window-button-background-color-active': `${chroma(
        background.secondary.lightest
      ).alpha(0.8)}`,
    },
    newsFeed: {
      '--theme-news-feed-background-color': '#233856',
      '--theme-news-feed-badge-background-color': '#ea4c5b',
      '--theme-news-feed-badge-text-color': '#ffffff',
      '--theme-news-feed-box-shadow-color': '-5px 0 20px 0 rgba(0, 0, 0, 0.25)',
      '--theme-news-feed-header-background-color': '#1e304a',
      '--theme-news-feed-header-box-shadow-color':
        '0 10px 10px -5px rgba(0, 0, 0, 0.25)',
      '--theme-news-feed-header-title-color': '#fafbfc',
      '--theme-news-feed-icon-close-button-color': '#fff',
      '--theme-news-feed-icon-close-hover-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-feed-icon-color': '#fafbfc',
      '--theme-news-feed-icon-color-connecting-screen': '#fafbfc',
      '--theme-news-feed-icon-color-syncing-screen': '#5e6066',
      '--theme-news-feed-icon-green-dot-background-color': '#2DC06C',
      '--theme-news-feed-icon-red-dot-background-color': '#ea4c5b',
      '--theme-news-feed-icon-toggle-hover-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-feed-no-fetch-color': '#fafbfc',
      '--theme-news-feed-incident-overlay-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-news-feed-incident-overlay-text-color': `${text.primary}`,
      '--theme-news-feed-incident-overlay-button-color': `${text.primary}`,
      '--theme-news-feed-incident-overlay-scrollbar-thumb-background': `${chroma(
        text.primary
      ).alpha(0.3)}`,
      '--theme-news-feed-incident-overlay-scrollbar-thumb-background-hover': `${chroma(
        text.primary
      ).alpha(0.3)}`,
      '--theme-news-feed-incident-overlay-content-list-color': `${chroma(
        text.primary
      ).alpha(0.7)}`,
      '--theme-news-feed-incident-overlay-content-background':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-feed-incident-overlay-button-background':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-feed-incident-overlay-button-color-hover': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-news-feed-incident-grey-overlay-background-color':
        'rgba(68, 68, 68, 0.98)',
      '--theme-news-feed-incident-grey-overlay-text-color': `${text.primary}`,
      '--theme-news-feed-incident-grey-overlay-button-color': `${text.primary}`,
      '--theme-news-feed-incident-grey-overlay-scrollbar-thumb-background': `${chroma(
        text.primary
      ).alpha(0.3)}`,
      '--theme-news-feed-incident-grey-overlay-scrollbar-thumb-background-hover': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-news-feed-incident-grey-overlay-content-list-color': `${chroma(
        text.primary
      ).alpha(0.7)}`,
      '--theme-news-feed-incident-red-overlay-background-color':
        'rgba(171, 23, 0, 0.98)',
      '--theme-news-feed-incident-red-overlay-text-color': `${text.primary}`,
      '--theme-news-feed-incident-red-overlay-button-color': `${text.primary}`,
      '--theme-news-feed-incident-red-overlay-scrollbar-thumb-background': `${chroma(
        text.primary
      ).alpha(0.3)}`,
      '--theme-news-feed-incident-red-overlay-scrollbar-thumb-background-hover': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-news-feed-incident-red-overlay-content-list-color': `${chroma(
        text.primary
      ).alpha(0.7)}`,
    },
    newsItem: {
      '--theme-news-item-action-button-background-color': 'rgba(0, 0, 0, 0.1)',
      '--theme-news-item-action-button-background-color-hover': '#29b595',
      '--theme-news-item-action-button-border-color': '#fafbfc',
      '--theme-news-item-action-button-color': '#fafbfc',
      '--theme-news-item-action-button-color-hover': '#ffffff',
      '--theme-news-item-alert-background-color': 'rgba(242, 162, 24, 0.5)',
      '--theme-news-item-announcement-background-color':
        'rgba(234, 76, 91, 0.25)',
      '--theme-news-item-badge-green-color': '#2DC06C',
      '--theme-news-item-badge-red-color': '#ea4c5b',
      '--theme-news-item-content-link-color': '#fafbfc',
      '--theme-news-item-info-background-color': 'rgba(0, 0, 0, 0.1)',
      '--theme-news-item-title-color': '#fafbfc',
    },
    newsUpdateOverlay: {
      '--theme-news-overlay-update-background-color': 'rgba(36, 62, 98, 0.96)',
      '--theme-news-overlay-update-text-color': '#fafbfc',
      '--theme-news-overlay-update-subtitle-text-color':
        'rgba(250, 251, 252, 0.7)',
      '--theme-news-overlay-update-content-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-overlay-update-content-scroll-background-color': `rgba(255, 255, 255, 0.3)`,
      '--theme-news-overlay-update-content-scroll-hover-background-color': `rgba(255, 255, 255, 0.5)`,
      '--theme-news-overlay-update-button-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-news-overlay-update-button-text-color': '#fafbfc',
      '--theme-news-overlay-update-button-hover-text-color': '#5e6066',
    },
    appUpdate: {
      '--theme-node-update-background-color': `${background.primary.regular}`,
      '--theme-node-update-title-color': `${text.primary}`,
      '--theme-node-update-message-color': `${text.primary}`,
      '--theme-node-sync-info-message-background-color': `${background.primary.regular}`,
      '--theme-node-sync-info-message-text-color': `${text.primary}`,
      '--theme-node-sync-icon-color': `${text.primary}`,
      '--theme-node-update-accept-button-background-color': `${background.secondary.regular}`,
      '--theme-node-update-accept-button-background-color-hover': `${background.secondary.light}`,
      '--theme-node-update-accept-button-background-color-active': `${background.secondary.darkest}`,
      '--theme-node-update-deny-button-background-color': `${background.secondary.regular}`,
      '--theme-node-update-deny-button-background-color-hover': `${background.secondary.light}`,
      '--theme-node-update-deny-button-background-color-active': `${background.secondary.darkest}`,
      '--theme-node-update-deny-button-text-color': `${text.secondary}`,
      '--theme-node-update-button-text-color': `${text.secondary}`,
    },
    notification: {
      '--theme-notification-message-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.95)}`,
      '--theme-notification-message-text-color': `${text.secondary}`,
      '--theme-notification-message-checkmark-icon-color': `${text.secondary}`,
      '--theme-notification-message-close-icon-color': `${text.secondary}`,
      '--theme-legacy-badge-background-color': `${error.dark}`,
      '--theme-legacy-notification-background-color': '#ab2712',
      '--theme-legacy-notification-learn-more-button-text-color': `${text.secondary}`,
      '--theme-legacy-notification-learn-more-button-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.2)}`,
      '--theme-legacy-notification-learn-more-button-background-color-hover': `${chroma(
        background.primary.regular
      ).alpha(0.3)}`,
      '--theme-legacy-notification-learn-more-button-background-color-active': `${chroma(
        background.primary.regular
      ).alpha(0.4)}`,
      '--theme-legacy-notification-move-button-text-color': '#ab2712',
      '--theme-legacy-notification-move-button-background-color': `${text.secondary}`,
      '--theme-legacy-notification-move-button-background-color-hover': `${chroma(
        background.primary.regular
      ).alpha(0.9)}`,
      '--theme-legacy-notification-move-button-background-color-active': `${chroma(
        background.primary.regular
      ).alpha(0.8)}`,
      '--theme-legacy-notification-title-color': `${text.secondary}`,
      '--theme-legacy-notification-description-color': `${text.secondary}`,
    },
    paperWallet: {
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': `${background.secondary.regular}`,
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': `${text.secondary}`,
    },
    progressBar: {
      '--theme-progress-bar-background-color': `${chroma(
        background.primary.light
      ).alpha(0.3)}`,
      '--theme-progress-bar-foreground-color': `${chroma(
        background.primary.light
      ).alpha(0.7)}`,
      '--theme-progress-bar-large-progress-stripe1': '#e0e5eb',
      '--theme-progress-bar-large-progress-stripe2': '#fafbfc',
      '--theme-progress-bar-large-progress-dark-stripe1': '#e0e5eb',
      '--theme-progress-bar-large-progress-dark-stripe2': '#fafbfc',
      '--theme-progress-bar-large-progress-light-stripe-1': `${background.secondary.dark}`,
      '--theme-progress-bar-large-progress-light-stripe-2-background-color': `${background.secondary.regular}`,
      '--theme-progress-bar-large-background-color': 'rgba(0, 0, 0, 0.1)',
    },
    receiveQRCode: {
      '--theme-receive-qr-code-background-color': 'transparent',
      '--theme-receive-qr-code-foreground-color': '#000',
    },
    recoveryPhrase: {
      '--theme-recovery-phrase-attention-background-color': `${background.primary.light}`,
      '--theme-recovery-phrase-normal-background-color':
        'rgba(32, 34, 37, 0.05)',
      '--theme-recovery-phrase-normal-border-color': 'rgba(68, 91, 124, .07)',
      '--theme-recovery-phrase-paper-wallet-background-color': `${background.primary.light}`,
      '--theme-recovery-phrase-paper-wallet-text-color': `${text.primary}`,
      '--theme-recovery-phrase-warning-background-color':
        'rgba(68, 91, 124, 0.05)',
    },
    walletImportDialog: {
      '--theme-wallet-import-background-color': `${background.secondary.regular}`,
      '--theme-wallet-import-button-background-color': `${background.secondary.regular}`,
      '--theme-wallet-import-description-close-color': `${text.primary}`,
      '--theme-wallet-import-description-close-hover-background': `${chroma(
        background.secondary.regular
      ).alpha(0.16)}`,
      '--theme-wallet-import-description-color': `${chroma(text.primary).alpha(
        0.7
      )}`,
      '--theme-wallet-import-description-bold-color': `${text.primary}`,
      '--theme-wallet-import-error-color': `${error.regular}`,
      '--theme-wallet-import-input-background-color': `${background.secondary.dark}`,
      '--theme-wallet-import-input-border-color': `${chroma(text.primary).alpha(
        0.3
      )}`,
      '--theme-wallet-import-title-color': `${text.primary}`,
      '--theme-wallet-import-stateFolder-border-color': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-wallet-import-stateFolder-button-background-color': `${chroma(
        text.primary
      ).alpha(0.1)}`,
      '--theme-wallet-import-stateFolder-button-background-color-hover': `${chroma(
        text.primary
      ).alpha(0.05)}`,
      '--theme-wallet-import-stateFolder-button-background-color-active': `${chroma(
        text.primary
      ).alpha(0.12)}`,
      '--theme-wallet-import-checkbox-border': '2px solid #ffffff',
      '--theme-wallet-import-checkbox-border-color': `${text.primary}`,
      '--theme-wallet-import-checkbox-border-color-disabled': `${text.primary}`,
      '--theme-wallet-import-checkbox-check-bg-color': `${text.primary}`,
      '--theme-wallet-import-checkbox-check-color': `${text.secondary}`,
      '--theme-wallet-import-stateFolder-label-color': `${text.primary}`,
    },
    reportIssue: {
      '--theme-report-issue-button-background-color': `${background.secondary.regular}`,
      '--theme-report-issue-button-background-color-hover': `${background.secondary.light}`,
      '--theme-report-issue-button-background-color-active': `${background.secondary.darkest}`,
      '--theme-report-issue-connecting-background-color': `${background.primary.regular}`,
      '--theme-report-issue-connecting-text-color': `${text.primary}`,
      '--theme-report-issue-icon-color': `${background.primary.regular}`,
    },
    scrollbar: {
      '--theme-scrollbar-thumb-background': `${background.primary.ultralight}`,
    },
    sendConfirmation: {
      '--theme-send-confirmation-dialog-send-values-color': `${error.regular}`,
    },
    settings: {
      '--theme-settings-body-background-color': `${background.primary.regular}`,
      '--theme-settings-delete-button-legacy-background-color': `${background.primary.light}`,
      '--theme-settings-delete-button-legacy-background-color-hover': `${background.primary.lighter}`,
      '--theme-settings-pane-background-color': `${background.primary.lightest}`,
      '--theme-settings-pane-border': `1px solid ${border}`,
      '--theme-settings-menu-box-background-color': `${background.primary.lightest}`,
      '--theme-settings-menu-box-border': `1px solid ${border}`,
      '--theme-settings-menu-item-text-color': `${text.primary}`,
      '--theme-settings-menu-item-text-color-active': `${text.primary}`,
      '--theme-settings-menu-item-text-color-disabled': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-settings-menu-item-background-color-active': `${background.primary.regular}`,
      '--theme-settings-menu-item-left-border-color-active': `${background.secondary.regular}`,
      '--theme-settings-theme-select-title-color': `${text.primary}`,
      '--theme-settings-theme-select-border-color': `${border}`,
      '--theme-settings-undelegate-wallet-divider-border-color': `${border}`,
      '--theme-settings-undelegate-wallet-deposit-amount-color': `${background.primary.regular}`,
      '--theme-settings-undelegate-wallet-fees-amount-color': `${error.regular}`,
    },
    sidebar: {
      '--theme-sidebar-background-color': `${background.secondary.regular}`,
      '--theme-sidebar-category-background-color-hover': `${background.secondary.dark}`,
      '--theme-sidebar-category-background-color-active': `${background.secondary.darker}`,
      '--theme-sidebar-category-text-color': `${text.secondary}`,
      '--theme-sidebar-category-networkInfo-background-color': '#eb2256',
      '--theme-sidebar-category-networkInfo-text-color': '#121326',
      '--theme-sidebar-layout-shadow-color': 'rgba(0, 0, 0, 0.25)',
      '--theme-sidebar-layout-topbar-shadow-color': 'rgba(0, 0, 0, 0.25)',
      '--theme-sidebar-menu-background-color': `${background.secondary.darker}`,
      '--theme-sidebar-menu-item-background-color-hover': `${background.secondary.darkest}`,
      '--theme-sidebar-menu-item-background-color-active': `${background.secondary.darkest}`,
      // rename to active wallet?
      '--theme-sidebar-menu-item-wallet-name-color': `${text.secondary}`,
      '--theme-sidebar-menu-item-wallet-info-color': `${text.secondary}`,
      '--theme-sidebar-menu-add-button-background-color': `${background.secondary.darkest}`,
      '--theme-sidebar-menu-add-button-background-color-active': `${chroma(
        background.secondary.darkest
      ).alpha(0.66)}`,
      '--theme-sidebar-menu-add-button-background-color-hover': `${chroma(
        background.secondary.darkest
      ).alpha(0.66)}`,
      '--theme-sidebar-menu-add-button-text-color': `${text.secondary}`,
      '--theme-sidebar-wallets-scrollbar-background-color':
        'rgba(255, 255, 255, 0.1)',
      '--theme-sidebar-wallets-scrollbar-background-color-active':
        'rgba(255, 255, 255, 0.3)',
      '--theme-sidebar-wallets-scrollbar-background-color-hover':
        'rgba(255, 255, 255, 0.3)',
    },
    splash: {
      '--theme-splash-network-background-color': 'rgba(36, 62, 98, 0.96)',
      '--theme-splash-network-background-color1': 'rgba(18, 19, 38, 1)',
      '--theme-splash-network-background-color2': 'rgba(18, 19, 38, 0.91)',
      '--theme-splash-network-background-color3': 'rgba(18, 19, 38, 0.31)',
      '--theme-splash-network-background-color4': 'rgba(18, 19, 38, 0)',
      '--theme-splash-network-background-color5': 'rgba(18, 19, 38, 0)',
      '--theme-splash-network-background-color6': 'rgba(18, 19, 38, 0.31)',
      '--theme-splash-network-background-color7': 'rgba(18, 19, 38, 0.91)',
      '--theme-splash-network-background-color8': 'rgba(18, 19, 38, 1)',
      '--theme-splash-network-logo-fill-color': '#fff',
      '--theme-splash-network-title-color': '#fafbfc',
      '--theme-splash-network-subTitle1-color': `${background.secondary}`,
      '--theme-splash-network-subTitle2-color': `${background.secondary}`,
      '--theme-splash-network-scrollbar-thumb-background': `${chroma(
        text.secondary
      ).alpha(0.3)}`,
      '--theme-splash-network-scrollbar-thumb-background-hover': `${chroma(
        text.secondary
      ).alpha(0.5)}`,
      '--theme-splash-network-description-background-color':
        'rgba(255, 255, 255, 0.1)',
      '--theme-splash-network-description-color': `${text.primary}`,
      '--theme-splash-network-learn-more-color': `${text.primary}`,
    },
    stakePools: {
      '--theme-staking-stake-pools-title-color': `${text.primary}`,
      '--theme-staking-stake-pools-search-button-color': `${text.primary}`,
      '--theme-staking-stake-pool-background-color': `${background.primary.lightest}`,
      '--theme-staking-stake-pool-border-color': `${border}`,
      '--theme-staking-stake-pool-glow-color': `${background.secondary.lightest}`,
      '--theme-staking-stake-pool-grey-color': `${background.primary.lightest}`,
      '--theme-staking-stake-pool-grey-bg-color': `${background.primary.light}`,
      '--theme-staking-stake-pool-saturation-background-color': `${chroma(
        text.secondary
      ).alpha(0.2)}`,
      '--theme-staking-stake-pool-saturation-green-color': '#1ccc5d',
      '--theme-staking-stake-pool-saturation-orange-color': '#ff8800',
      '--theme-staking-stake-pool-saturation-red-color': `${error.regular}`,
      '--theme-staking-stake-pool-saturation-yellow-color': '#ffcc00',
      '--theme-staking-stake-pools-search-icon-color': `${text.primary}`,
      '--theme-staking-stake-pools-search-clear-button-background-color':
        'rgba(68, 91, 124, 0.05)',
      '--theme-staking-stake-pool-selected-background-color': '#5da377',
      '--theme-staking-stake-pool-selected-checkmark-icon-color': `${text.secondary}`,
      '--theme-staking-stake-pool-selected-ticker-color': `${text.secondary}`,
      '--theme-staking-stake-pool-ticker-color': `${text.primary}`,
      '--theme-staking-stake-pool-retirement-background-color': `${error.regular}`,
      '--theme-staking-stake-pool-tooltip-border-color': `${border}`,
      '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.25)',
      '--theme-staking-stake-pool-tooltip-text-color': `${text.primary}`,
      '--theme-staking-stake-pool-tooltip-id-background-color': `${text.secondary}`,
      '--theme-staking-stake-pool-tooltip-id-shadow-1': 'rgba(42, 43, 60, 1)',
      '--theme-staking-stake-pool-tooltip-id-shadow-2': 'rgba(42, 43, 60, 0)',
      '--theme-staking-stake-pool-tooltip-link-color': `${background.secondary.dark}`,
      '--theme-staking-stake-pool-tooltip-neutral-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.1)}`,
      '--theme-staking-stake-pool-tooltip-neutral-text-color': text.primary,
      '--theme-staking-stake-pool-tooltip-table-title-color': `${text.primary}`,
      '--theme-staking-stake-pool-tooltip-table-param-color': `${text.primary}`,
      '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
      '--theme-staking-stake-pool-tooltip-retirement-background-color': `${error.regular}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-background-color': `${background.secondary.regular}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color': `${background.secondary.light}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color': `${background.secondary.darkest}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-text-color': `${text.secondary}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color': `${text.secondary}`,
      '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
        'transparent',
      '--theme-staking-progress-label-light': `${text.secondary}`,
    },
    staking: {
      '--theme-staking-content-background-color': `${background.primary.lightest}`,
      '--theme-staking-content-border-color': `${border}`,
      '--theme-staking-font-color-accent': `${focus}`,
      '--theme-staking-font-color-regular': `${text.primary}`,
      '--theme-staking-font-color-light': `${chroma(text.primary).alpha(0.7)}`,
      '--theme-staking-font-color-lighter': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--theme-staking-table-head-background-color': `${background.primary.regular}`,
      '--theme-staking-table-border-color': `${border}`,
      '--theme-staking-link-color': `${background.secondary.regular}`,
      '--theme-staking-link-color-light': `${background.secondary.light}`,
      '--theme-staking-progress-bar-background-color': `${background.primary.regular}`,
      '--theme-staking-progress-stripe-dark-1-background-color': `${background.secondary.dark}`,
      '--theme-staking-progress-stripe-dark-2-background-color': `${background.secondary.regular}`,
      '--theme-staking-slider-background-color-1': `${background.secondary.dark}`,
      '--theme-staking-slider-background-color-2': `${background.secondary.regular}`,
      '--theme-staking-slider-box-shadow-color': `${chroma(
        background.primary.regular
      ).alpha(0.25)}`,
      '--theme-staking-table-body-highlighted-text-color': `${background.secondary.dark}`,
      '--theme-staking-info-learn-more-button-text-color': `${background.primary.lightest}`,
      '--theme-staking-info-learn-more-icon-color': `${text.secondary}`,
      '--theme-staking-learn-more-button-color': `${background.primary.lightest}`,
      '--theme-staking-learn-more-icon-color': `${text.secondary}`,
      '--theme-staking-donut-ring-completed-color': `${error.regular}`,
      '--theme-staking-donut-ring-remaining-color': `${error.ultralight}`,
      '--theme-staking-wallet-row-border-color': `${border}`,
      '--theme-staking-wallet-row-action-undelegate-text-color': `${error.regular}`,
      '--theme-staking-wallet-row-ticker-background-color': `${background.primary.regular}`,
      '--theme-staking-wallet-row-ticker-text-color': `${text.primary}`,
      '--theme-staking-wallet-row-ticker-ada-icon-fill-color': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-staking-dropdown-item-text-color-hover': `${text.primary}`,
      '--theme-staking-dropdown-item-background-color': `${background.primary.lightest}`,
      '--theme-staking-dropdown-item-background-color-hover': `${chroma(
        background.primary.regular
      ).alpha(0.1)}`,
      '--theme-staking-delegation-center-gear-icon-fill-color': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-staking-delegation-center-gear-icon-fill-color-active': `${text.primary}`,
      '--theme-staking-delegation-center-no-wallets-instructions-color': `${text.primary}`,
      '--theme-staking-delegation-center-divider-border-color': `${border}`,
      '--theme-staking-delegation-center-fees-amount-color': `${error.regular}`,
      '--theme-staking-countdown-widget-background-color': `${background.primary.regular}`,
      '--theme-staking-countdown-widget-delimeter-background-color': `${text.primary}`,
      '--theme-staking-countdown-widget-field-label-color': `${chroma(
        background.primary.light
      ).alpha(0.7)}`,
      '--theme-staking-countdown-widget-field-value-color': `${text.primary}`,
      '--theme-staking-export-button-shadow-color': `${chroma(
        background.primary.regular
      ).alpha(0.18)}`,
      '--theme-staking-export-button-color': `${background.primary.lightest}`,
      '--theme-staking-redeemItnRewards-text-color': `${text.secondary}`,
      '--theme-staking-redeemItnRewards-separator-color': `${`${background.secondary.dark}`}`,
      '--theme-staking-redeemItnRewards-attention-text-color': `${error.regular}`,
      '--theme-staking-redeemItnRewards-description-text-color': `${chroma(
        background.primary.light
      ).alpha(0.7)}`,
      '--theme-staking-redeemItnRewards-icon-color': `${text.primary}`,
    },
    support: {
      '--theme-support-settings-item-color': `${text.primary}`,
      '--theme-support-settings-link-color': `${background.secondary.regular}`,
      '--theme-support-settings-text-color': `${text.primary}`,
    },
    syncing: {
      '--theme-syncing-background-color': `${background.primary.regular}`,
      '--theme-syncing-text-color': `${text.primary}`,
    },
    systemError: {
      '--theme-system-error-overlay-attention-icon-color': `${text.secondary}`,
      '--theme-system-error-overlay-background-color': `${error.regular}`,
      '--theme-system-error-overlay-support-link-icon-color': `${text.secondary}`,
      '--theme-system-error-overlay-text-color': `${text.secondary}`,
    },
    tabs: {
      '--theme-choice-tabs-text-color': `${text.primary}`,
      '--theme-choice-tabs-text-color-active': `${text.primary}`,
      '--theme-choice-tabs-bottom-border-color-active': `${text.primary}`,
    },
    testEnvironment: {
      '--theme-test-environment-label-background-color': '#ab1700',
      '--theme-test-environment-label-text-color': `${text.secondary}`,
    },
    topBar: {
      '--theme-topbar-background-color': `${background.secondary.darkest}`,
      '--theme-topbar-layout-body-background-color': `${background.secondary.regular}`,
      '--theme-topbar-wallet-name-color': `${text.secondary}`,
      '--theme-topbar-wallet-info-color': `${text.secondary}`,
      '--theme-topbar-logo-color': `${text.primary}`,
    },
    transactions: {
      '--theme-transactions-header-background-color': `${background.primary.regular}`,
      '--theme-transactions-header-texct-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--theme-transactions-list-background-color': `${background.primary.lightest}`,
      '--theme-transactions-list-border-color': `${border}`,
      '--theme-transactions-list-group-date-color': `${text.primary}`,
      '--theme-transactions-list-item-details-color': `${text.primary}`,
      '--theme-transactions-list-item-highlight-color': `${error.regular}`,
      '--theme-transactions-state-ok-background-color': '#007600',
      '--theme-transactions-state-pending-background-color': `${background.primary.dark}`,
      '--theme-transactions-state-pending-warning-background-color': '#ec5d6b',
      '--theme-transactions-state-text-color': `${background.primary.regular}`,
      '--theme-transactions-search-background-color': `${background.primary.regular}`,
      '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
      '--theme-transactions-icon-type-income-background-color': '#2dc06c',
      '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
      '--theme-transactions-icon-type-pending-regular-background-color': `
        ${chroma(text.primary).alpha(0.5)}
      `,
      '--theme-transactions-icon-type-pending-warning-background-color': `
        ${chroma(error.regular).alpha(0.8)}
      `,
      '--theme-transactions-icon-type-failed-background-color': `
        ${chroma(error.regular)}
      `,
      '--theme-transactions-arrow-stroke-color': `${text.primary}`,
      '--theme-transactions-filter-button-color': `${background.primary.lightest}`,
      '--theme-transactions-filter-button-shadow-color': `${chroma(
        background.primary.regular
      ).alpha(0.18)}`,
      '--theme-transactions-date-picker-button-background-color': `${background.primary.lightest}`,
      '--theme-transactions-date-picker-button-background-color-hover': `${background.primary.regular}`,
      '--theme-transactions-date-picker-button-color': `${text.primary}`,
      '--theme-transactions-filter-modal-bg-color': `${background.primary.lightest}`,
      '--theme-transactions-filter-date-picker-shadow':
        '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
      '--theme-transactions-filter-title-button-text-color': `${background.primary.lightest}`,
      '--theme-transactions-filter-title-button-background-color': `${background.primary.regular}`,
      '--theme-transactions-filter-title-button-background-color-hover': `${chroma(
        background.primary.regular
      ).alpha(0.5)}`,
      '--theme-transactions-filter-title-button-background-color-active': `${background.primary.regular}`,
      '--theme-transactions-transfer-funds-selected-wallet-background-color': `${chroma(
        focus
      ).alpha(0.05)}`,
    },
    uploader: {
      '--theme-uploader-text-color': `${text.primary}`,
      '--theme-uploader-border-color': `${border}`,
    },
    utxo: {
      '--theme-utxo-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.5)}`,
      '--theme-utxo-title-text-color': `${text.primary}`,
      '--theme-utxo-title-description-color': `${chroma(text.primary).alpha(
        0.7
      )}`,
      '--theme-utxo-bar-color': `${chroma(background.secondary.dark).alpha(
        0.5
      )}`,
      '--theme-utxo-label-text-color': `${chroma(text.primary).alpha(0.45)}`,
      '--theme-utxo-tick-text-color': `${chroma(text.primary).alpha(0.45)}`,
      '--theme-utxo-cursor-background-color': `${chroma(
        background.secondary.lightest
      ).alpha(0.2)}`,
      '--theme-utxo-tooltip-background-color': `${chroma(
        background.primary.darkest
      )}`,
      '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
      '--theme-utxo-tooltip-text-color': `${text.secondary}`,
    },
    voting: {
      '--theme-voting-font-color-accent': `${focus}`,
      '--theme-voting-font-color-light': `${chroma(text.primary).alpha(0.7)}`,
      '--theme-voting-font-color-regular': `${text.primary}`,
      '--theme-voting-info-background-color': `${chroma(
        background.primary.darkest
      )}`,
      '--theme-voting-info-font-color': `${chroma(background.primary.darkest)}`,
      '--theme-voting-registration-steps-activation-steps-indicator-color': `${text.primary}`,
      '--theme-voting-registration-steps-choose-wallet-error-message-color': `${error.regular}`,
      '--theme-voting-registration-steps-choose-wallet-error-message-light-color': `${chroma(
        error.regular
      ).alpha(0.7)}`,
      '--theme-voting-registration-steps-deposit-fees-amount-color': `${error.regular}`,
      '--theme-voting-registration-steps-deposit-fees-label-color': `${text.primary}`,
      '--theme-voting-registration-steps-description-color': `${chroma(
        text.primary
      ).alpha(0.8)}`,
      '--theme-voting-registration-steps-description-highlighted-color': `${text.primary}`,
      '--theme-voting-separator-color': `${chroma(text.primary).alpha(0.15)}`,
    },
    walletRestoreDialog: {
      '--theme-wallet-restore-dialog-new-label-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.1)}`,
      '--theme-wallet-restore-dialog-new-label-color': `${chroma(
        text.primary
      )}`,
      '--theme-wallet-restore-dialog-step-walletType-hardwareWalletDisclaimer-text-color': `${error.regular}`,
    },
    walletSettings: {
      '--theme-wallet-settings-section-separator-color': `${chroma(
        text.primary
      ).alpha(0.1)}`,
    },
    walletNotRespondingOverlay: {
      '--theme-wallet-not-responding-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-wallet-not-responding-button-background-color': `${background.secondary.dark}`,
      '--theme-wallet-not-responding-button-background-color-hover': `${text.secondary}`,
      '--theme-wallet-not-responding-button-border-color': `${text.secondary}`,
      '--theme-wallet-not-responding-button-text-color': `${text.secondary}`,
      '--theme-wallet-not-responding-button-text-color-hover': `${background.secondary.darkest}`,
      '--theme-wallet-not-responding-description-background-color': `${background.secondary.dark}`,
      '--theme-wallet-not-responding-description-text-color': `${text.secondary}`,
      '--theme-wallet-not-responding-icon-color': `${text.secondary}`,
      '--theme-wallet-not-responding-link-text-color': `${text.secondary}`,
      '--theme-wallet-not-responding-title-text-color': `${text.secondary}`,
    },
    widgets: {
      '--theme-widgets-asset-token-background-color': `${chroma(
        background.primary.regular
      ).alpha(0.5)}`,
      '--theme-widgets-asset-token-fingerprint-background-color': `rgba(${chroma(
        focus
      )
        .alpha(0.1)
        .rgba()})`,
      '--theme-widgets-asset-token-text-color': `${text.primary}`,
    },
  };
};
export const createTheme = (
  fullThemeParts: CreateThemeParams
): Record<string, any> => {
  const { colors: themeColors, config, fonts: themeFonts } = fullThemeParts;
  let daedalusTheme = {};
  let colors = {};
  let fonts = themeFonts;

  if (themeColors && !isEmpty(themeColors)) {
    colors = {
      ...themeColors,
      error: createErrorShades(themeColors.error),
      background: {
        primary: createBackgroundShades(themeColors.background.primary),
        secondary: createBackgroundShades(themeColors.background.secondary),
      },
    };
  }

  if (!themeFonts || isEmpty(themeFonts)) {
    fonts = {
      black: 'NotoSans-Black, NotoSansCJKjp-Black',
      bold: 'NotoSans-Bold, NotoSansCJKjp-Bold',
      heavy: 'NotoSans-ExtraBold, NotoSansCJKjp-Black',
      light: 'NotoSans-Light, NotoSansCJKjp-Light',
      medium: 'NotoSans-Medium, NotoSansCJKjp-Medium',
      mono: 'SFMono-Light',
      regular: 'NotoSans-Regular, NotoSansCJKjp-Regular',
      semibold: 'NotoSans-SemiBold, NotoSansCJKjp-Medium',
      thin: 'NotoSans-Thin, NotoSansCJKjp-Thin',
      ultralight: 'NotoSans-ExtraLight, NotoSansCJKjp-Thin',
    };
  }

  // create react-polymorph & daedalus theme, combine into a theme object
  if (colors && !isEmpty(colors) && fonts && !isEmpty(fonts)) {
    daedalusTheme = {
      ...createReactPolymorphTheme({
        // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
        colors,
        fonts,
      }),
      ...createDaedalusComponentsTheme({
        // @ts-ignore ts-migrate(2322) FIXME: Type '{}' is not assignable to type 'ThemeColors'.
        colors,
        fonts,
      }),
    };
  }

  // if user passed theme config, compose its values with daedalusTheme object
  if (config && !isEmpty(config)) {
    daedalusTheme = updateTheme(daedalusTheme, config);
  }

  // return theme object (composed with config if passed by user)
  return daedalusTheme;
};
