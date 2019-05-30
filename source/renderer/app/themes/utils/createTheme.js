// @flow
import chroma from 'chroma-js';
import { isEmpty } from 'lodash';
import { createBackgroundShades, createErrorShades } from './createShades';
import type { ThemeColors, ThemeFonts, CreateThemeParams } from '../types';

type PartialThemeParts = {
  colors: ThemeColors,
  fonts: ThemeFonts,
};

// assigns values to all react-polymorph CSS variables & returns them
const createReactPolymorphTheme = (themeParts: PartialThemeParts): Object => {
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
      '--rp-autocomplete-selected-word-box-bg-color': `${
        background.secondary.light
      }`,
      '--rp-autocomplete-selected-word-text-color': `${text.secondary}`,
      '--rp-autocomplete-selected-words-font-family': `${fonts.regular}`,
    },
    rpBubble: {
      '--rp-bubble-bg-color': `${background.primary.lightest}`,
      '--rp-bubble-border-color': `${border}`,
      '--rp-bubble-border-radius': '2px',
    },
    rpButton: {
      '--rp-button-bg-color': `${background.secondary.regular}`,
      '--rp-button-bg-color-active': `${background.secondary.darkest}`,
      '--rp-button-bg-color-disabled': `${background.secondary.lightest}`,
      '--rp-button-bg-color-hover': `${background.secondary.light}`,
      '--rp-button-font-family': `${fonts.medium}`,
      '--rp-button-height': '50px',
      '--rp-button-line-height': '20px',
      '--rp-button-padding': '0',
      '--rp-button-text-color': `${text.secondary}`,
      '--rp-button-text-transform': 'none',
      '--rp-button-width': '360px',
    },
    rpCheckbox: {
      '--rp-checkbox-border': `1px solid ${background.secondary.regular}`,
      '--rp-checkbox-border-color-disabled': `${chroma(
        background.secondary.regular
      ).alpha(0.4)}`,
      '--rp-checkbox-check-bg-color': `${background.secondary.regular}`,
      '--rp-checkbox-label-text-color': `${text.primary}`,
      '--rp-checkbox-label-text-color-disabled': `${chroma(text.primary).alpha(
        0.3
      )}`,
    },
    rpError: {
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
    },
    rpSelect: {
      '--rp-select-arrow-bg-color': `${border}`,
      '--rp-select-arrow-bg-color-open': `${focus}`,
      '--rp-select-input-bg-color': `${background.primary.lightest}`,
      '--rp-select-input-border-color': `${border}`,
      '--rp-select-input-border-color-focus': `${focus}`,
      '--rp-select-input-text-color': `${text.primary}`,
    },
    rpSwitch: {
      '--rp-switch-bg-color-off': `${background.secondary.regular}`,
      '--rp-switch-bg-color-on': `${background.secondary.regular}`,
      '--rp-switch-label-margin': '0 30px 0 0',
      '--rp-switch-label-opacity': '0.5',
      '--rp-switch-label-text-color': `${text.primary}`,
      '--rp-switch-label-width': '100%',
      '--rp-switch-opacity-off': '0.3',
      '--rp-switch-root-margin': '0 0 30px 0',
      '--rp-switch-thumb-bg-color': `${text.secondary}`,
    },
    rpTextarea: {
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
  };
};

// assigns values to all Daedalus CSS variables & returns them
const createDaedalusComponentsTheme = (
  themeParts: PartialThemeParts
): Object => {
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
      '--theme-about-window-icon-close-button-color': `${
        background.primary.lightest
      }`,
      '--theme-about-window-icon-close-hover-background': `${
        background.secondary.dark
      }`,
    },
    adaRedemption: {
      '--theme-ada-redemption-headline-color': `${text.primary}`,
      '--theme-ada-redemption-instructions-color': `${text.primary}`,
      '--theme-ada-redemption-success-overlay-border-color': `${
        text.secondary
      }`,
      '--theme-ada-redemption-success-overlay-message-color': `${
        text.secondary
      }`,
      '--theme-ada-redemption-success-overlay-button-text-color': `${
        text.secondary
      }`,
      '--theme-ada-redemption-success-overlay-button-text-color-hover': `${
        background.secondary.regular
      }`,
      '--theme-ada-redemption-success-overlay-background-color': `${
        background.secondary.regular
      }`,
      '--theme-ada-redemption-success-overlay-button-background-color-hover': `${
        background.secondary.light
      }`,
      '--theme-ada-redemption-disclaimer-background-color':
        'rgba(171, 23, 0, 0.94)',
      '--theme-ada-redemption-disclaimer-text-color': `${text.secondary}`,
      '--theme-ada-redemption-disclaimer-checkbox-color-check': `${
        background.primary.lightest
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-color-checked': `${
        background.primary.lightest
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-color-after': `${
        error.regular
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-label-color': `${
        text.secondary
      }`,
      '--theme-ada-redemption-no-wallets-instructions-color': `${text.primary}`,
      '--theme-ada-redemption-disclaimer-button-border-color': `${
        background.primary.lightest
      }`,
    },
    blockConsolidation: {
      '--theme-block-consolidation-background-color': `${
        background.secondary.regular
      }`,
      '--theme-block-consolidation-title-text-color': `${text.secondary}`,
      '--theme-block-consolidation-text-color': `${text.secondary}`,
      '--theme-block-consolidation-text-highlight-color': `${text.secondary}`,
      '--theme-block-consolidation-epochs-text-color': `${
        background.secondary.regular
      }`,
      '--theme-block-consolidation-indicator-text-color': `${text.secondary}`,
      '--theme-block-consolidation-indicator-container-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-block-consolidation-indicator-epochs-behind-background-color-1': `${
        background.primary.lightest
      }`,
      '--theme-block-consolidation-indicator-epochs-behind-background-color-2': `${
        background.secondary.regular
      }`,
      '--theme-block-consolidation-stripe-dark-1-background-color': `${
        background.primary.regular
      }`,
      '--theme-block-consolidation-stripe-dark-2-background-color': `${
        background.primary.dark
      }`,
      '--theme-block-consolidation-stripe-light-1-background-color': `${
        background.secondary.lightest
      }`,
      '--theme-block-consolidation-stripe-light-2-background-color': `${
        background.primary.light
      }`,
      '--theme-block-consolidation-button-background-color': `${
        background.secondary.dark
      }`,
      '--theme-block-consolidation-button-background-color-hover': `${
        background.primary.lightest
      }`,
      '--theme-block-consolidation-button-text-color-hover': `${
        background.secondary.regular
      }`,
      '--theme-block-consolidation-button-border-color': `${
        background.primary.lightest
      }`,
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
    baton: {
      '--theme-label-button-color': `${text.primary}`,
    },
    buttonAttention: {
      '--theme-button-attention-background-color': `${error.regular}`,
      '--theme-button-attention-background-color-hover': `${error.light}`,
      '--theme-button-attention-background-color-active': `${error.darkest}`,
      '--theme-button-attention-background-color-disabled': `${
        error.ultralight
      }`,

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
      '--theme-button-flat-background-color-hover': `${
        background.primary.lighter
      }`,
      '--theme-button-flat-background-color-active': `${
        background.primary.regular
      }`,
      '--theme-button-flat-background-color-disabled': `${
        background.primary.lighter
      }`,
      '--theme-button-flat-text-color-disabled': `${text.primary}`,
      '--theme-button-flat-text-color': `${text.primary}`,
      '--theme-button-flat-outline-color': `${background.primary.dark}`,
    },
    buttonPrimary: {
      '--theme-button-primary-background-color': `${
        background.secondary.regular
      }`,
      '--theme-button-primary-background-color-hover': `${
        background.secondary.light
      }`,
      '--theme-button-primary-background-color-active': `${
        background.secondary.darkest
      }`,
      '--theme-button-primary-background-color-disabled': `${
        background.secondary.lightest
      }`,
      '--theme-button-primary-text-color-disabled': `${text.secondary}`,
      '--theme-button-primary-text-color': `${text.secondary}`,
      '--theme-button-primary-outline-color': `${background.secondary.light}`,
    },
    connecting: {
      '--theme-connecting-background-color': `${background.primary.regular}`,
      '--theme-connecting-text-color': `${text.primary}`,
    },
    dataMigration: {
      '--theme-data-migration-layer-background-color': `${
        background.secondary.regular
      }`,
      '--theme-data-migration-layer-box-shadow-color': `${
        background.secondary.regular
      }`,
      '--theme-data-migration-layer-button-background-color-hover': `${
        background.primary.regular
      }`,
      '--theme-data-migration-layer-text-color': `${text.secondary}`,
      '--theme-data-migration-layer-text-opacity-color': `${text.secondary}`,
    },
    dialog: {
      '--theme-dialog-choice-tabs-text-color': `${text.primary}`,
      '--theme-dialog-choice-tabs-text-color-active': `${text.primary}`,
      '--theme-dialog-choice-tabs-bottom-border-color-active': `${focus}`,
      '--theme-dialog-big-button-background-color': `${
        background.primary.lightest
      }`,
      '--theme-dialog-big-button-border-color': `${border}`,
      '--theme-dialog-big-button-label-color': `${text.primary}`,
      '--theme-dialog-big-button-description-color': `${text.primary}`,
      '--theme-dialog-title-color': `${text.primary}`,
      '--theme-dialog-text-color': `${text.primary}`,
      '--theme-dialog-border-color': `${border}`,
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
    icon: {
      '--theme-icon-nav-color': `${text.secondary}`,
      '--theme-icon-nav-color-active': `${text.primary}`,
      '--theme-icon-sidebar-color': `${background.primary.regular}`,
      '--theme-icon-toggle-menu-color': `${background.primary.regular}`,
      '--theme-icon-node-update-notification-arrow-color': `${text.primary}`,
      '--theme-icon-add-wallet-from-sidebar-color': `${text.secondary}`,
      '--theme-icon-ada-redemption-attention-color': `${text.secondary}`,
      '--theme-icon-ada-redemption-success-color': `${text.primary}`,
      '--theme-icon-ada-redemption-certificate-color': `${text.primary}`,
      '--theme-icon-ada-redemption-no-wallets': `${text.primary}`,
      '--theme-icon-ada-summary-wallet-amount-symbol-color': `${text.primary}`,
      '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color': `${
        text.primary
      }`,
      '--theme-icon-add-wallet-dialog-big-button-color': `${
        background.primary.darker
      }`,
      '--theme-icon-copy-address-color': `${text.primary}`,
      '--theme-icon-back-button-color': `${text.primary}`,
      '--theme-icon-close-button-color': `${text.primary}`,
      '--theme-icon-file-upload-color': `${text.primary}`,
      '--theme-icon-transactions-ada-symbol-color': `${text.primary}`,
      '--theme-icon-syncing-logo-color': `${text.primary}`,
      '--theme-icon-connecting-logo-color': `${text.primary}`,
      '--theme-icon-transaction-type-color': '#fafbfc',
    },
    input: {
      '--theme-input-border-color': `${border}`,
      '--theme-input-label-color': `${text.primary}`,
      '--theme-input-text-color': `${text.primary}`,
      '--theme-input-right-floating-text-color': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--theme-input-placeholder-color': `${chroma(text.primary).alpha(0.5)}`,
      '--theme-input-remove-color-light': `${error.regular}`,
      '--theme-input-background-color': `${background.primary.lightest}`,
      '--theme-input-focus-border-color': `${focus}`,
      '--theme-input-hint-font': `${fonts.regular}`,
    },
    loading: {
      '--theme-loading-background-color': `${background.primary.regular}`,
      '--theme-loading-no-disk-space-background-color': `${
        background.primary.regular
      }`,
      '--theme-loading-no-disk-space-text-color': `${text.primary}`,
      '--theme-loading-no-disk-space-attention-icon-color': `${text.primary}`,
      '--theme-loading-status-icons-on-color': '#2dc06c',
      '--theme-loading-status-icons-off-color': '#ea4c5b',
      '--theme-loading-status-icons-unloaded-loading-color': `${text.primary}`,
      '--theme-loading-status-icons-unloaded-syncing-color': `${text.primary}`,
      '--theme-loading-status-icons-tooltip-color': `${text.primary}`,
      '--theme-loading-spinner-color': `${text.primary}`,
    },
    mnemonic: {
      '--theme-mnemonic-background-color': `${background.primary.regular}`,
      '--theme-mnemonic-background-color-hover': `${background.primary.dark}`,
      '--theme-backup-mnemonic-background-color': `${background.primary.light}`,
    },
    modal: {
      '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
    },
    navItem: {
      '--theme-nav-item-background-color': `${background.secondary.darkest}`,
      '--theme-nav-item-background-color-active': `${
        background.primary.lightest
      }`,
      '--theme-nav-item-background-color-hover': `${
        background.secondary.darker
      }`,
      '--theme-nav-item-text-color': `${text.secondary}`,
      '--theme-nav-item-text-color-active': `${text.primary}`,
    },
    network: {
      '--theme-network-window-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.96)}`,
      '--theme-network-window-text-color': `${text.secondary}`,
      '--theme-network-window-icon-close-hover-background': `${
        background.secondary.dark
      }`,
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
    },
    nodeUpdate: {
      '--theme-node-update-background-color': `${background.primary.regular}`,
      '--theme-node-update-title-color': `${text.primary}`,
      '--theme-node-update-message-color': `${text.primary}`,
      '--theme-node-sync-info-message-background-color': `${
        background.primary.regular
      }`,
      '--theme-node-sync-info-message-text-color': `${text.primary}`,
      '--theme-node-update-accept-button-background-color': `${
        background.secondary.regular
      }`,
      '--theme-node-update-accept-button-background-color-hover': `${
        background.secondary.light
      }`,
      '--theme-node-update-accept-button-background-color-active': `${
        background.secondary.darkest
      }`,
      '--theme-node-update-deny-button-background-color': `${
        background.secondary.regular
      }`,
      '--theme-node-update-deny-button-background-color-hover': `${
        background.secondary.light
      }`,
      '--theme-node-update-deny-button-background-color-active': `${
        background.secondary.darkest
      }`,
      '--theme-node-update-button-text-color': `${text.secondary}`,
    },
    notification: {
      '--theme-notification-message-background-color': `${chroma(
        background.secondary.regular
      ).alpha(0.88)}`,
      '--theme-notification-message-text-color': `${text.secondary}`,
    },
    paperWallet: {
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': `${
        background.secondary.regular
      }`,
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': `${
        text.secondary
      }`,
    },
    progressBar: {
      '--theme-progress-bar-background-color': `${chroma(
        background.primary.light
      ).alpha(0.3)}`,
      '--theme-progress-bar-foreground-color': `${chroma(
        background.primary.light
      ).alpha(0.7)}`,
    },
    receiveQRCode: {
      '--theme-receive-qr-code-background-color': 'transparent',
      '--theme-receive-qr-code-foreground-color': '#000',
    },
    reportIssue: {
      '--theme-report-issue-button-background-color': `${
        background.secondary.regular
      }`,
      '--theme-report-issue-button-background-color-hover': `${
        background.secondary.light
      }`,
      '--theme-report-issue-button-background-color-active': `${
        background.secondary.darkest
      }`,
      '--theme-report-issue-connecting-background-color': `${
        background.primary.regular
      }`,
      '--theme-report-issue-connecting-text-color': `${text.primary}`,
      '--theme-report-issue-syncing-background-color': `${
        background.primary.regular
      }`,
      '--theme-report-issue-syncing-text-color': `${text.primary}`,
      '--theme-report-issue-syncing-download-logs-text-color': `${
        text.primary
      }`,
      '--theme-report-issue-syncing-download-logs-text-color-connecting': `${
        text.primary
      }`,
    },
    scrollbar: {
      '--theme-scrollbar-thumb-background': `${background.primary.darker}`,
    },
    sendConfirmation: {
      '--theme-send-confirmation-dialog-send-values-color': `${error.regular}`,
    },
    settings: {
      '--theme-settings-body-background-color': `${background.primary.regular}`,
      '--theme-settings-pane-background-color': `${
        background.primary.lightest
      }`,
      '--theme-settings-pane-border': `1px solid ${border}`,
      '--theme-settings-menu-box-background-color': `${
        background.primary.lightest
      }`,
      '--theme-settings-menu-box-border': `1px solid ${border}`,
      '--theme-settings-menu-item-text-color': `${text.primary}`,
      '--theme-settings-menu-item-text-color-active': `${text.primary}`,
      '--theme-settings-menu-item-text-color-disabled': `${chroma(
        text.primary
      ).alpha(0.5)}`,
      '--theme-settings-menu-item-background-color-active': `${
        background.primary.regular
      }`,
      '--theme-settings-menu-item-left-border-color-active': `${
        background.secondary.regular
      }`,
      '--theme-settings-theme-select-title-color': `${text.primary}`,
    },
    sidebar: {
      '--theme-sidebar-background-color': `${background.secondary.regular}`,
      '--theme-sidebar-category-background-color-hover': `${
        background.secondary.dark
      }`,
      '--theme-sidebar-category-background-color-active': `${
        background.secondary.darker
      }`,
      '--theme-sidebar-category-text-color': `${text.secondary}`,
      '--theme-sidebar-menu-background-color': `${background.secondary.darker}`,
      '--theme-sidebar-menu-item-background-color-hover': `${
        background.secondary.darkest
      }`,
      '--theme-sidebar-menu-item-background-color-active': `${
        background.secondary.darkest
      }`, // rename to active wallet?
      '--theme-sidebar-menu-item-wallet-name-color': `${text.secondary}`,
      '--theme-sidebar-menu-item-wallet-info-color': `${text.secondary}`,
      '--theme-sidebar-menu-add-button-background-color': `${
        background.secondary.darkest
      }`,
      '--theme-sidebar-menu-add-button-background-color-active': `${chroma(
        background.secondary.darkest
      ).alpha(0.66)}`,
      '--theme-sidebar-menu-add-button-background-color-hover': `${chroma(
        background.secondary.darkest
      ).alpha(0.66)}`,
      '--theme-sidebar-menu-add-button-text-color': `${text.secondary}`,
    },
    staking: {
      '--theme-staking-background-color': `${background.primary.regular}`,
      '--theme-staking-content-background-color': `${
        background.primary.lightest
      }`,
      '--theme-staking-content-border-color': `${border}`,
      '--theme-staking-font-color-accent': `${focus}`,
      '--theme-staking-font-color-regular': `${text.primary}`,
      '--theme-staking-font-color-light': `${chroma(text.primary).alpha(0.7)}`,
      '--theme-staking-font-color-lighter': `${chroma(text.primary).alpha(
        0.5
      )}`,
      '--theme-staking-table-head-background-color': `${
        background.primary.regular
      }`,
      '--theme-staking-table-border-color': `${border}`,
      '--theme-staking-link-color': `${background.secondary.regular}`,
      '--theme-staking-link-color-light': `${background.secondary.light}`,
      '--theme-staking-decentralization-progress-stripe-dark-1-background-color': `${
        background.secondary.dark
      }`,
      '--theme-staking-decentralization-progress-stripe-dark-2-background-color': `${
        background.secondary.regular
      }`,
    },
    support: {
      '--theme-support-settings-text-color': `${text.primary}`,
      '--theme-support-settings-link-color': `${background.secondary.regular}`,
      '--theme-support-settings-item-color': `${text.primary}`,
    },
    syncing: {
      '--theme-syncing-background-color': `${background.primary.regular}`,
      '--theme-syncing-text-color': `${text.primary}`,
    },
    systemError: {
      '--theme-system-error-overlay-attention-icon-color': `${text.secondary}`,
      '--theme-system-error-overlay-background-color': `${error.regular}`,
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
      '--theme-topbar-wallet-name-color': `${text.secondary}`,
      '--theme-topbar-wallet-info-color': `${text.secondary}`,
      '--theme-topbar-layout-body-background-color': `${
        background.secondary.regular
      }`,
    },
    transactions: {
      '--theme-transactions-list-background-color': `${
        background.primary.lightest
      }`,
      '--theme-transactions-list-border-color': `${border}`,
      '--theme-transactions-list-group-date-color': `${text.primary}`,
      '--theme-transactions-list-item-details-color': `${text.primary}`,
      '--theme-transactions-state-failed-background-color': `${
        background.primary.dark
      }`,
      '--theme-transactions-state-failed-text-color': `${text.primary}`,
      '--theme-transactions-state-pending-background-color': `${
        background.primary.dark
      }`,
      '--theme-transactions-state-pending-stripes-color': `${
        background.primary.darker
      }`,
      '--theme-transactions-priority-color': `${background.primary.regular}`,
      '--theme-transactions-priority-low-background-color': `${error.dark}`,
      '--theme-transactions-priority-medium-background-color': '#e6aa00',
      '--theme-transactions-priority-high-background-color': '#007600',
      '--theme-transactions-search-background-color': `${
        background.primary.regular
      }`,
      '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
      '--theme-transactions-icon-type-income-background-color': '#2dc06c',
      '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
      '--theme-transactions-icon-type-failed-background-color': `${
        error.light
      }`,
      '--theme-transactions-arrow-stroke-color': `${text.primary}`,
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
  };
};

export const createTheme = (fullThemeParts: CreateThemeParams): Object => {
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
      ...createReactPolymorphTheme({ colors, fonts }),
      ...createDaedalusComponentsTheme({ colors, fonts }),
    };

    // flatten daedalusTheme object for consumption by ThemeManager
    daedalusTheme = Object.values(daedalusTheme).reduce(
      (theme, componentVars) => ({ ...theme, ...componentVars }),
      {}
    );
  }

  // if user passed theme config, compose its values with daedalusTheme object
  if (config && !isEmpty(config)) {
    daedalusTheme = Object.values(config).reduce(
      (theme, componentVars) => ({ ...theme, ...componentVars }),
      daedalusTheme
    );
  }

  // returned flat theme object composed with config (if passed by user)
  return daedalusTheme;
};
