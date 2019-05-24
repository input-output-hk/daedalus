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
  const { primary, secondary, error } = colors;
  return {
    rpAutocomplete: {
      '--rp-autocomplete-bg-color': `${primary.background.lightest}`,
      '--rp-autocomplete-border': `1px solid ${primary.border}`,
      '--rp-autocomplete-border-color-opened': `${primary.focus}`,
      '--rp-autocomplete-input-text-color': `${primary.text}`,
      '--rp-autocomplete-placeholder-color': `${primary.placeholder}`,
      '--rp-autocomplete-selected-word-box-bg-color': `${
        secondary.background.light
      }`,
      '--rp-autocomplete-selected-word-text-color': `${secondary.text}`,
      '--rp-autocomplete-selected-words-font-family': `${fonts.regular}`,
    },
    rpBubble: {
      '--rp-bubble-bg-color': `${primary.background.lightest}`,
      '--rp-bubble-border-color': `${primary.border}`,
      '--rp-bubble-border-radius': '2px',
    },
    rpButton: {
      '--rp-button-bg-color': `${secondary.background.regular}`,
      '--rp-button-bg-color-active': `${secondary.active}`,
      '--rp-button-bg-color-disabled': `${secondary.background.lightest}`,
      '--rp-button-bg-color-hover': `${secondary.background.light}`,
      '--rp-button-font-family': `${fonts.medium}`,
      '--rp-button-height': '50px',
      '--rp-button-line-height': '20px',
      '--rp-button-padding': '0',
      '--rp-button-text-color': `${secondary.text}`,
      '--rp-button-text-transform': 'none',
      '--rp-button-width': '360px',
    },
    rpCheckbox: {
      '--rp-checkbox-border': `1px solid ${secondary.background.regular}`,
      '--rp-checkbox-border-color-disabled': `${secondary.disabled}`,
      '--rp-checkbox-check-bg-color': `${secondary.background.regular}`,
      '--rp-checkbox-label-text-color': `${primary.text}`,
      '--rp-checkbox-label-text-color-disabled': `${primary.disabled}`,
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
      '--rp-formfield-label-text-color': `${primary.text}`,
      '--rp-formfield-label-text-color-disabled': `${primary.text}`,
      '--rp-formfield-error-text-color': `${error.regular}`,
      '--rp-formfield-error-text-opacity': '0.75',
    },
    rpInput: {
      '--rp-input-bg-color': `${primary.background.lightest}`,
      '--rp-input-bg-color-disabled': `${primary.background.lighter}`,
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
    rpModal: {
      '--rp-modal-bg-color': `${primary.background.lightest}`,
      '--rp-modal-max-height': '90%',
      '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
    },
    rpOptions: {
      '--rp-option-bg-color': `${primary.background.lightest}`,
      '--rp-option-bg-color-highlighted': `${primary.background.regular}`,
      '--rp-option-border-color': `${primary.border}`,
      '--rp-option-checkmark-color': `${primary.text}`,
      '--rp-option-line-height': '22px',
      '--rp-option-text-color': `${primary.text}`,
      '--rp-options-border-color': `${primary.border}`,
      '--rp-options-shadow': 'none',
    },
    rpSelect: {
      '--rp-select-arrow-bg-color': `${primary.border}`,
      '--rp-select-arrow-bg-color-open': `${primary.active}`,
      '--rp-select-input-bg-color': `${primary.background.lightest}`,
      '--rp-select-input-border-color': `${primary.border}`,
      '--rp-select-input-border-color-focus': `${primary.focus}`,
      '--rp-select-input-text-color': `${primary.text}`,
    },
    rpSwitch: {
      '--rp-switch-bg-color-off': `${secondary.background.regular}`,
      '--rp-switch-bg-color-on': `${secondary.background.regular}`,
      '--rp-switch-label-margin': '0 30px 0 0',
      '--rp-switch-label-opacity': '0.5',
      '--rp-switch-label-text-color': `${primary.text}`,
      '--rp-switch-label-width': '100%',
      '--rp-switch-opacity-off': '0.3',
      '--rp-switch-root-margin': '0 0 30px 0',
      '--rp-switch-thumb-bg-color': `${secondary.text}`,
    },
    rpTextarea: {
      '--rp-textarea-bg-color': `${primary.background.regular}`,
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

// assigns values to all Daedalus CSS variables & returns them
const createDaedalusComponentsTheme = (
  themeParts: PartialThemeParts
): Object => {
  const { colors, fonts } = themeParts;
  const { primary, secondary, error } = colors;
  return {
    aboutWindow: {
      '--theme-about-window-background-color': `${chroma(
        secondary.background.regular
      ).alpha(0.96)}`,
      '--theme-about-window-header-bottom-border-color': `${secondary.border}`,
      '--theme-about-window-daedalus-icon-color': `${secondary.text}`,
      '--theme-about-window-cardano-icon-color': `${secondary.text}`,
      '--theme-about-window-title-varsion-color': `${secondary.text}`,
      '--theme-about-window-title-stroke-color': `${secondary.text}`,
      '--theme-about-window-content-color': `${secondary.text}`,
      '--theme-about-window-content-text-color': `${secondary.text}`,
      '--theme-about-window-content-bottom-border-color': `${secondary.border}`,
      '--theme-about-window-icon-close-button-color': `${
        primary.background.lightest
      }`,
      '--theme-about-window-icon-close-hover-background': `${
        secondary.background.dark
      }`,
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
        secondary.background.regular
      }`,
      '--theme-ada-redemption-success-overlay-background-color': `${
        secondary.background.regular
      }`,
      '--theme-ada-redemption-success-overlay-button-background-color-hover': `${
        secondary.hover
      }`,
      '--theme-ada-redemption-disclaimer-background-color':
        'rgba(171, 23, 0, 0.94)',
      '--theme-ada-redemption-disclaimer-text-color': `${secondary.text}`,
      '--theme-ada-redemption-disclaimer-checkbox-color-check': `${
        primary.background.lightest
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-color-checked': `${
        primary.background.lightest
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-color-after': `${
        error.regular
      }`,
      '--theme-ada-redemption-disclaimer-checkbox-label-color': `${
        secondary.text
      }`,
      '--theme-ada-redemption-no-wallets-instructions-color': `${primary.text}`,
      '--theme-ada-redemption-disclaimer-button-border-color': `${
        primary.background.lightest
      }`,
    },
    blockConsolidation: {
      '--theme-block-consolidation-background-color': `${
        secondary.background.regular
      }`,
      '--theme-block-consolidation-title-text-color': `${secondary.text}`,
      '--theme-block-consolidation-text-color': `${secondary.text}`,
      '--theme-block-consolidation-text-highlight-color': `${secondary.text}`,
      '--theme-block-consolidation-epochs-text-color': `${
        secondary.background.regular
      }`,
      '--theme-block-consolidation-indicator-text-color': `${secondary.text}`,
      '--theme-block-consolidation-indicator-container-background-color':
        'rgba(0, 0, 0, 0.1)',
      '--theme-block-consolidation-indicator-epochs-behind-background-color-1': `${
        primary.background.lightest
      }`,
      '--theme-block-consolidation-indicator-epochs-behind-background-color-2': `${
        secondary.background.regular
      }`,
      '--theme-block-consolidation-stripe-dark-1-background-color': `${
        primary.background.regular
      }`,
      '--theme-block-consolidation-stripe-dark-2-background-color': `${
        primary.hover
      }`,
      '--theme-block-consolidation-stripe-light-1-background-color': `${
        secondary.background.lightest
      }`,
      '--theme-block-consolidation-stripe-light-2-background-color': `${
        primary.background.light
      }`,
      '--theme-block-consolidation-button-background-color': `${
        secondary.background.dark
      }`,
      '--theme-block-consolidation-button-background-color-hover': `${
        primary.background.lightest
      }`,
      '--theme-block-consolidation-button-text-color-hover': `${
        secondary.background.regular
      }`,
      '--theme-block-consolidation-button-border-color': `${
        primary.background.lightest
      }`,
    },
    body: {
      '--theme-main-body-background-color': `${primary.background.regular}`,
      '--theme-main-body-messages-color': `${primary.text}`,
    },
    borderedBox: {
      '--theme-bordered-box-background-color': `${primary.background.lightest}`,
      '--theme-bordered-box-border': `1px solid ${primary.border}`,
      '--theme-bordered-box-text-color': `${primary.text}`,
    },
    baton: {
      '--theme-label-button-color': `${primary.placeholder}`,
    },
    buttonAttention: {
      '--theme-button-attention-background-color': `${error.regular}`,
      '--theme-button-attention-background-color-hover': `${error.light}`,
      '--theme-button-attention-background-color-active': `${error.dark}`,
      '--theme-button-attention-background-color-disabled': `${
        error.ultralight
      }`,

      '--theme-button-attention-text-color': `${secondary.text}`,
      '--theme-button-attention-text-color-disabled': `${secondary.text}`,
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
      '--theme-button-flat-background-color': `${primary.background.light}`,
      '--theme-button-flat-background-color-hover': `${
        primary.background.lighter
      }`,
      '--theme-button-flat-background-color-active': `${
        primary.background.lighter
      }`,
      '--theme-button-flat-background-color-disabled': `${
        primary.background.lighter
      }`,
      '--theme-button-flat-text-color-disabled': `${primary.text}`,
      '--theme-button-flat-text-color': `${primary.text}`,
      '--theme-button-flat-outline-color': `${primary.outline}`,
    },
    buttonPrimary: {
      '--theme-button-primary-background-color': `${
        secondary.background.regular
      }`,
      '--theme-button-primary-background-color-hover': `${
        secondary.background.light
      }`,
      '--theme-button-primary-background-color-active': `${
        secondary.background.light
      }`,
      '--theme-button-primary-background-color-disabled': `${
        secondary.background.lightest
      }`,
      '--theme-button-primary-text-color-disabled': `${secondary.text}`,
      '--theme-button-primary-text-color': `${secondary.text}`,
      '--theme-button-primary-outline-color': `${secondary.background.light}`,
    },
    connecting: {
      '--theme-connecting-background-color': `${primary.background.regular}`,
      '--theme-connecting-text-color': `${primary.text}`,
    },
    dataMigration: {
      '--theme-data-migration-layer-background-color': `${
        secondary.background.regular
      }`,
      '--theme-data-migration-layer-box-shadow-color': `${
        secondary.background.regular
      }`,
      '--theme-data-migration-layer-button-background-color-hover': `${
        primary.background.regular
      }`,
      '--theme-data-migration-layer-text-color': `${secondary.text}`,
      '--theme-data-migration-layer-text-opacity-color': `${secondary.text}`,
    },
    dialog: {
      '--theme-dialog-choice-tabs-text-color': `${primary.text}`,
      '--theme-dialog-choice-tabs-text-color-active': `${primary.text}`,
      '--theme-dialog-choice-tabs-bottom-border-color-active': `${
        primary.active
      }`,
      '--theme-dialog-big-button-background-color': `${
        primary.background.lightest
      }`,
      '--theme-dialog-big-button-border-color': `${primary.border}`,
      '--theme-dialog-big-button-label-color': `${primary.text}`,
      '--theme-dialog-big-button-description-color': `${primary.text}`,
      '--theme-dialog-title-color': `${primary.text}`,
      '--theme-dialog-text-color': `${primary.text}`,
      '--theme-dialog-border-color': `${primary.border}`,
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
      '--theme-icon-nav-color': `${secondary.text}`,
      '--theme-icon-nav-color-active': `${primary.text}`,
      '--theme-icon-sidebar-color': `${primary.background.regular}`,
      '--theme-icon-toggle-menu-color': `${primary.background.regular}`,
      '--theme-icon-node-update-notification-arrow-color': `${primary.text}`,
      '--theme-icon-add-wallet-from-sidebar-color': `${secondary.text}`,
      '--theme-icon-ada-redemption-attention-color': `${secondary.text}`,
      '--theme-icon-ada-redemption-success-color': `${primary.text}`,
      '--theme-icon-ada-redemption-certificate-color': `${primary.text}`,
      '--theme-icon-ada-redemption-no-wallets': `${primary.text}`,
      '--theme-icon-ada-summary-wallet-amount-symbol-color': `${primary.text}`,
      '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color': `${
        primary.text
      }`,
      '--theme-icon-add-wallet-dialog-big-button-color': `${
        primary.background.darker
      }`,
      '--theme-icon-copy-address-color': `${primary.text}`,
      '--theme-icon-back-button-color': `${primary.text}`,
      '--theme-icon-close-button-color': `${primary.text}`,
      '--theme-icon-file-upload-color': `${primary.text}`,
      '--theme-icon-transactions-ada-symbol-color': `${primary.text}`,
      '--theme-icon-syncing-logo-color': `${primary.text}`,
      '--theme-icon-connecting-logo-color': `${primary.text}`,
      '--theme-icon-transaction-type-color': '#fafbfc',
    },
    input: {
      '--theme-input-border-color': `${primary.border}`,
      '--theme-input-label-color': `${primary.text}`,
      '--theme-input-text-color': `${primary.text}`,
      '--theme-input-right-floating-text-color': `${chroma(primary.text).alpha(
        0.5
      )}`,
      '--theme-input-placeholder-color': `${primary.placeholder}`,
      '--theme-input-remove-color-light': `${error.regular}`,
      '--theme-input-background-color': `${primary.background.lightest}`,
      '--theme-input-focus-border-color': `${primary.focus}`,
      '--theme-input-hint-font': `${fonts.regular}`,
    },
    loading: {
      '--theme-loading-background-color': `${primary.background.regular}`,
      '--theme-loading-no-disk-space-background-color': `${
        primary.background.regular
      }`,
      '--theme-loading-no-disk-space-text-color': `${primary.text}`,
      '--theme-loading-no-disk-space-attention-icon-color': `${primary.text}`,
      '--theme-loading-status-icons-on-color': '#2dc06c',
      '--theme-loading-status-icons-off-color': '#ea4c5b',
      '--theme-loading-status-icons-unloaded-loading-color': `${primary.text}`,
      '--theme-loading-status-icons-unloaded-syncing-color': `${primary.text}`,
      '--theme-loading-status-icons-tooltip-color': `${primary.text}`,
      '--theme-loading-spinner-color': `${primary.text}`,
    },
    mnemonic: {
      '--theme-mnemonic-background-color': `${primary.background.regular}`,
      '--theme-mnemonic-background-color-hover': `${primary.hover}`,
      '--theme-backup-mnemonic-background-color': `${primary.background.light}`,
    },
    modal: {
      '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
    },
    navItem: {
      '--theme-nav-item-background-color': `${secondary.background.darkest}`,
      '--theme-nav-item-background-color-active': `${
        primary.background.lightest
      }`,
      '--theme-nav-item-background-color-hover': `${
        secondary.background.darker
      }`,
      '--theme-nav-item-text-color': `${secondary.text}`,
      '--theme-nav-item-text-color-active': `${primary.text}`,
    },
    network: {
      '--theme-network-window-background-color': `${chroma(
        secondary.background.regular
      ).alpha(0.96)}`,
      '--theme-network-window-text-color': `${secondary.text}`,
      '--theme-network-window-icon-close-hover-background': `${
        secondary.background.dark
      }`,
      '--theme-network-window-red-color': '#f06e05',
      '--theme-network-window-green-color': '#05f079',
      '--theme-network-window-white-color': `${secondary.text}`,
      '--theme-network-window-transparent-color': 'transparent',
      '--theme-network-window-border-color': `${chroma(secondary.text).alpha(
        0.7
      )}`,
      '--theme-network-window-button-text-color': `${secondary.text}`,
      '--theme-network-window-button-background-color': `${chroma(
        secondary.background.lightest
      ).alpha(0.4)}`,
    },
    nodeUpdate: {
      '--theme-node-update-background-color': `${primary.background.regular}`,
      '--theme-node-update-title-color': `${primary.text}`,
      '--theme-node-update-message-color': `${primary.text}`,
      '--theme-node-sync-info-message-background-color': `${
        primary.background.regular
      }`,
      '--theme-node-sync-info-message-text-color': `${primary.text}`,
      '--theme-node-update-accept-button-background-color': `${
        secondary.background.regular
      }`,
      '--theme-node-update-accept-button-background-color-hover': `${
        secondary.hover
      }`,
      '--theme-node-update-accept-button-background-color-active': `${
        secondary.active
      }`,
      '--theme-node-update-deny-button-background-color': `${
        secondary.background.regular
      }`,
      '--theme-node-update-deny-button-background-color-hover': `${
        secondary.hover
      }`,
      '--theme-node-update-deny-button-background-color-active': `${
        secondary.active
      }`,
      '--theme-node-update-button-text-color': `${secondary.text}`,
    },
    notification: {
      '--theme-notification-message-background-color': `${chroma(
        secondary.background.regular
      ).alpha(0.88)}`,
      '--theme-notification-message-text-color': `${secondary.text}`,
    },
    paperWallet: {
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': `${
        secondary.background.regular
      }`,
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': `${
        secondary.text
      }`,
    },
    progressBar: {
      '--theme-progress-bar-background-color': `${primary.border}`,
      '--theme-progress-bar-foreground-color': `${primary.outline}`,
    },
    receiveQRCode: {
      '--theme-receive-qr-code-background-color': 'transparent',
      '--theme-receive-qr-code-foreground-color': '#000',
    },
    reportIssue: {
      '--theme-report-issue-button-background-color': `${
        secondary.background.regular
      }`,
      '--theme-report-issue-button-background-color-hover': `${
        secondary.hover
      }`,
      '--theme-report-issue-button-background-color-active': `${
        secondary.active
      }`,
      '--theme-report-issue-connecting-background-color': `${
        primary.background.regular
      }`,
      '--theme-report-issue-connecting-text-color': `${primary.text}`,
      '--theme-report-issue-syncing-background-color': `${
        primary.background.regular
      }`,
      '--theme-report-issue-syncing-text-color': `${primary.text}`,
      '--theme-report-issue-syncing-download-logs-text-color': `${
        primary.text
      }`,
      '--theme-report-issue-syncing-download-logs-text-color-connecting': `${
        primary.text
      }`,
    },
    scrollbar: {
      '--theme-scrollbar-thumb-background': `${primary.background.darker}`,
    },
    sendConfirmation: {
      '--theme-send-confirmation-dialog-send-values-color': `${error.regular}`,
    },
    settings: {
      '--theme-settings-body-background-color': `${primary.background.regular}`,
      '--theme-settings-pane-background-color': `${
        primary.background.lightest
      }`,
      '--theme-settings-pane-border': `1px solid ${primary.border}`,
      '--theme-settings-menu-box-background-color': `${
        primary.background.lightest
      }`,
      '--theme-settings-menu-box-border': `1px solid ${primary.border}`,
      '--theme-settings-menu-item-text-color': `${primary.text}`,
      '--theme-settings-menu-item-text-color-active': `${primary.text}`,
      '--theme-settings-menu-item-text-color-disabled': `${primary.disabled}`,
      '--theme-settings-menu-item-background-color-active': `${primary.hover}`,
      '--theme-settings-menu-item-left-border-color-active': `${
        secondary.background.regular
      }`,
      '--theme-settings-theme-select-title-color': `${primary.text}`,
    },
    sidebar: {
      '--theme-sidebar-background-color': `${secondary.background.regular}`,
      '--theme-sidebar-category-background-color-hover': `${
        secondary.background.dark
      }`,
      '--theme-sidebar-category-background-color-active': `${
        secondary.background.darker
      }`,
      '--theme-sidebar-category-text-color': `${secondary.text}`,
      '--theme-sidebar-menu-background-color': `${secondary.background.darker}`,
      '--theme-sidebar-menu-item-background-color-hover': `${
        secondary.background.darkest
      }`,
      '--theme-sidebar-menu-item-background-color-active': `${
        secondary.background.darkest
      }`, // rename to active wallet?
      '--theme-sidebar-menu-item-wallet-name-color': `${secondary.text}`,
      '--theme-sidebar-menu-item-wallet-info-color': `${secondary.text}`,
      '--theme-sidebar-menu-add-button-background-color': `${
        secondary.background.darkest
      }`,
      '--theme-sidebar-menu-add-button-background-color-active': `${
        secondary.background.dark
      }`,
      '--theme-sidebar-menu-add-button-background-color-hover': `${
        secondary.background.dark
      }`,
      '--theme-sidebar-menu-add-button-text-color': `${secondary.text}`,
    },
    staking: {
      '--theme-staking-background-color': `${primary.background.regular}`,
      '--theme-staking-content-background-color': `${
        primary.background.regular
      }`,
      '--theme-staking-content-border-color': `${primary.border}`,
      '--theme-staking-font-color-accent': `${primary.focus}`,
      '--theme-staking-font-color-regular': `${primary.text}`,
    },
    support: {
      '--theme-support-settings-text-color': `${primary.text}`,
      '--theme-support-settings-link-color': `${secondary.background.regular}`,
      '--theme-support-settings-item-color': `${primary.text}`,
    },
    syncing: {
      '--theme-syncing-background-color': `${primary.background.regular}`,
      '--theme-syncing-text-color': `${primary.text}`,
    },
    systemError: {
      '--theme-system-error-overlay-attention-icon-color': `${secondary.text}`,
      '--theme-system-error-overlay-background-color': `${error.regular}`,
      '--theme-system-error-overlay-text-color': `${secondary.text}`,
    },
    tabs: {
      '--theme-choice-tabs-text-color': `${primary.text}`,
      '--theme-choice-tabs-text-color-active': `${primary.text}`,
      '--theme-choice-tabs-bottom-border-color-active': `${primary.text}`,
    },
    testEnvironment: {
      '--theme-test-environment-label-background-color': '#ab1700',
      '--theme-test-environment-label-text-color': `${secondary.text}`,
    },
    topBar: {
      '--theme-topbar-background-color': `${secondary.background.darkest}`,
      '--theme-topbar-wallet-name-color': `${secondary.text}`,
      '--theme-topbar-wallet-info-color': `${secondary.text}`,
      '--theme-topbar-layout-body-background-color': `${
        secondary.background.regular
      }`,
    },
    transactions: {
      '--theme-transactions-list-background-color': `${
        primary.background.lightest
      }`,
      '--theme-transactions-list-border-color': `${primary.border}`,
      '--theme-transactions-list-group-date-color': `${primary.text}`,
      '--theme-transactions-list-item-details-color': `${primary.text}`,
      '--theme-transactions-state-failed-background-color': `${
        primary.disabled
      }`,
      '--theme-transactions-state-failed-text-color': `${primary.disabled}`,
      '--theme-transactions-state-pending-background-color': `${
        primary.placeholder
      }`,
      '--theme-transactions-state-pending-stripes-color': `${
        primary.placeholder
      }`,
      '--theme-transactions-priority-color': `${primary.background.regular}`,
      '--theme-transactions-priority-low-background-color': `${error.dark}`,
      '--theme-transactions-priority-medium-background-color': '#e6aa00',
      '--theme-transactions-priority-high-background-color': '#007600',
      '--theme-transactions-search-background-color': `${
        primary.background.regular
      }`,
      '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
      '--theme-transactions-icon-type-income-background-color': '#2dc06c',
      '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
      '--theme-transactions-icon-type-failed-background-color': `${
        error.light
      }`,
      '--theme-transactions-arrow-stroke-color': `${primary.text}`,
    },
    uploader: {
      '--theme-uploader-text-color': `${primary.text}`,
      '--theme-uploader-border-color': `${primary.border}`,
    },
    utxo: {
      '--theme-utxo-background-color': `${chroma(
        primary.background.regular
      ).alpha(0.5)}`,
      '--theme-utxo-title-text-color': `${primary.text}`,
      '--theme-utxo-title-description-color': `${chroma(primary.text).alpha(
        0.7
      )}`,
      '--theme-utxo-bar-color': `${chroma(secondary.background.dark).alpha(
        0.5
      )}`,
      '--theme-utxo-label-text-color': `${chroma(primary.text).alpha(0.45)}`,
      '--theme-utxo-tick-text-color': `${chroma(primary.text).alpha(0.45)}`,
      '--theme-utxo-cursor-background-color': `${chroma(
        secondary.background.lightest
      ).alpha(0.2)}`,
      '--theme-utxo-tooltip-background-color': `${chroma(
        primary.background.darkest
      )}`,
      '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
      '--theme-utxo-tooltip-text-color': `${secondary.text}`,
    },
  };
};

export const createTheme = (fullThemeParts: CreateThemeParams): Object => {
  const { colors: themeColors, config, fonts } = fullThemeParts;

  const colors = {
    ...themeColors,
    error: createErrorShades(themeColors.error),
    primary: {
      ...themeColors.primary,
      background: createBackgroundShades(themeColors.primary.background),
    },
    secondary: {
      ...themeColors.secondary,
      background: createBackgroundShades(themeColors.secondary.background),
    },
  };

  // create react-polymorph & daedalus theme, combine into a theme object
  let daedalusTheme = {
    ...createReactPolymorphTheme({ colors, fonts }),
    ...createDaedalusComponentsTheme({ colors, fonts }),
  };

  // flatten daedalusTheme object for consumption by ThemeManager
  daedalusTheme = Object.values(daedalusTheme).reduce(
    (theme, componentVars) => ({ ...theme, ...componentVars }),
    {}
  );

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
