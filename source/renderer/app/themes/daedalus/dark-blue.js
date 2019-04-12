// @flow
import type { CreateThemeParams } from '../types';

//  ==== react-polymorph: theme config === //
// AUTOCOMPLETE
const rpAutocomplete = {
  '--rp-autocomplete-bg-color': '#263345',
  '--rp-autocomplete-border': '1px solid rgba(102, 122, 138, 0.3)',
  '--rp-autocomplete-border-color-opened': '#667a8a',
  '--rp-autocomplete-input-text-color': '#e9f4fe',
  '--rp-autocomplete-placeholder-color': '#8793a1',
  '--rp-autocomplete-selected-word-box-bg-color': 'rgba(83, 99, 112, 0.5)',
  '--rp-autocomplete-selected-word-text-color': '#fafbfc',
  '--rp-autocomplete-selected-words-font-family':
    'NotoSans-Regular, NotoSansCJKjp-Regular',
};

// BUBBLE
const rpBubble = {
  '--rp-bubble-bg-color': '#263345',
  '--rp-bubble-border-color': '#667a8a',
  '--rp-bubble-border-radius': '2px',
};

// BUTTON
const rpButton = {
  '--rp-button-bg-color': '#536370',
  '--rp-button-bg-color-active': '#495863',
  '--rp-button-bg-color-disabled': 'rgba(233, 244, 254, 0.5)',
  '--rp-button-bg-color-hover': '#667a8a',
  '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-button-height': '50px',
  '--rp-button-line-height': '20px',
  '--rp-button-padding': '0',
  '--rp-button-text-color': '#e9f4fe',
  '--rp-button-text-transform': 'none',
  '--rp-button-width': '360px',
};

// CHECKBOX
const rpCheckbox = {
  '--rp-checkbox-border': '1px solid #536370',
  '--rp-checkbox-border-color-disabled': 'rgba(83, 99, 112, 0.4)',
  '--rp-checkbox-check-bg-color': '#536370',
  '--rp-checkbox-label-text-color': '#e9f4fe',
  '--rp-checkbox-label-text-color-disabled': 'rgba(233, 244, 254, 0.3)',
};

// COLORS
const rpColors = {
  '--rp-theme-color-error': '#ea4c5b',
};

// FONTS
const rpFonts = {
  '--rp-theme-font-thin': 'NotoSans-Thin, NotoSansCJKjp-Thin',
  '--rp-theme-font-light': 'NotoSans-Light, NotoSansCJKjp-Light',
  '--rp-theme-font-medium': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-theme-font-regular': 'NotoSans-Regular, NotoSansCJKjp-Regular',
  '--rp-theme-font-bold': 'NotoSans-Bold, NotoSansCJKjp-Bold',
};

// FORMFIELD
const rpFormfield = {
  '--rp-formfield-bg-color-disabled': 'none',
  '--rp-formfield-label-text-color': '#e9f4fe',
  '--rp-formfield-label-text-color-disabled': '#e9f4fe',
  '--rp-formfield-error-text-color': '#ea4c5b',
  '--rp-formfield-error-text-opacity': '0.75',
};

// INPUT
const rpInput = {
  '--rp-input-bg-color': '#263345',
  '--rp-input-bg-color-disabled': 'rgba(135, 147, 161, 0.1)',
  '--rp-input-border-color': 'rgba(102, 122, 138, 0.3)',
  '--rp-input-border-color-disabled': 'rgba(135, 147, 161, 0.1)',
  '--rp-input-border-color-errored': '#ea4c5b',
  '--rp-input-border-color-focus': '#667a8a',
  '--rp-input-line-height': '22px',
  '--rp-input-padding': '12px 20px',
  '--rp-input-placeholder-color': '#8793a1',
  '--rp-input-placeholder-color-disabled': '#8793a1',
  '--rp-input-text-color': '#e9f4fe',
  '--rp-input-text-color-disabled': '#8793a1',
};

// MODAL
const rpModal = {
  '--rp-modal-bg-color': '#263345',
  '--rp-modal-max-height': '90%',
  '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
};

// OPTIONS
const rpOptions = {
  '--rp-option-bg-color': '#263345',
  '--rp-option-bg-color-highlighted': '#536370',
  '--rp-option-border-color': '#667a8a',
  '--rp-option-checkmark-color': '#fff',
  '--rp-option-line-height': '22px',
  '--rp-option-text-color': '#e9f4fe',
  '--rp-options-border-color': '#667a8a',
  '--rp-options-shadow':
    '0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 0 4px 0 rgba(0, 0, 0, 0.1)',
};

// SELECT
const rpSelect = {
  '--rp-select-arrow-bg-color': '#606d7b',
  '--rp-select-arrow-bg-color-open': '#e9f4fe',
  '--rp-select-input-bg-color': '#263345',
  '--rp-select-input-border-color': 'rgba(102, 122, 138, 0.3)',
  '--rp-select-input-border-color-focus': '#667a8a',
  '--rp-select-input-text-color': '#e9f4fe',
};

// SWITCH
const rpSwitch = {
  '--rp-switch-bg-color-off': '#536370',
  '--rp-switch-bg-color-on': '#536370',
  '--rp-switch-label-margin': '0 30px 0 0',
  '--rp-switch-label-opacity': '0.5',
  '--rp-switch-label-text-color': '#e9f4fe',
  '--rp-switch-label-width': '100%',
  '--rp-switch-opacity-off': '0.3',
  '--rp-switch-root-margin': '0 0 30px 0',
  '--rp-switch-thumb-bg-color': '#fff',
};

// TEXTAREA
const rpTextArea = {
  '--rp-textarea-bg-color': '#263345',
  '--rp-textarea-bg-color-disabled': 'rgba(135, 147, 161, 0.1)',
  '--rp-textarea-border': '1px solid rgba(102, 122, 138, 0.3)',
  '--rp-textarea-border-color-disabled': 'rgba(135, 147, 161, 0.1)',
  '--rp-textarea-border-color-errored': '#ea4c5b',
  '--rp-textarea-border-color-focus': '#667a8a',
  '--rp-textarea-border-radius': '2px',
  '--rp-textarea-line-height': '20px',
  '--rp-textarea-placeholder-color': '#8793a1',
  '--rp-textarea-resize': 'none',
  '--rp-textarea-text-color': '#e9f4fe',
};

const rpDarkBlueTheme = {
  ...rpAutocomplete,
  ...rpBubble,
  ...rpButton,
  ...rpCheckbox,
  ...rpColors,
  ...rpFonts,
  ...rpFormfield,
  ...rpInput,
  ...rpModal,
  ...rpOptions,
  ...rpSelect,
  ...rpSwitch,
  ...rpTextArea,
};

//  ==== Dark Blue Theme for Daedalus Components === //
export const DARK_BLUE_THEME_CONFIG = {
  reactPolymorph: {
    ...rpDarkBlueTheme,
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': '#536370',
  },
  adaRedemption: {
    '--theme-ada-redemption-headline-color': '#e9f4fe', // primary.text
    '--theme-ada-redemption-instructions-color': '#e9f4fe', // primary.text
    '--theme-ada-redemption-success-overlay-background-color':
      'rgba(38, 51, 69, 0.88)', // config
    '--theme-ada-redemption-success-overlay-border-color': '#fafbfc', // secondary.text
    '--theme-ada-redemption-success-overlay-message-color': '#e9f4fe', // config
    '--theme-ada-redemption-success-overlay-button-text-color': '#e9f4fe', // config
    '--theme-ada-redemption-success-overlay-button-text-color-hover': '#263345',
    '--theme-ada-redemption-success-overlay-button-background-color-hover':
      '#e9f4fe',
    '--theme-ada-redemption-disclaimer-background-color':
      'rgba(171, 23, 0, 0.94)',
    '--theme-ada-redemption-disclaimer-text-color': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-check': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-checked': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-after': '#ab1700',
    '--theme-ada-redemption-disclaimer-checkbox-label-color': '#fafbfc',
    '--theme-ada-redemption-no-wallets-instructions-color': '#e9f4fe',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#263345',
    '--theme-bordered-box-border': 'none',
    '--theme-bordered-box-text-color': '#e9f4fe',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#536370',
    '--theme-button-primary-background-color-hover': '#667a8a',
    '--theme-button-primary-background-color-active': '#495863',
    '--theme-button-primary-background-color-disabled':
      'rgba(83, 99, 112, 0.3)',
    '--theme-button-primary-text-color-disabled': 'rgba(233, 244, 254, 0.5)',
    '--theme-button-primary-text-color': '#e9f4fe',
    '--theme-button-primary-outline-color': '#667a8a',
    '--theme-label-button-color': 'rgba(233, 244, 254, 0.5)',
  },
  buttonFlat: {
    '--theme-button-flat-background-color': 'rgba(83, 99, 112, 0.3)',
    '--theme-button-flat-background-color-hover': 'rgba(102, 122, 138, 0.3)',
    '--theme-button-flat-background-color-active': 'rgba(73, 88, 99, 0.3)',
    '--theme-button-flat-background-color-disabled': 'rgba(83, 99, 112, 0.1)',
    '--theme-button-flat-text-color-disabled': 'rgba(233, 244, 254, 0.3)',
    '--theme-button-flat-text-color': '#e9f4fe',
    '--theme-button-flat-outline-color': 'rgba(102,122,138, 0.3)',
  },
  buttonAttention: {
    '--theme-button-attention-background-color': '#ea4c5b',
    '--theme-button-attention-background-color-hover': '#ec5d6b',
    '--theme-button-attention-background-color-active': '#d34452',
    '--theme-button-attention-background-color-disabled':
      'rgba(234, 76, 91, 0.3)',
    '--theme-button-attention-text-color-disabled': '#fafbfc',
    '--theme-button-attention-text-color': '#fafbfc',
    '--theme-button-attention-outline-color': '#ff707e',
  },
  buttonDisclaimer: {
    '--theme-button-disclaimer-background-color': '#ab1700',
    '--theme-button-disclaimer-background-color-hover': '#fafbfc',
    '--theme-button-disclaimer-background-color-active': '#fafbfc',
    '--theme-button-disclaimer-background-color-disabled':
      'rgba(171, 23, 0, .3)',
    '--theme-button-disclaimer-text-color-disabled': 'rgba(250, 251, 252, .3)',
    '--theme-button-disclaimer-text-color': '#fafbfc',
    '--theme-button-disclaimer-outline-color': 'rgba(250, 251, 252, .3)',
    '--theme-button-disclaimer-border-color': '#fafbfc',
    '--theme-button-disclaimer-border-color-disabled':
      'rgba(250, 251, 252, .3)',
  },
  tabs: {
    '--theme-choice-tabs-text-color': '#e9f4fe',
    '--theme-choice-tabs-text-color-active': '#e9f4fe',
    '--theme-choice-tabs-bottom-border-color-active': '#e9f4fe',
  },
  dialog: {
    '--theme-dialog-choice-tabs-text-color': '#e9f4fe',
    '--theme-dialog-choice-tabs-text-color-active': '#e9f4fe',
    '--theme-dialog-choice-tabs-bottom-border-color-active': '#e9f4fe',
    '--theme-dialog-big-button-background-color': '#536370',
    '--theme-dialog-big-button-border-color': '#536370',
    '--theme-dialog-big-button-label-color': '#e9f4fe',
    '--theme-dialog-big-button-description-color': 'rgba(233, 244, 254, 0.6)',
    '--theme-dialog-title-color': '#e9f4fe',
    '--theme-dialog-text-color': '#e9f4fe',
    '--theme-dialog-border-color': '#334152',
  },
  input: {
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-border-color': 'rgba(102, 122, 138, 0.3)',
    '--theme-input-label-color': '#e9f4fe',
    '--theme-input-text-color': '#e9f4fe',
    '--theme-input-right-floating-text-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-input-placeholder-color': '#8793a1',
    '--theme-input-error-color': '#ea4c5b',
    '--theme-input-remove-color-dark': '#d34452',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-remove-color-lighter': '#ec5d6b',
    '--theme-input-remove-color-lightest': '#393546',
    '--theme-input-background-color': '#263345',
    '--theme-input-disabled-background-color': 'rgba(135, 147, 161, 0.1)',
    '--theme-input-disabled-border-color': 'rgba(135, 147, 161, 0.1)',
    '--theme-input-focus-border-color': '#667a8a',
  },
  body: {
    '--theme-main-body-background-color': '#0b1926',
    '--theme-main-body-messages-color': '#e9f4fe',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navItem: {
    '--theme-nav-item-background-color': '#263345',
    '--theme-nav-item-background-color-hover': 'rgba(83, 99, 112, 0.1)',
    '--theme-nav-item-background-color-active': '#536370',
    '--theme-nav-item-text-color': '#9ba6b3',
    '--theme-nav-item-text-color-active': '#e9f4fe',
  },
  nodeUpdate: {
    '--theme-node-update-background-color': '#536370',
    '--theme-node-update-title-color': '#e9f4fe',
    '--theme-node-update-message-color': '#e9f4fe',
    '--theme-node-sync-info-message-background-color': '#536370',
    '--theme-node-sync-info-message-text-color': '#e9f4fe',
    '--theme-node-update-accept-button-background-color': '#263345',
    '--theme-node-update-accept-button-background-color-hover': '#34465e',
    '--theme-node-update-accept-button-background-color-active': '#18202b',
    '--theme-node-update-deny-button-background-color': 'rgba(38, 51, 69, 0.3)',
    '--theme-node-update-deny-button-background-color-hover':
      'rgba(52, 70, 94, 0.3)',
    '--theme-node-update-deny-button-background-color-active':
      'rgba(24, 32, 43, 0.3)',
    '--theme-node-update-button-text-color': '#fafbfc',
  },
  notification: {
    '--theme-notification-message-background-color': 'rgba(83, 99, 112, 0.88)',
    '--theme-notification-message-text-color': '#fafbfc',
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': '#fff',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  paperWallet: {
    '--theme-paper-wallet-create-certificate-dialog-recovery-phrase-background-color':
      'rgba(233, 244, 254, 0.05)',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-color':
      '#e9f4fe',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(233, 244, 254, 0.05)',
  },
  settings: {
    '--theme-settings-body-background-color': '#0b1926',
    '--theme-settings-pane-background-color': '#263345',
    '--theme-settings-pane-border': 'none',
    '--theme-settings-menu-box-background-color': '#263345',
    '--theme-settings-menu-box-border': 'none',
    '--theme-settings-menu-item-text-color': '#cecfd1',
    '--theme-settings-menu-item-text-color-active': '#cecfd1',
    '--theme-settings-menu-item-text-color-disabled': '#7a8691',
    '--theme-settings-menu-item-background-color-active': '#536370',
    '--theme-settings-menu-item-left-border-color-active': '#0b1926',
    '--theme-settings-theme-select-title-color': '#cecfd1',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#314259',
    '--theme-sidebar-category-background-color-hover': 'rgba(38, 51, 69, 0.5)',
    '--theme-sidebar-category-background-color-active': '#263345',
    '--theme-sidebar-category-text-color': '#e9f4fe',
    '--theme-sidebar-menu-background-color': '#263345',
    '--theme-sidebar-menu-item-background-color-hover': 'rgba(27, 36, 48, 0.5)',
    '--theme-sidebar-menu-item-background-color-active': '#1b2430',
    '--theme-sidebar-menu-item-wallet-name-color': '#e9f4fe',
    '--theme-sidebar-menu-item-wallet-info-color': '#bdc0c1',
    '--theme-sidebar-menu-add-button-background-color': '#1b2430',
    '--theme-sidebar-menu-add-button-background-color-active': '#1f2a38',
    '--theme-sidebar-menu-add-button-background-color-hover': '#1f2a38',
    '--theme-sidebar-menu-add-button-text-color': '#e9f4fe',
  },
  staking: {
    '--theme-staking-background-color': '#0b1926',
    '--theme-staking-content-background-color': '#263345',
    '--theme-staking-content-border-color': '#263345',
    '--theme-staking-font-color-accent': '#cecfd1',
    '--theme-staking-font-color-regular': '#0b1926',
  },
  systemError: {
    '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
    '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-system-error-overlay-text-color': '#fafbfc',
  },
  testEnvironment: {
    '--theme-test-environment-label-background-color': '#ab1700',
    '--theme-test-environment-label-text-color': '#fafbfc',
  },
  topBar: {
    '--theme-topbar-background-color': '#263345',
    '--theme-topbar-wallet-name-color': '#e9f4fe',
    '--theme-topbar-wallet-info-color': '#e9f4fe',
    '--theme-topbar-layout-body-background-color': '#0b1926',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#263345',
    '--theme-transactions-list-border-color': '#263345',
    '--theme-transactions-list-group-date-color': '#7a8691',
    '--theme-transactions-list-item-details-color': '#e9f4fe',
    '--theme-transactions-card-background-color': '#e6ebf2',
    '--theme-transactions-card-income-background-color': '#e6ebf2',
    '--theme-transactions-card-expend-background-color': '#f2e6e6',
    '--theme-transactions-card-exchange-background-color': '#f2e6e6',
    '--theme-transactions-state-failed-background-color':
      'rgba(189, 197, 206, 0.4)',
    '--theme-transactions-state-failed-text-color': 'rgba(233, 244, 254, 0.4)',
    '--theme-transactions-state-pending-background-color':
      'rgba(189, 197, 206, 0.4)',
    '--theme-transactions-state-pending-stripes-color':
      'rgba(78, 98, 128, 0.5)',
    '--theme-transactions-priority-color': 'rgba(250, 251, 252, 0.8)',
    '--theme-transactions-priority-low-background-color': '#542A2B',
    '--theme-transactions-priority-medium-background-color': '#706233',
    '--theme-transactions-priority-heigh-background-color': '#274C2D',
    '--theme-transactions-search-background-color': '#fafbfc',
    '--theme-transactions-icon-type-expend-background-color': '#4f5f7a',
    '--theme-transactions-icon-type-income-background-color': '#3b6956',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-icon-type-failed-background-color':
      'rgba(234, 76, 91, 0.5)',
    '--theme-transactions-arrow-stroke-color': '#e9f4fe',
  },
  uploader: {
    '--theme-uploader-text-color': '#e9f4fe',
    '--theme-uploader-border-color': 'rgba(102, 122, 138, 0.3)',
  },
  icon: {
    '--theme-icon-nav-color': '#9ba6b3',
    '--theme-icon-nav-color-active': '#e9f4fe',
    '--theme-icon-sidebar-color': '#fafbfc',
    '--theme-icon-toggle-menu-color': '#fafbfc',
    '--theme-icon-node-update-notification-arrow-color': '#e9f4fe',
    '--theme-icon-add-wallet-from-sidebar-color': '#fafbfc',
    '--theme-icon-ada-redemption-attention-color': '#fafbfc',
    '--theme-icon-ada-redemption-success-color': '#fafbfc',
    '--theme-icon-ada-redemption-certificate-color': '#9eabbb',
    '--theme-icon-ada-redemption-no-wallets': '#e9f4fe',
    '--theme-icon-ada-summary-wallet-amount-symbol-color': '#e9f4fe',
    '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color':
      '#e9f4fe',
    '--theme-icon-add-wallet-dialog-big-button-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-icon-copy-address-color': '#8793a1',
    '--theme-icon-back-button-color': '#e9f4fe',
    '--theme-icon-close-button-color': '#e9f4fe',
    '--theme-icon-file-upload-color': '#8793a1',
    '--theme-icon-transactions-ada-symbol-color': '#e9f4fe',
    '--theme-icon-syncing-logo-color': '#e9f4fe',
    '--theme-icon-connecting-logo-color': '#fafbfc',
    '--theme-icon-transaction-type-color': '#fafbfc',
  },
  aboutWindow: {
    '--theme-about-window-background-color': 'rgba(36, 62, 98, 0.96)',
    '--theme-about-window-header-bottom-border-color':
      'rgba(233, 244, 254, 0.3)',
    '--theme-about-window-daedalus-icon-color': '#e9f4fe',
    '--theme-about-window-cardano-icon-color': '#e9f4fe',
    '--theme-about-window-title-varsion-color': '#e9f4fe',
    '--theme-about-window-title-stroke-color': '#e9f4fe',
    '--theme-about-window-content-color': '#fafbfc',
    '--theme-about-window-content-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-about-window-content-bottom-border-color':
      'rgba(233, 244, 254, 0.3)',
    '--theme-about-window-icon-close-button-color': '#fff',
    '--theme-about-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.2)',
  },
  mnemonic: {
    '--theme-backup-mnemonic-background-color': 'rgba(233, 244, 254, 0.05)',
    '--theme-mnemonic-background-color': 'rgba(83, 99, 112, 0.3)',
    '--theme-mnemonic-background-color-hover': 'rgba(102, 122, 138, 0.3)',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#536370',
    '--theme-report-issue-button-background-color-hover': '#667a8a',
    '--theme-report-issue-button-background-color-active': '#495863',
    '--theme-report-issue-connecting-background-color':
      'rgba(250, 251, 252, 0.05)',
    '--theme-report-issue-connecting-text-color': '#e9f4fe',
    '--theme-report-issue-syncing-background-color':
      'rgba(250, 251, 252, 0.05)',
    '--theme-report-issue-syncing-text-color': '#e9f4fe',
    '--theme-report-issue-syncing-download-logs-text-color': '#e9f4fe',
    '--theme-report-issue-syncing-download-logs-text-color-connecting':
      '#e9f4fe',
  },
  connecting: {
    '--theme-connecting-background-color': '#263345',
    '--theme-connecting-text-color': '#fff',
  },
  syncing: {
    '--theme-syncing-background-color': '#0b1926',
    '--theme-syncing-text-color': '#e9f4fe',
  },
  loading: {
    '--theme-loading-background-color': '#0b1926',
    '--theme-loading-no-disk-space-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-loading-no-disk-space-text-color': '#fafbfc',
    '--theme-loading-no-disk-space-attention-icon-color': '#fafbfc',
    '--theme-loading-status-icons-on-color': '#2dc06c',
    '--theme-loading-status-icons-off-color': '#ea4c5b',
    '--theme-loading-status-icons-unloaded-loading-color': '#fafbfc',
    '--theme-loading-status-icons-unloaded-syncing-color': '#fafbfc',
    '--theme-loading-status-icons-tooltip-color': '#4b5a68',
    '--theme-loading-spinner-color': '#e9f4fe',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': '#243E62',
    '--theme-data-migration-layer-box-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-data-migration-layer-button-background-color-hover': '#fafbfc',
    '--theme-data-migration-layer-text-color': '#fafbfc',
    '--theme-data-migration-layer-text-opacity-color':
      'rgba(250, 251, 252, 0.5)',
  },
  support: {
    '--theme-support-settings-text-color': '#e9f4fe',
    '--theme-support-settings-link-color': '#20b56b',
    '--theme-support-settings-item-color': 'rgba(233, 244, 254, 0.5)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': 'rgba(233, 244, 254, 0.3)',
    '--theme-progress-bar-foreground-color': 'rgba(233, 244, 254, 0.7)',
  },
  blockConsolidation: {
    '--theme-block-consolidation-background-color': 'rgba(38, 51, 69, 0.96)',
    '--theme-block-consolidation-container-background-color': '#fafbfc',
    '--theme-block-consolidation-title-text-color': '#fafbfc',
    '--theme-block-consolidation-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-block-consolidation-text-highlight-color': '#fafbfc',
    '--theme-block-consolidation-epochs-text-color': '#263345',
    '--theme-block-consolidation-indicator-text-color': '#fafbfc',
    '--theme-block-consolidation-indicator-container-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-1':
      '#fafbfc',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-2':
      'rgba(250, 251, 252, 0)',
    '--theme-block-consolidation-stripe-dark-1-background-color':
      'rgba(224, 229, 234, 0.3)',
    '--theme-block-consolidation-stripe-dark-2-background-color':
      'rgba(250, 251, 252, 0.3)',
    '--theme-block-consolidation-stripe-light-1-background-color': '#e0e5ea',
    '--theme-block-consolidation-stripe-light-2-background-color': '#fafbfc',
    '--theme-block-consolidation-button-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-block-consolidation-button-background-color-hover': '#fafbfc',
    '--theme-block-consolidation-button-text-color-hover': '#263345',
    '--theme-block-consolidation-button-border-color': '#fafbfc',
  },
  errors: {
    '--theme-color-error': '#ea4c5b',
  },
};

export const DARK_BLUE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: {
      dark: '#d34452',
      light: '#ec5d6b',
      regular: '#ea4c5b',
      ultralight: '#fac8ce',
    },
    primary: {
      active: '#495863',
      background: '#263345',
      border: 'rgba(102, 122, 138, 0.3)',
      disabled: 'rgba(233, 244, 254, 0.5)',
      focus: '#667a8a',
      hover: '#667a8a',
      outline: 'rgba(102,122,138, 0.3)',
      placeholder: '#8793a1',
      text: '#e9f4fe',
    },
    secondary: {
      active: '#495863',
      background: '#536370',
      border: 'rgba(250, 251, 252, .3)',
      disabled: 'rgba(135, 147, 161, 0.1)',
      focus: '#667a8a',
      hover: '#667a8a',
      outline: 'rgba(250, 251, 252, .3)',
      placeholder: '#e9f4fe',
      text: '#fafbfc',
    },
  },
  fonts: {
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
  },
  config: DARK_BLUE_THEME_CONFIG,
};
