// @flow
import chroma from 'chroma-js';
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

//  ==== react-polymorph: theme config === //
// AUTOCOMPLETE
const rpAutocomplete = {
  '--rp-autocomplete-bg-color': '#fafbfc',
  '--rp-autocomplete-border': '1px solid #c6cdd6',
  '--rp-autocomplete-border-color-opened': '#5e6066',
  '--rp-autocomplete-input-text-color': '#5e6066',
  '--rp-autocomplete-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-autocomplete-selected-word-box-bg-color': 'rgba(44, 187, 105, 0.9)',
  '--rp-autocomplete-selected-word-text-color': '#fafbfc',
  '--rp-autocomplete-selected-words-font-family':
    'NotoSans-Regular, NotoSansCJKjp-Regular',
};

// BUBBLE
const rpBubble = {
  '--rp-bubble-bg-color': '#fafbfc',
  '--rp-bubble-border-color': '#c6cdd6',
  '--rp-bubble-border-radius': '2px',
};

// BUTTON
const rpButton = {
  '--rp-button-bg-color': '#2cbb69',
  '--rp-button-bg-color-active': '#239554',
  '--rp-button-bg-color-disabled': 'rgba(44, 187, 105, 0.3)',
  '--rp-button-bg-color-hover': '#56c887',
  '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-button-font-size': '14px',
  '--rp-button-height': '50px',
  '--rp-button-line-height': '20px',
  '--rp-button-padding': '0',
  '--rp-button-text-color': '#fafbfc',
  '--rp-button-text-color-disabled': '#fafbfc',
  '--rp-button-text-transform': 'none',
  '--rp-button-width': '360px',
};

// CHECKBOX
const rpCheckbox = {
  '--rp-checkbox-border': '1px solid #2cbb69',
  '--rp-checkbox-border-color-disabled': 'rgba(44, 187, 105, 0.2)',
  '--rp-checkbox-check-bg-color': '#2cbb69',
  '--rp-checkbox-label-text-color': '#5e6066',
  '--rp-checkbox-label-text-color-disabled': 'rgba(94, 96, 102, 0.3)',
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
  '--rp-formfield-label-text-color': '#5e6066',
  '--rp-formfield-label-text-color-disabled': '#5e6066',
  '--rp-formfield-error-text-color': '#ea4c5b',
  '--rp-formfield-error-text-opacity': '0.75',
};

// INPUT
const rpInput = {
  '--rp-input-bg-color': '#fafbfc',
  '--rp-input-bg-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-input-border-color': '#c6cdd6',
  '--rp-input-border-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-input-border-color-errored': '#ea4c5b',
  '--rp-input-border-color-focus': '#5e6066',
  '--rp-input-line-height': '22px',
  '--rp-input-padding': '12px 20px',
  '--rp-input-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-placeholder-color-disabled': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-text-color': '#5e6066',
  '--rp-input-text-color-disabled': 'rgba(94, 96, 102, 0.5)',
};

// MODAL
const rpModal = {
  '--rp-modal-bg-color': '#fafbfc',
  '--rp-modal-max-height': '90%',
  '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
};

// OPTIONS
const rpOptions = {
  '--rp-option-bg-color': '#fafbfc',
  '--rp-option-bg-color-highlighted': '#edeeef',
  '--rp-option-border-color': '#c6cdd6',
  '--rp-option-checkmark-color': '#5e6066',
  '--rp-option-line-height': '22px',
  '--rp-option-text-color': '#5e6066',
  '--rp-options-border-color': '#c6cdd6',
  '--rp-options-shadow': 'none',
};

// SELECT
const rpSelect = {
  '--rp-select-arrow-bg-color': '#c6cdd6',
  '--rp-select-arrow-bg-color-open': '#5e6066',
  '--rp-select-input-bg-color': '#fafbfc',
  '--rp-select-input-border-color': '#c6cdd6',
  '--rp-select-input-border-color-focus': '#5e6066',
  '--rp-select-input-text-color': '#5e6066',
};

// SWITCH
const rpSwitch = {
  '--rp-switch-bg-color-off': '#2cbb69',
  '--rp-switch-bg-color-on': '#2cbb69',
  '--rp-switch-label-margin': '0 30px 0 0',
  '--rp-switch-label-opacity': '0.5',
  '--rp-switch-label-text-color': '#5e6066',
  '--rp-switch-label-width': '100%',
  '--rp-switch-opacity-off': '0.3',
  '--rp-switch-root-margin': '0 0 30px 0',
  '--rp-switch-thumb-bg-color': '#fff',
};

// TEXTAREA
const rpTextArea = {
  '--rp-textarea-bg-color': '#fafbfc',
  '--rp-textarea-bg-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-textarea-border': '1px solid #c6cdd6',
  '--rp-textarea-border-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-textarea-border-color-errored': '#ea4c5b',
  '--rp-textarea-border-color-focus': '#5e6066',
  '--rp-textarea-border-radius': '2px',
  '--rp-textarea-line-height': '20px',
  '--rp-textarea-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-textarea-resize': 'none',
  '--rp-textarea-text-color': '#5e6066',
};

const rpCardanoTheme = {
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

//  ==== Cardano Theme for Daedalus Components === //
const CARDANO_THEME_CONFIG = {
  aboutWindow: {
    '--theme-about-window-background-color': 'rgba(32, 34, 37, 0.96)',
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
  adaRedemption: {
    '--theme-ada-redemption-headline-color': '#5e6066',
    '--theme-ada-redemption-instructions-color': '#5e6066',
    '--theme-ada-redemption-success-overlay-background-color':
      'rgba(44, 187, 105, 0.88)',
    '--theme-ada-redemption-success-overlay-border-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-message-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-button-text-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-button-text-color-hover': '#2cbb69',
    '--theme-ada-redemption-success-overlay-button-background-color-hover':
      '#fafbfc',
    '--theme-ada-redemption-disclaimer-background-color':
      'rgba(171, 23, 0, 0.94)',
    '--theme-ada-redemption-disclaimer-text-color': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-check': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-checked': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-after': '#ab1700',
    '--theme-ada-redemption-disclaimer-checkbox-label-color': '#fafbfc',
    '--theme-ada-redemption-no-wallets-instructions-color': '#5e6066',
  },
  blockConsolidation: {
    '--theme-block-consolidation-background-color': 'rgba(32, 34, 37, 0.96)',
    '--theme-block-consolidation-title-text-color': '#fafbfc',
    '--theme-block-consolidation-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-block-consolidation-text-highlight-color': '#fafbfc',
    '--theme-block-consolidation-epochs-text-color': '#202225',
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
    '--theme-block-consolidation-button-text-color-hover': '#202225',
    '--theme-block-consolidation-button-border-color': '#fafbfc',
  },
  body: {
    '--theme-main-body-background-color': '#efefef',
    '--theme-main-body-messages-color': '#5e6066',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#fafbfc',
    '--theme-bordered-box-border': '1px solid #c6cdd6',
    '--theme-bordered-box-text-color': '#5e6066',
  },
  button: {
    '--theme-label-button-color': '#5e6066',
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
  buttonFlat: {
    '--theme-button-flat-background-color': '#f0f3f5',
    '--theme-button-flat-background-color-hover': '#f3f5f7',
    '--theme-button-flat-background-color-active': '#ebeef0',
    '--theme-button-flat-background-color-disabled': 'rgba(240, 243, 245, 0.3)',
    '--theme-button-flat-text-color-disabled': '#5e6066',
    '--theme-button-flat-text-color': '#5e6066',
    '--theme-button-flat-outline-color': '#e1e4e6',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#2cbb69',
    '--theme-button-primary-background-color-hover': '#56c887',
    '--theme-button-primary-background-color-active': '#239554',
    '--theme-button-primary-background-color-disabled':
      'rgba(44, 187, 105, 0.3)',
    '--theme-button-primary-text-color-disabled': '#fafbfc',
    '--theme-button-primary-text-color': '#fafbfc',
    '--theme-button-primary-outline-color': '#35de7e',
  },
  connecting: {
    '--theme-connecting-background-color': '#202225',
    '--theme-connecting-text-color': '#fff',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': '#243E62',
    '--theme-data-migration-layer-box-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-data-migration-layer-button-background-color-hover': '#fafbfc',
    '--theme-data-migration-layer-text-color': '#fafbfc',
    '--theme-data-migration-layer-text-opacity-color':
      'rgba(250, 251, 252, 0.5)',
  },
  dialog: {
    '--theme-dialog-choice-tabs-text-color': '#5e6066',
    '--theme-dialog-choice-tabs-text-color-active': '#5e6066',
    '--theme-dialog-choice-tabs-bottom-border-color-active': '#5e6066',
    '--theme-dialog-big-button-background-color': '#f5f5f5',
    '--theme-dialog-big-button-border-color': '#e1e1e1',
    '--theme-dialog-big-button-label-color': '#5e6066',
    '--theme-dialog-big-button-description-color': '#5e6066',
    '--theme-dialog-title-color': '#5e6066',
    '--theme-dialog-text-color': '#5e6066',
    '--theme-dialog-border-color': '#dfe4e8',
  },
  errors: {
    '--theme-color-error': '#ea4c5b',
  },
  fonts: {
    '--font-ultralight': 'NotoSans-ExtraLight, NotoSansCJKjp-Thin',
    '--font-thin': 'NotoSans-Thin, NotoSansCJKjp-Thin',
    '--font-light': 'NotoSans-Light, NotoSansCJKjp-Light',
    '--font-regular': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--font-medium': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--font-semibold': 'NotoSans-SemiBold, NotoSansCJKjp-Medium',
    '--font-bold': 'NotoSans-Bold, NotoSansCJKjp-Bold',
    '--font-heavy': 'NotoSans-ExtraBold, NotoSansCJKjp-Black',
    '--font-black': 'NotoSans-Black, NotoSansCJKjp-Black',
    '--font-mono': 'SFMono-Light',
  },
  icon: {
    '--theme-icon-nav-color': '#cecfd1',
    '--theme-icon-nav-color-active': '#5e6066',
    '--theme-icon-sidebar-color': '#fafbfc',
    '--theme-icon-toggle-menu-color': '#fafbfc',
    '--theme-icon-node-update-notification-arrow-color': '#5e6066',
    '--theme-icon-add-wallet-from-sidebar-color': '#fafbfc',
    '--theme-icon-ada-redemption-attention-color': '#fafbfc',
    '--theme-icon-ada-redemption-success-color': '#fafbfc',
    '--theme-icon-ada-redemption-certificate-color': '#9eabbb',
    '--theme-icon-ada-redemption-no-wallets': '#5e6066',
    '--theme-icon-ada-summary-wallet-amount-symbol-color': '#5e6066',
    '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color':
      '#5e6066',
    '--theme-icon-add-wallet-dialog-big-button-color': '#ababab',
    '--theme-icon-copy-address-color': '#5e6066',
    '--theme-icon-back-button-color': '#5e6066',
    '--theme-icon-close-button-color': '#5e6066',
    '--theme-icon-file-upload-color': '#5e6066',
    '--theme-icon-transactions-ada-symbol-color': '#5e6066',
    '--theme-icon-syncing-logo-color': '#5e6066',
    '--theme-icon-connecting-logo-color': '#fafbfc',
    '--theme-icon-transaction-type-color': '#fafbfc',
  },
  input: {
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-border-color': '#c6cdd6',
    '--theme-input-label-color': '#5e6066',
    '--theme-input-text-color': '#5e6066',
    '--theme-input-right-floating-text-color': 'rgba(94, 96, 102, 0.5)',
    '--theme-input-placeholder-color': 'rgba(94, 96, 102, 0.5)',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-background-color': '#fafbfc',
    '--theme-input-focus-border-color': '#5e6066',
  },
  loading: {
    '--theme-loading-background-color': '#fafbfc',
    '--theme-loading-no-disk-space-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-loading-no-disk-space-text-color': '#fafbfc',
    '--theme-loading-no-disk-space-attention-icon-color': '#fafbfc',
    '--theme-loading-status-icons-on-color': '#2dc06c',
    '--theme-loading-status-icons-off-color': '#ea4c5b',
    '--theme-loading-status-icons-unloaded-loading-color': '#fafbfc',
    '--theme-loading-status-icons-unloaded-syncing-color': '#5e6066',
    '--theme-loading-status-icons-tooltip-color': '#5E6066',
    '--theme-loading-spinner-color': '#5e6066',
  },
  manualUpdate: {
    '--theme-manual-update-overlay-background-color': 'rgba(32, 34, 37, 0.96)',
    '--theme-manual-update-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-manual-update-overlay-button-background-color-hover': '#fafbfc',
    '--theme-manual-update-overlay-button-border-color': '#fafbfc',
    '--theme-manual-update-overlay-button-text-color-hover': '#202225',
    '--theme-manual-update-overlay-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-manual-update-overlay-text-highlight-color': '#fafbfc',
    '--theme-manual-update-overlay-title-text-color': '#fafbfc',
  },
  mnemonic: {
    '--theme-backup-mnemonic-background-color': 'rgba(68, 91, 124, 0.05)',
    '--theme-mnemonic-background-color': '#f0f3f5',
    '--theme-mnemonic-background-color-hover': '#f3f5f7',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navDropdown: {
    '--theme-nav-dropdown-item-text-color': '#5e6066',
    '--theme-nav-dropdown-item-background-color': '#fafbfc',
    '--theme-nav-dropdown-item-background-color-hover': '#f1f3f5',
  },
  navItem: {
    '--theme-nav-item-background-color': '#202225',
    '--theme-nav-item-background-color-hover': 'rgba(250, 251, 252, 0.1)',
    '--theme-nav-item-background-color-active': '#fafbfc',
    '--theme-nav-item-text-color': '#cecfd1',
    '--theme-nav-item-text-color-active': '#5e6066',
  },
  network: {
    '--theme-network-window-background-color': 'rgba(32, 34, 37, 0.96)',
    '--theme-network-window-text-color': '#fafbfc',
    '--theme-network-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.2)',
    '--theme-network-window-red-color': '#f06e05',
    '--theme-network-window-green-color': '#05f079',
    '--theme-network-window-white-color': '#fff',
    '--theme-network-window-transparent-color': 'transparent',
    '--theme-network-window-border-color': 'rgba(255, 255, 255, 0.25)',
    '--theme-network-window-button-text-color': '#282c31',
    '--theme-network-window-button-background-color':
      'rgba(250, 251, 252, 0.4)',
    '--theme-network-window-button-background-color-hover':
      'rgba(250, 251, 252, 0.6)',
    '--theme-network-window-button-background-color-active':
      'rgba(250, 251, 252, 0.8)',
  },
  nodeUpdate: {
    '--theme-node-update-background-color': '#efefef',
    '--theme-node-update-title-color': '#5e6066',
    '--theme-node-update-message-color': '#5e6066',
    '--theme-node-sync-info-message-background-color': '#fafbfc',
    '--theme-node-sync-info-message-text-color': '#5e6066',
    '--theme-node-update-accept-button-background-color': '#56c887',
    '--theme-node-update-accept-button-background-color-hover': '#2cbb69',
    '--theme-node-update-accept-button-background-color-active': '#239554',
    '--theme-node-update-deny-button-background-color':
      'rgba(86, 200, 135, 0.3)',
    '--theme-node-update-deny-button-background-color-hover':
      'rgba(44, 187, 105, 0.3)',
    '--theme-node-update-deny-button-background-color-active':
      'rgba(35, 149, 84, 0.3)',
    '--theme-node-update-button-text-color': '#fafbfc',
  },
  notification: {
    '--theme-notification-message-background-color': 'rgba(44, 187, 105, 0.88)',
    '--theme-notification-message-text-color': '#fafbfc',
    '--theme-legacy-badge-background-color': '#ab1700',
    '--theme-legacy-notification-background-color': '#ab2712',
    '--theme-legacy-notification-learn-more-button-text-color': '#fafbfc',
    '--theme-legacy-notification-learn-more-button-background-color':
      'rgba(250, 251, 252, 0.2)',
    '--theme-legacy-notification-learn-more-button-background-color-hover':
      'rgba(250, 251, 252, 0.3)',
    '--theme-legacy-notification-learn-more-button-background-color-active':
      'rgba(250, 251, 252, 0.4)',
    '--theme-legacy-notification-move-button-text-color': '#ab2712',
    '--theme-legacy-notification-move-button-background-color': '#fafbfc',
    '--theme-legacy-notification-move-button-background-color-hover':
      'rgba(250, 251, 252, 0.9)',
    '--theme-legacy-notification-move-button-background-color-active':
      'rgba(250, 251, 252, 0.8)',
    '--theme-legacy-notification-title-color': '#fafbfc',
    '--theme-legacy-notification-description-color': '#fafbfc',
  },
  paperWallet: {
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-color':
      '#5e6066',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(68, 91, 124, 0.05)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': 'rgba(255, 255, 255, 0.3)',
    '--theme-progress-bar-foreground-color': 'rgba(255, 255, 255, 0.7)',
  },
  reactPolymorph: {
    ...rpCardanoTheme,
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': 'transparent',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#2cbb69',
    '--theme-report-issue-button-background-color-hover': '#56c887',
    '--theme-report-issue-button-background-color-active': '#239554',
    '--theme-report-issue-connecting-background-color':
      'rgba(250, 251, 252, 0.05)',
    '--theme-report-issue-connecting-text-color': '#fafbfc',
    '--theme-report-issue-syncing-background-color': 'rgba(94, 96, 102, 0.05)',
    '--theme-report-issue-syncing-text-color': '#5e6066',
    '--theme-report-issue-syncing-download-logs-text-color': '#fafbfc',
    '--theme-report-issue-syncing-download-logs-text-color-connecting':
      '#fafbfc',
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': '#c8ccce',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  settings: {
    '--theme-settings-body-background-color': '#efefef',
    '--theme-settings-pane-background-color': '#fafbfc',
    '--theme-settings-pane-border': '1px solid #c6cdd6',
    '--theme-settings-menu-box-background-color': '#fafbfc',
    '--theme-settings-menu-box-border': '1px solid #c6cdd6',
    '--theme-settings-menu-item-text-color': '#5e6066',
    '--theme-settings-menu-item-text-color-active': '#5e6066',
    '--theme-settings-menu-item-text-color-disabled': '#b3b3b3',
    '--theme-settings-menu-item-background-color-active': '#edf0f3',
    '--theme-settings-menu-item-left-border-color-active': '#2cbb69',
    '--theme-settings-theme-select-title-color': '#5e6066',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#4a5058',
    '--theme-sidebar-category-background-color-hover': 'rgba(52, 56, 61, 0.5)',
    '--theme-sidebar-category-background-color-active': '#34383d',
    '--theme-sidebar-category-text-color': '#fafbfc',
    '--theme-sidebar-menu-background-color': '#34383d',
    '--theme-sidebar-menu-item-background-color-hover': 'rgba(32, 34, 37, 0.5)',
    '--theme-sidebar-menu-item-background-color-active': '#202225',
    '--theme-sidebar-menu-item-wallet-name-color': '#fafbfc',
    '--theme-sidebar-menu-item-wallet-info-color': '#bdc0c1',
    '--theme-sidebar-menu-add-button-background-color': '#202225',
    '--theme-sidebar-menu-add-button-background-color-active': '#272a2e',
    '--theme-sidebar-menu-add-button-background-color-hover': '#272a2e',
    '--theme-sidebar-menu-add-button-text-color': '#fafbfc',
  },
  stakePools: {
    '--theme-staking-stake-pools-title-color': '#5e6066',
    '--theme-staking-stake-pools-search-button-color': '#5e6066',
    '--theme-staking-stake-pool-background-color': '#fafbfc',
    '--theme-staking-stake-pool-border-color': '#c6cdd6',
    '--theme-staking-stake-pool-glow-color': '#7cfeb54c',
    '--theme-staking-stake-pool-id-color': '#5e6066',
    '--theme-staking-stake-pool-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-background-color': `${chroma(
      '#FFFFFF'
    ).alpha(0.97)}`,
    '--theme-staking-stake-pool-tooltip-border-color': '#c6cdd6',
    '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-staking-stake-pool-tooltip-text-color': '#5e6066',
    '--theme-staking-stake-pool-tooltip-link-color': '#26ab5f',
    '--theme-staking-stake-pool-tooltip-table-title-color': '#5e6066',
    '--theme-staking-stake-pool-tooltip-table-param-color': '#5e6066',
    '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-delegate-button-background-color':
      '#2cbb69',
    '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color':
      '#56c887',
    '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color':
      '#239554',
    '--theme-staking-stake-pool-tooltip-delegate-button-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color':
      '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
      'transparent',
  },
  staking: {
    '--theme-staking-content-background-color': '#fafbfc',
    '--theme-staking-content-border-color': '#c6cdd6',
    '--theme-staking-font-color-accent': '#5e6066',
    '--theme-staking-background-color': '#efefef',
    '--theme-staking-font-color-regular': '#5e6066',
    '--theme-staking-font-color-light': 'rgba(94, 96, 102, 0.7)',
    '--theme-staking-font-color-lighter': 'rgba(94, 96, 102, 0.5)',
    '--theme-staking-table-head-background-color': '#f1f3f5',
    '--theme-staking-table-border-color': '#c6cdd6',
    '--theme-staking-link-color': 'rgba(28, 172, 99, 1)',
    '--theme-staking-link-color-light': 'rgba(28, 172, 99, 0.5)',
    '--theme-staking-decentralization-progress-stripe-dark-1-background-color':
      '#259c59',
    '--theme-staking-decentralization-progress-stripe-dark-2-background-color':
      '#2cbb69',
  },
  support: {
    '--theme-support-settings-text-color': '#5e6066',
    '--theme-support-settings-link-color': '#1cac63',
    '--theme-support-settings-item-color': 'rgba(94, 96, 102, 0.5)',
  },
  syncing: {
    '--theme-syncing-background-color': '#fafbfc',
    '--theme-syncing-text-color': '#5e6066',
  },
  systemError: {
    '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
    '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-system-error-overlay-text-color': '#fafbfc',
  },
  tabs: {
    '--theme-choice-tabs-text-color': '#5e6066',
    '--theme-choice-tabs-text-color-active': '#5e6066',
    '--theme-choice-tabs-bottom-border-color-active': '#5e6066',
  },
  testEnvironment: {
    '--theme-test-environment-label-background-color': '#ab1700',
    '--theme-test-environment-label-text-color': '#fafbfc',
  },
  topBar: {
    '--theme-topbar-background-color': '#202225',
    '--theme-topbar-wallet-name-color': '#fafbfc',
    '--theme-topbar-wallet-info-color': '#fafbfc',
    '--theme-topbar-layout-body-background-color': '#ebeff2',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#fafbfc',
    '--theme-transactions-list-border-color': '#c6cdd6',
    '--theme-transactions-list-group-date-color': '#5e6066',
    '--theme-transactions-list-item-details-color': '#5e6066',
    '--theme-transactions-state-failed-background-color': '#bdc5ce',
    '--theme-transactions-state-failed-text-color': '#bdc5ce',
    '--theme-transactions-state-pending-background-color': '#bdc5ce',
    '--theme-transactions-state-pending-stripes-color': '#b2bac2',
    '--theme-transactions-priority-color': '#fafbfc',
    '--theme-transactions-priority-low-background-color': '#ab1700',
    '--theme-transactions-priority-medium-background-color': '#e6aa00',
    '--theme-transactions-priority-high-background-color': '#007600',
    '--theme-transactions-search-background-color': '#fafbfc',
    '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
    '--theme-transactions-icon-type-income-background-color': '#2dc06c',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-icon-type-failed-background-color': '#ea4c5b',
    '--theme-transactions-arrow-stroke-color': '#5e6066',
  },
  uploader: {
    '--theme-uploader-text-color': '#5e6066',
    '--theme-uploader-border-color': '#c6cdd6',
  },
  utxo: {
    '--theme-utxo-background-color': 'rgba(32, 34, 37, 0.05)',
    '--theme-utxo-title-text-color': '#5e6066',
    '--theme-utxo-title-description-color': 'rgba(94, 96, 102, 0.7)',
    '--theme-utxo-bar-color': 'rgba(32, 34, 37, 0.5)',
    '--theme-utxo-label-text-color': 'rgba(32, 34, 37, 0.2)',
    '--theme-utxo-tick-text-color': 'rgba(32, 34, 37, 0.3)',
    '--theme-utxo-cursor-background-color': 'rgba(32, 34, 37, 0.1)',
    '--theme-utxo-tooltip-background-color': 'rgba(94, 96, 102, 0.9)',
    '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-utxo-tooltip-text-color': '#fafbfc',
  },
};

const CARDANO_THEME_PARAMS: CreateThemeParams = {
  config: CARDANO_THEME_CONFIG,
};

export default createTheme(CARDANO_THEME_PARAMS);
