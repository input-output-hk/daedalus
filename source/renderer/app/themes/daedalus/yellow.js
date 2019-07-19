// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

//  ==== yellow theme config for Daedalus and react-polymorph components === //
export const YELLOW_THEME_CONFIG = {
  rpAutocomplete: {
    '--rp-autocomplete-bg-color': '#fff',
    '--rp-autocomplete-border': '1px solid #e1dac6',
    '--rp-autocomplete-border-color-opened': '#2d2d2d',
    '--rp-autocomplete-input-text-color': '#2d2d2d',
    '--rp-autocomplete-placeholder-color': '#2d2d2d80',
    '--rp-autocomplete-selected-word-box-bg-color': '#2d2d2d',
    '--rp-autocomplete-selected-word-text-color': '#fafbfc',
    '--rp-autocomplete-selected-words-font-family':
      'NotoSans-Regular, NotoSansCJKjp-Regular',
  },
  rpBubble: {
    '--rp-bubble-bg-color': '#fff',
    '--rp-bubble-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-bubble-border-radius': '2px',
    '--rp-bubble-arrow-bg-color': 'rgba(45, 45, 45, 0.07)',
  },
  rpButton: {
    '--rp-button-bg-color': '#2d2d2d',
    '--rp-button-bg-color-active': '#222222',
    '--rp-button-bg-color-disabled': 'rgba(45, 45, 45, 0.3)',
    '--rp-button-bg-color-hover': '#424242',
    '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--rp-button-font-size': '14px',
    '--rp-button-height': '50px',
    '--rp-button-line-height': '20px',
    '--rp-button-padding': '0',
    '--rp-button-text-color': '#fafbfc',
    '--rp-button-text-color-disabled': '#fafbfc',
    '--rp-button-text-transform': 'none',
    '--rp-button-width': '360px',
  },
  rpCheckbox: {
    '--rp-checkbox-border': '1px solid #2d2d2d',
    '--rp-checkbox-border-color-disabled': '#2d2d2d66',
    '--rp-checkbox-check-bg-color': '#2d2d2d',
    '--rp-checkbox-label-text-color': '#2d2d2d',
    '--rp-checkbox-label-text-color-disabled': '#2d2d2d4d',
  },
  rpColors: {
    '--rp-theme-color-error': '#ea4c5b',
  },
  rpFonts: {
    '--rp-theme-font-thin': 'NotoSans-Thin, NotoSansCJKjp-Thin',
    '--rp-theme-font-light': 'NotoSans-Light, NotoSansCJKjp-Light',
    '--rp-theme-font-medium': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--rp-theme-font-regular': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--rp-theme-font-bold': 'NotoSans-Bold, NotoSansCJKjp-Bold',
  },
  rpFormfield: {
    '--rp-formfield-bg-color-disabled': 'none',
    '--rp-formfield-label-text-color': '#2d2d2d',
    '--rp-formfield-label-text-color-disabled': '#2d2d2d',
    '--rp-formfield-error-text-color': '#ea4c5b',
    '--rp-formfield-error-text-opacity': '0.75',
  },
  rpInput: {
    '--rp-input-bg-color': '#ffffff',
    '--rp-input-bg-color-disabled': 'rgba(45, 45, 45, 0.07)',
    '--rp-input-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-input-border-color-disabled': '#e1dac680',
    '--rp-input-border-color-errored': '#ea4c5b',
    '--rp-input-border-color-focus': '#2d2d2d',
    '--rp-input-line-height': '22px',
    '--rp-input-padding': '12px 20px',
    '--rp-input-placeholder-color': '#2d2d2d80',
    '--rp-input-placeholder-color-disabled': '#2d2d2d80',
    '--rp-input-text-color': '#2d2d2d',
    '--rp-input-text-color-disabled': '#2d2d2d80',
  },
  rpModal: {
    '--rp-modal-bg-color': '#ffffff',
    '--rp-modal-max-height': '90%',
    '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
  },
  rpOptions: {
    '--rp-option-bg-color': '#fff',
    '--rp-option-bg-color-highlighted': '#edeeef',
    '--rp-option-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-option-checkmark-color': '#2d2d2d',
    '--rp-option-line-height': '22px',
    '--rp-option-text-color': '#2d2d2d',
    '--rp-options-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-options-shadow': 'none',
  },
  rpSelect: {
    '--rp-select-arrow-bg-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-select-arrow-bg-color-open': '#2d2d2d',
    '--rp-select-input-bg-color': '#fff',
    '--rp-select-input-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-select-input-border-color-focus': '#2d2d2d',
    '--rp-select-input-text-color': '#2d2d2d',
    '--rp-select-input-placeholder-color': '#2d2d2d80',
  },
  rpStepper: {
    '--rp-stepper-bullet-background-color-disabled': '#ffffff',
    '--rp-stepper-bullet-border-color': '#2d2d2d1a',
    '--rpstepper-bullet-height': '12px',
    '--rpstepper-bullet-width': '12px',
    '--rp-stepper-label-color': '#2d2d2d',
    '--rp-stepper-label-color-light': '#2d2d2d4d',
    '--rp-stepper-main-color': '#2d2d2d',
    '--rp-stepper-main-color-light': '#2d2d2d1a',
    '--rpstepper-stepper-step-label-bottom-margin': '6px',
    '--rpstepper-steps-bar-color-disabled': '#2d2d2d1a',
    '--rpstepper-steps-bar-top-position': '6px',
  },
  rpSwitch: {
    '--rp-switch-bg-color-off': '#2d2d2d',
    '--rp-switch-bg-color-on': '#2d2d2d',
    '--rp-switch-label-margin': '0 30px 0 0',
    '--rp-switch-label-opacity': '0.5',
    '--rp-switch-label-text-color': '#2d2d2d',
    '--rp-switch-label-width': '100%',
    '--rp-switch-opacity-off': '0.3',
    '--rp-switch-root-margin': '0 0 30px 0',
    '--rp-switch-thumb-bg-color': '#fff',
  },
  rpTextArea: {
    '--rp-textarea-bg-color': '#f8f3ed',
    '--rp-textarea-bg-color-disabled': '#f8f3ed80',
    '--rp-textarea-border': '1px solid #e1dac6',
    '--rp-textarea-border-color-disabled': '#e1dac680',
    '--rp-textarea-border-color-errored': '#ea4c5b',
    '--rp-textarea-border-color-focus': '#2d2d2d',
    '--rp-textarea-border-radius': '2px',
    '--rp-textarea-line-height': '20px',
    '--rp-textarea-placeholder-color': '#2d2d2d80',
    '--rp-textarea-resize': 'none',
    '--rp-textarea-text-color': '#2d2d2d',
  },
  rpTooltip: {
    '--rp-tooltip-bg-color': '#2d2d2d',
    '--rp-tooltip-text-color': '#ffffff',
  },
  aboutWindow: {
    '--theme-about-window-background-color': 'rgba(255, 185, 35, 0.96)',
    '--theme-about-window-header-bottom-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-about-window-daedalus-icon-color': '#2d2d2d',
    '--theme-about-window-cardano-icon-color': '#2d2d2d',
    '--theme-about-window-title-varsion-color': '#2d2d2d',
    '--theme-about-window-title-stroke-color': '#2d2d2d',
    '--theme-about-window-content-color': '#2d2d2d',
    '--theme-about-window-content-text-color': '#2d2d2d',
    '--theme-about-window-content-bottom-border-color': '#2d2d2d4d',
    '--theme-about-window-icon-close-button-color': '#2d2d2d',
    '--theme-about-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.1)',
  },
  adaRedemption: {
    '--theme-ada-redemption-headline-color': '#2d2d2d',
    '--theme-ada-redemption-instructions-color': '#2d2d2d',
    '--theme-ada-redemption-success-overlay-background-color': '#fdcd68',
    '--theme-ada-redemption-success-overlay-border-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-message-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-button-text-color': '#fafbfc',
    '--theme-ada-redemption-success-overlay-button-text-color-hover': '#fdcd68',
    '--theme-ada-redemption-success-overlay-button-background-color-hover':
      '#fdd786',
    '--theme-ada-redemption-disclaimer-background-color':
      'rgba(171, 23, 0, 0.94)',
    '--theme-ada-redemption-disclaimer-text-color': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-check': '#f8f7f3',
    '--theme-ada-redemption-disclaimer-checkbox-color-checked': '#f8f7f3',
    '--theme-ada-redemption-disclaimer-checkbox-color-after': '#ea4c5b',
    '--theme-ada-redemption-disclaimer-checkbox-label-color': '#fafbfc',
    '--theme-ada-redemption-no-wallets-instructions-color': '#2d2d2d',
    '--theme-ada-redemption-disclaimer-button-border-color': '#f8f7f3',
  },
  blockConsolidation: {
    '--theme-block-consolidation-background-color': 'rgba(255, 185, 35, 0.96)',
    '--theme-block-consolidation-title-text-color': 'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-text-color': 'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-text-highlight-color': 'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-epochs-text-color': '#ffffff',
    '--theme-block-consolidation-indicator-text-color': 'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-indicator-container-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-1':
      '#2d2d2d',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-2':
      'rgba(45, 45, 45, 0)',
    '--theme-block-consolidation-stripe-dark-1-background-color':
      'rgba(63, 62, 62, 0.3)',
    '--theme-block-consolidation-stripe-dark-2-background-color':
      'rgba(45, 45, 45, 0.3)',
    '--theme-block-consolidation-stripe-light-1-background-color':
      'rgba(63, 62, 62, 1)',
    '--theme-block-consolidation-stripe-light-2-background-color':
      'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-button-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-block-consolidation-button-background-color-hover': '#2d2d2d',
    '--theme-block-consolidation-button-icon-color': '#2d2d2d',
    '--theme-block-consolidation-button-icon-color-hover': '#ffffff',
    '--theme-block-consolidation-button-text-color': 'rgba(45, 45, 45, 0.7)',
    '--theme-block-consolidation-button-text-color-hover': '#ffffff',
    '--theme-block-consolidation-button-border-color': 'rgba(45, 45, 45, 1)',
    '--theme-block-consolidation-button-border-color-hover': 'transparent',
    '--theme-block-consolidation-epochs-image-color': '#2d2d2d',
  },
  body: {
    '--theme-main-body-background-color': '#f8f3ed',
    '--theme-main-body-messages-color': '#2d2d2d',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#ffffff',
    '--theme-bordered-box-border': '1px solid #e1dac6',
    '--theme-bordered-box-text-color': '#2d2d2d',
  },
  button: {
    '--theme-button-spinner-color': '#f8f7f3',
    '--theme-label-button-color': '#2d2d2d',
  },
  buttonAttention: {
    '--theme-button-attention-background-color': '#ea4c5b',
    '--theme-button-attention-background-color-hover': '#ee707c',
    '--theme-button-attention-background-color-active': '#a43540',
    '--theme-button-attention-background-color-disabled': '#fbdbde',
    '--theme-button-attention-text-color': '#fafbfc',
    '--theme-button-attention-text-color-disabled': '#fafbfc',
    '--theme-button-attention-outline-color': '#f2949d',
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
    '--theme-button-flat-background-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-button-flat-background-color-hover': 'rgba(45, 45, 45, 0.05)',
    '--theme-button-flat-background-color-active': 'rgba(45, 45, 45, 0.12)',
    '--theme-button-flat-background-color-disabled': 'rgba(45, 45, 45, 0.02)',
    '--theme-button-flat-text-color-disabled': '#2d2d2d',
    '--theme-button-flat-text-color': '#2d2d2d',
    '--theme-button-flat-outline-color': '#d6d3ca',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#2d2d2d',
    '--theme-button-primary-background-color-hover': '#424242',
    '--theme-button-primary-background-color-active': '#222222',
    '--theme-button-primary-background-color-disabled': 'rgba(45, 45, 45, 0.3)',
    '--theme-button-primary-text-color-disabled': '#fafbfc',
    '--theme-button-primary-text-color': '#fafbfc',
    '--theme-button-primary-outline-color': '#4C4C4C',
  },
  connecting: {
    '--theme-connecting-background-color': 'rgba(255, 185, 35, 0.96)',
    '--theme-connecting-text-color': '#2d2d2d',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': '#fdcd68',
    '--theme-data-migration-layer-box-shadow-color': '#fdcd68',
    '--theme-data-migration-layer-button-background-color-hover': '#f8f3ed',
    '--theme-data-migration-layer-text-color': '#fafbfc',
    '--theme-data-migration-layer-text-opacity-color': '#fafbfc',
  },
  delegationSetupWizard: {
    '--theme-delegation-steps-activation-steps-indicator-color': '#2d2d2d',
    '--theme-delegation-steps-activation-description-color': '#2d2d2dcc',
    '--theme-delegation-steps-activation-fees-label-color': '#2d2d2d',
    '--theme-delegation-steps-activation-fees-amount-color': '#ea4c5b',
    '--theme-delegation-steps-activation-address-value-color': '#2d2d2d',
    '--theme-delegation-steps-choose-stake-pool-checkmark-icon-color':
      '#e1dac6',
    '--theme-delegation-steps-choose-stake-pool-delegated-pools-label-color':
      '#2d2d2d',
    '--theme-delegation-steps-choose-stake-pool-slug-color': '#e1dac6',
    '--theme-delegation-steps-choose-stake-pool-select-box-placeholder-color':
      '#e1dac6',
    '--theme-delegation-steps-choose-stake-pool-selected-checkmark-icon-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-selected-slug-color': '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-title-color': '#2d2d2dcc',
    '--theme-delegation-steps-choose-stake-pool-tooltip-arrow-color':
      '#2d2d2de6',
    '--theme-delegation-steps-choose-stake-pool-tooltip-background-color':
      '#2d2d2de6',
    '--theme-delegation-steps-choose-stake-pool-tooltip-box-shadow':
      '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-description-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-tooltip-slug-color':
      '#2d2d2d99',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-label-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-value-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-tooltip-title-color': '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-tooltip-url-color': '#d6902f',
    '--theme-delegation-steps-choose-wallet-custom-value-color': '#2d2d2d',
    '--theme-delegation-steps-choose-wallet-description-color': '#2d2d2dcc',
    '--theme-delegation-steps-choose-wallet-description-highlighted-color':
      '#2d2d2d',
    '--theme-delegation-steps-choose-wallet-error-message-color': '#ea4c5b',
    '--theme-delegation-steps-choose-wallet-error-message-light-color':
      '#ea4c5bb3',
    '--theme-delegation-steps-choose-wallet-error-select-options-color':
      '#2d2d2d',
    '--theme-delegation-steps-choose-wallet-steps-indicator-color': '#2d2d2d',
    '--theme-delegation-steps-confirmation-steps-indicator-color': '#2d2d2d',
    '--theme-delegation-steps-confirmation-description-color': '#2d2d2dcc',
    '--theme-delegation-steps-confirmation-fees-label-color': '#2d2d2d',
    '--theme-delegation-steps-confirmation-fees-amount-color': '#ea4c5b',
    '--theme-delegation-steps-intro-content-text-color': '#2d2d2dcc',
    '--theme-delegation-steps-intro-divider-border-color': '#e1dac6',
    '--theme-delegation-steps-intro-link-color': '#d6902f',
    '--theme-delegation-steps-intro-list-label-color': '#2d2d2d',
    '--theme-delegation-steps-intro-list-numbers-color': '#2d2d2d',
    '--theme-delegation-steps-intro-list-optional-label-color': '#2d2d2d80',
    '--theme-delegation-steps-not-available-description-text-color':
      '#2d2d2dcc',
    '--theme-delegation-steps-not-available-description-highlight-text-color':
      '#2d2d2d',
    '--theme-delegation-steps-not-available-icon-color': '#2d2d2d',
    '--theme-delegation-steps-not-available-subtitle-text-color': '#2d2d2d',
  },
  dialog: {
    '--theme-dialog-choice-tabs-text-color': '#2d2d2d',
    '--theme-dialog-choice-tabs-text-color-active': '#2d2d2d',
    '--theme-dialog-choice-tabs-bottom-border-color-active': '#2d2d2d',
    '--theme-dialog-big-button-background-color': '#ffffff',
    '--theme-dialog-big-button-border-color': '#e1dac6',
    '--theme-dialog-big-button-label-color': '#2d2d2d',
    '--theme-dialog-big-button-description-color': '#2d2d2d',
    '--theme-dialog-title-color': '#2d2d2d',
    '--theme-dialog-text-color': '#2d2d2d',
    '--theme-dialog-border-color': 'rgba(45, 45, 45, 0.2)',
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
    '--theme-icon-nav-color': 'rgba(45, 45, 45, 0.6)',
    '--theme-icon-nav-color-active': '#ffffff',
    '--theme-icon-sidebar-color': '#2d2d2d',
    '--theme-icon-toggle-menu-color': '#2d2d2d',
    '--theme-icon-node-update-notification-arrow-color': '#2d2d2d',
    '--theme-icon-add-wallet-from-sidebar-color': '#2d2d2d',
    '--theme-icon-ada-redemption-attention-color': '#fafbfc',
    '--theme-icon-ada-redemption-success-color': '#2d2d2d',
    '--theme-icon-ada-redemption-certificate-color': '#2d2d2d',
    '--theme-icon-ada-redemption-no-wallets': '#2d2d2d',
    '--theme-icon-ada-summary-wallet-amount-symbol-color': '#2d2d2d',
    '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color':
      '#2d2d2d',
    '--theme-icon-add-wallet-dialog-big-button-color': '#2d2d2d',
    '--theme-icon-back-button-color': '#2d2d2d',
    '--theme-icon-close-button-color': '#2d2d2d',
    '--theme-icon-connecting-logo-color': '#2d2d2d',
    '--theme-icon-copy-address-color': '#2d2d2d',
    '--theme-icon-delegation-center-no-wallets': '#2d2d2d',
    '--theme-icon-file-upload-color': '#2d2d2d',
    '--theme-icon-syncing-logo-color': '#2d2d2d',
    '--theme-icon-transactions-ada-symbol-color': '#2d2d2d',
    '--theme-icon-transaction-type-color': '#fafbfc',
  },
  input: {
    '--theme-input-background-color': '#fff',
    '--theme-input-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-input-focus-border-color': '#2d2d2d',
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-label-color': '#2d2d2d',
    '--theme-input-placeholder-color': '#2d2d2d80',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-right-floating-text-color': '#2d2d2d80',
    '--theme-input-text-color': '#2d2d2d',
  },
  link: {
    '--theme-link-main-color': '#d6902f',
  },
  loading: {
    '--theme-loading-background-color': '#ffffff',
    '--theme-loading-no-disk-space-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-loading-no-disk-space-text-color': '#ffffff',
    '--theme-loading-no-disk-space-attention-icon-color': '#ffffff',
    '--theme-loading-status-icons-on-color': '#2dc06c',
    '--theme-loading-status-icons-off-color': '#ea4c5b',
    '--theme-loading-status-icons-unloaded-loading-color': '#2d2d2d',
    '--theme-loading-status-icons-unloaded-syncing-color': '#2d2d2d',
    '--theme-loading-status-icons-tooltip-color': '#2d2d2d',
    '--theme-loading-spinner-color': '#2d2d2d',
  },
  manualUpdate: {
    '--theme-manual-update-overlay-background-color':
      'rgba(255, 185, 35, 0.96)',
    '--theme-manual-update-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-manual-update-overlay-button-background-color-hover': '#2d2d2d',
    '--theme-manual-update-overlay-button-icon-color': '#2d2d2d',
    '--theme-manual-update-overlay-button-icon-color-hover': '#ffffff',
    '--theme-manual-update-overlay-button-text-color-hover': '#ffffff',
    '--theme-manual-update-overlay-button-border-color': '#2d2d2d',
    '--theme-manual-update-overlay-text-color': 'rgba(45, 45, 45, 0.7)',
    '--theme-manual-update-overlay-text-highlight-color':
      '#rgba(45, 45, 45, 1)',
    '--theme-manual-update-overlay-title-text-color': 'rgba(45, 45, 45, 1)',
    '--theme-manual-update-overlay-button-label-color': 'rgba(45, 45, 45, 1)',
    '--theme-manual-update-overlay-button-label-color-hover': '#ffffff',
  },
  mnemonic: {
    '--theme-mnemonic-background-color': 'rgba(45, 45, 45, 0.07)',
    '--theme-mnemonic-background-color-hover': '#d6d3ca',
    '--theme-backup-mnemonic-background-color': '#f1eee6',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navDropdown: {
    '--theme-nav-dropdown-item-text-color': '#2d2d2d',
    '--theme-nav-dropdown-item-background-color': '#fff',
    '--theme-nav-dropdown-item-background-color-hover':
      'rgba(45, 45, 45, 0.07)',
    '--theme-nav-dropdown-item-color-hover': '#2d2d2d',
  },
  navItem: {
    '--theme-nav-item-background-color': '#eea70e',
    '--theme-nav-item-background-color-active': '#2d2d2d',
    '--theme-nav-item-background-color-hover': '#ffb923',
    '--theme-nav-item-text-color': 'rgba(45, 45, 45, 0.6)',
    '--theme-nav-item-text-color-active': '#fafbfc',
  },
  network: {
    '--theme-network-window-background-color': 'rgba(255, 185, 35, 0.96)',
    '--theme-network-window-text-color': '#2d2d2d',
    '--theme-network-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.1)',
    '--theme-network-window-red-color': '#be0b0b',
    '--theme-network-window-green-color': '#008114',
    '--theme-network-window-white-color': '#2d2d2d',
    '--theme-network-window-transparent-color': 'transparent',
    '--theme-network-window-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-network-window-button-text-color': '#ffffff',
    '--theme-network-window-button-background-color': '#2d2d2d',
    '--theme-network-window-button-background-color-hover': '#424242',
    '--theme-network-window-button-background-color-active': '#222222',
  },
  nodeUpdate: {
    '--theme-node-update-background-color': '#f8f3ed',
    '--theme-node-update-title-color': '#2d2d2d',
    '--theme-node-update-message-color': '#2d2d2d',
    '--theme-node-sync-info-message-background-color': '#f8f3ed',
    '--theme-node-sync-info-message-text-color': '#2d2d2d',
    '--theme-node-sync-icon-color': '#2d2d2d',
    '--theme-node-update-accept-button-background-color': '#fdcd68',
    '--theme-node-update-accept-button-background-color-hover': '#fdd786',
    '--theme-node-update-accept-button-background-color-active': '#eea70e',
    '--theme-node-update-deny-button-background-color': '#fdcd68',
    '--theme-node-update-deny-button-background-color-hover': '#fdd786',
    '--theme-node-update-deny-button-background-color-active': '#eea70e',
    '--theme-node-update-button-text-color': '#fafbfc',
  },
  notification: {
    '--theme-notification-message-background-color': '#fdcd68f2',
    '--theme-notification-message-text-color': '#2d2d2d',
    '--theme-notification-message-checkmark-icon-color': '#2d2d2d',
    '--theme-notification-message-close-icon-color': '#2d2d2d',
    '--theme-legacy-badge-background-color': '#d34452',
    '--theme-legacy-notification-background-color': '#ab2712',
    '--theme-legacy-notification-learn-more-button-text-color': '#fafbfc',
    '--theme-legacy-notification-learn-more-button-background-color':
      '#f8f3ed33',
    '--theme-legacy-notification-learn-more-button-background-color-hover':
      '#f8f3ed4d',
    '--theme-legacy-notification-learn-more-button-background-color-active':
      '#f8f3ed66',
    '--theme-legacy-notification-move-button-text-color': '#ab2712',
    '--theme-legacy-notification-move-button-background-color': '#fafbfc',
    '--theme-legacy-notification-move-button-background-color-hover':
      '#f8f3ede6',
    '--theme-legacy-notification-move-button-background-color-active':
      '#f8f3edcc',
    '--theme-legacy-notification-title-color': '#fafbfc',
    '--theme-legacy-notification-description-color': '#fafbfc',
  },
  paperWallet: {
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-color':
      '#d6902f',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(45, 45, 45, 0.07)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': '#f1eee64d',
    '--theme-progress-bar-foreground-color': '#f1eee6b3',
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': 'transparent',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#2d2d2d',
    '--theme-report-issue-button-background-color-hover': '#424242',
    '--theme-report-issue-button-background-color-active': '#222222',
    '--theme-report-issue-connecting-background-color': '#fdcd68',
    '--theme-report-issue-icon-color': '#f8f3ed',
    '--theme-report-issue-connecting-text-color': '#2d2d2d',
    '--theme-report-issue-syncing-background-color': 'rgba(94, 96, 102, 0.05)',
    '--theme-report-issue-syncing-text-color': '#2d2d2d',
    '--theme-report-issue-syncing-download-logs-text-color': '#d6902f',
    '--theme-report-issue-syncing-download-logs-text-color-connecting':
      '#2d2d2d',
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': '#bebbb3',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  settings: {
    '--theme-settings-body-background-color': '#f8f3ed',
    '--theme-settings-pane-background-color': '#fff',
    '--theme-settings-pane-border': '1px solid #e1dac6',
    '--theme-settings-menu-box-background-color': '#fff',
    '--theme-settings-menu-box-border': '1px solid #e1dac6',
    '--theme-settings-menu-item-text-color': '#2d2d2d',
    '--theme-settings-menu-item-text-color-active': '#2d2d2d',
    '--theme-settings-menu-item-text-color-disabled': '#2d2d2d80',
    '--theme-settings-menu-item-background-color-active':
      'rgba(45, 45, 45, 0.07)',
    '--theme-settings-menu-item-left-border-color-active':
      'rgba(45, 45, 45, 1)',
    '--theme-settings-theme-select-title-color': '#2d2d2d',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#fdcd68',
    '--theme-sidebar-category-background-color-hover':
      'rgba(255, 185, 35, 0.5)',
    '--theme-sidebar-category-background-color-active': '#ffb923',
    '--theme-sidebar-category-text-color': '#2d2d2d',
    '--theme-sidebar-menu-background-color': '#ffb923',
    '--theme-sidebar-menu-item-background-color-hover':
      'rgba(237, 167, 14, 0.5)',
    '--theme-sidebar-menu-item-background-color-active': '#eea70e',
    '--theme-sidebar-menu-item-wallet-name-color': '#2d2d2d',
    '--theme-sidebar-menu-item-wallet-info-color': '#2d2d2d',
    '--theme-sidebar-menu-add-button-background-color': '#eea70e',
    '--theme-sidebar-menu-add-button-background-color-active': '#eea70ea8',
    '--theme-sidebar-menu-add-button-background-color-hover': '#eea70ea8',
    '--theme-sidebar-menu-add-button-text-color': '#2d2d2d',
  },
  stakePools: {
    '--theme-staking-stake-pools-title-color': '#2d2d2d',
    '--theme-staking-stake-pools-search-button-color': '#2d2d2d',
    '--theme-staking-stake-pool-background-color': '#ffffff',
    '--theme-staking-stake-pool-border-color': '#e1dac6',
    '--theme-staking-stake-pool-glow-color': 'rgba(45, 45, 45, 0.14)',
    '--theme-staking-stake-pools-search-icon-color': '#2d2d2d',
    '--theme-staking-stake-pool-selected-background-color': '#5da377',
    '--theme-staking-stake-pool-selected-checkmark-icon-color': '#fafbfc',
    '--theme-staking-stake-pool-selected-slug-color': '#fafbfc',
    '--theme-staking-stake-pool-slug-color': '#2d2d2d',
    '--theme-staking-stake-pool-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-background-color': '#ffffff',
    '--theme-staking-stake-pool-tooltip-border-color': '#e1dac6',
    '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-staking-stake-pool-tooltip-text-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-link-color': '#d6902f',
    '--theme-staking-stake-pool-tooltip-table-title-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-table-param-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-retirement-background-color':
      '#ea4c5b4d',
    '--theme-staking-stake-pool-tooltip-delegate-button-background-color':
      '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color':
      '#fdd786',
    '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color':
      '#eea70e',
    '--theme-staking-stake-pool-tooltip-delegate-button-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color':
      '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
      'transparent',
    '--theme-staking-progress-label-light': '#2d2d2d',
  },
  staking: {
    '--theme-staking-content-background-color': '#ffffff',
    '--theme-staking-content-border-color': '#e1dac6',
    '--theme-staking-font-color-accent': '#2d2d2d',
    '--theme-staking-font-color-regular': '#2d2d2d',
    '--theme-staking-font-color-light': '#2d2d2db3',
    '--theme-staking-font-color-lighter': '#2d2d2d80',
    '--theme-staking-table-head-background-color': 'rgba(45, 45, 45, 0.07)',
    '--theme-staking-table-border-color': '#e1dac6',
    '--theme-staking-link-color': '#d6902f',
    '--theme-staking-link-color-light': '#fdcd68',
    '--theme-staking-progress-bar-background-color': 'rgba(45, 45, 45, 0.07)',
    '--theme-staking-progress-stripe-dark-1-background-color': '#e4b95e',
    '--theme-staking-progress-stripe-dark-2-background-color': '#fdcd68',
    '--theme-staking-table-body-highlighted-text-color': '#d6902f',
    '--theme-staking-info-learn-more-button-color': '#f8f7f3',
    '--theme-staking-info-learn-more-icon-color': '#fafbfc',
    '--theme-staking-learn-more-button-color': '#f8f7f3',
    '--theme-staking-learn-more-icon-color': '#fafbfc',
    '--theme-staking-donut-ring-completed-color': '#ea4c5b',
    '--theme-staking-donut-ring-remaining-color': '#fbdbde',
    '--theme-staking-wallet-row-border-color': '#e1dac6',
    '--theme-staking-dropdown-item-text-color-hover': '#2d2d2d',
    '--theme-staking-dropdown-item-background-color': '#ffffff',
    '--theme-staking-dropdown-item-background-color-hover':
      'rgba(45, 45, 45, 0.07)',
    '--theme-staking-delegation-center-gear-icon-fill-color': '#2d2d2d80',
    '--theme-staking-delegation-center-gear-icon-fill-color-active': '#2d2d2d',
    '--theme-staking-delegation-center-no-wallets-instructions-color':
      '#2d2d2d',
  },
  support: {
    '--theme-support-settings-item-color': '#2d2d2d',
    '--theme-support-settings-link-color': '#d6902f',
    '--theme-support-settings-text-color': '#2d2d2d',
  },
  syncing: {
    '--theme-syncing-background-color': '#ffffff',
    '--theme-syncing-text-color': '#2d2d2d',
  },
  systemError: {
    '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
    '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-system-error-overlay-support-link-icon-color': '#fafbfc',
    '--theme-system-error-overlay-text-color': '#fafbfc',
  },
  tabs: {
    '--theme-choice-tabs-text-color': '#2d2d2d',
    '--theme-choice-tabs-text-color-active': '#2d2d2d',
    '--theme-choice-tabs-bottom-border-color-active': '#2d2d2d',
  },
  testEnvironment: {
    '--theme-test-environment-label-background-color': '#ab1700',
    '--theme-test-environment-label-text-color': '#fafbfc',
  },
  topBar: {
    '--theme-topbar-background-color': '#eea70e',
    '--theme-topbar-layout-body-background-color': '#fdcd68',
    '--theme-topbar-wallet-name-color': '#2d2d2d',
    '--theme-topbar-wallet-info-color': '#2d2d2d',
    '--theme-topbar-logo-color': '#2d2d2d',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#fff',
    '--theme-transactions-list-border-color': '#e1dac6',
    '--theme-transactions-list-group-date-color': '#2d2d2d',
    '--theme-transactions-list-item-details-color': '#2d2d2d',
    '--theme-transactions-state-failed-background-color': '#d6d3ca',
    '--theme-transactions-state-failed-text-color': '#2d2d2d',
    '--theme-transactions-state-pending-background-color': '#d6d3ca',
    '--theme-transactions-state-pending-stripes-color': '#bebbb3',
    '--theme-transactions-priority-color': '#f8f3ed',
    '--theme-transactions-priority-low-background-color': '#d34452',
    '--theme-transactions-priority-medium-background-color': '#e6aa00',
    '--theme-transactions-priority-high-background-color': '#007600',
    '--theme-transactions-search-background-color': '#f8f3ed',
    '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
    '--theme-transactions-icon-type-income-background-color': '#2dc06c',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-icon-type-failed-background-color': '#ee707c',
    '--theme-transactions-arrow-stroke-color': '#2d2d2d',
  },
  uploader: {
    '--theme-uploader-text-color': '#2d2d2d',
    '--theme-uploader-border-color': 'rgba(45, 45, 45, 0.2)',
  },
  utxo: {
    '--theme-utxo-background-color': 'rgba(45, 45, 45, 0.05)',
    '--theme-utxo-title-text-color': '#2d2d2d',
    '--theme-utxo-title-description-color': '#2d2d2db3',
    '--theme-utxo-bar-color': '#2d2d2d',
    '--theme-utxo-label-text-color': '#2d2d2d73',
    '--theme-utxo-tick-text-color': '#2d2d2d73',
    '--theme-utxo-cursor-background-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-utxo-tooltip-background-color': '#2d2d2d',
    '--theme-utxo-tooltip-shadow-color': 'rgba(45, 45, 45, 0.18)',
    '--theme-utxo-tooltip-text-color': '#fff',
  },
};

const YELLOW_THEME_PARAMS: CreateThemeParams = {
  config: YELLOW_THEME_CONFIG,
};

export default createTheme(YELLOW_THEME_PARAMS);
