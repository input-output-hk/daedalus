// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

//  ==== white theme config for Daedalus and react-polymorph components === //
export const WHITE_THEME_CONFIG = {
  rpAutocomplete: {
    '--rp-autocomplete-bg-color': '#fdfdfd',
    '--rp-autocomplete-border': '1px solid rgba(45, 45, 45, 0.1)',
    '--rp-autocomplete-border-color-opened': '#2d2d2d',
    '--rp-autocomplete-input-text-color': '#2d2d2d',
    '--rp-autocomplete-placeholder-color': '#2d2d2d80',
    '--rp-autocomplete-selected-word-box-bg-color': '#29b595',
    '--rp-autocomplete-selected-word-text-color': '#ffffff',
    '--rp-autocomplete-selected-words-font-family':
      'NotoSans-Regular, NotoSansCJKjp-Regular',
  },
  rpBubble: {
    '--rp-bubble-bg-color': '#fff',
    '--rp-bubble-border-color': 'rgba(94, 96, 102, 0.2)',
    '--rp-bubble-border-radius': '2px',
    '--rp-bubble-arrow-bg-color': 'rgba(94, 96, 102, 0.07)',
  },
  rpButton: {
    '--rp-button-bg-color': '#29b595',
    '--rp-button-bg-color-active': '#25a386',
    '--rp-button-bg-color-disabled': 'rgba(41, 181, 149, 0.3)',
    '--rp-button-bg-color-hover': '#54c4aa',
    '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--rp-button-font-size': '14px',
    '--rp-button-height': '50px',
    '--rp-button-line-height': '20px',
    '--rp-button-padding': '0',
    '--rp-button-text-color': '#fff',
    '--rp-button-text-color-disabled': '#fff',
    '--rp-button-text-transform': 'none',
    '--rp-button-width': '360px',
  },
  rpCheckbox: {
    '--rp-checkbox-border': '1px solid #29b595',
    '--rp-checkbox-border-color-disabled': 'rgba(41, 181, 149, 0.3)',
    '--rp-checkbox-border-color': '#29b595',
    '--rp-checkbox-check-bg-color': '#29b595',
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
    '--rp-input-bg-color': '#fdfdfd',
    '--rp-input-bg-color-disabled': 'rgba(94, 96, 102, 0.05)',
    '--rp-input-border-color': 'rgba(45, 45, 45, 0.2)',
    '--rp-input-border-color-disabled': 'rgba(94, 96, 102, 0.05)',
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
    '--rp-modal-bg-color': '#fdfdfd',
    '--rp-modal-max-height': '90%',
    '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
  },
  rpOptions: {
    '--rp-option-bg-color': '#fff',
    '--rp-option-bg-color-highlighted': '#edeeef',
    '--rp-option-border-color': 'rgba(94, 96, 102, 0.2)',
    '--rp-option-checkmark-color': '#2d2d2d',
    '--rp-option-line-height': '22px',
    '--rp-option-text-color': '#2d2d2d',
    '--rp-options-border-color': 'rgba(94, 96, 102, 0.2)',
    '--rp-options-shadow': 'none',
  },
  rpSelect: {
    '--rp-select-arrow-bg-color': 'rgba(45, 45, 45, 0.1)',
    '--rp-select-arrow-bg-color-open': '#2d2d2d',
    '--rp-select-input-bg-color': '#fdfdfd',
    '--rp-select-input-border-color': 'rgba(45, 45, 45, 0.1)',
    '--rp-select-input-border-color-focus': '#2d2d2d',
    '--rp-select-input-text-color': '#2d2d2d',
    '--rp-select-input-placeholder-color': '#2d2d2d80',
  },
  rpStepper: {
    '--rp-stepper-bullet-background-color-disabled': '#fff',
    '--rp-stepper-bullet-border-color': '#29b5951a',
    '--rpstepper-bullet-height': '12px',
    '--rpstepper-bullet-width': '12px',
    '--rp-stepper-label-color': '#29b595',
    '--rp-stepper-label-color-light': '#29b5954d',
    '--rp-stepper-main-color': '#29b595',
    '--rp-stepper-main-color-light': '#29b5951a',
    '--rpstepper-stepper-step-label-bottom-margin': '6px',
    '--rpstepper-steps-bar-color-disabled': '#29b5951a',
    '--rpstepper-steps-bar-top-position': '6px',
  },
  rpSwitch: {
    '--rp-switch-bg-color-off': '#29b595',
    '--rp-switch-bg-color-on': '#29b595',
    '--rp-switch-label-margin': '0 30px 0 0',
    '--rp-switch-label-opacity': '0.5',
    '--rp-switch-label-text-color': '#2d2d2d',
    '--rp-switch-label-width': '100%',
    '--rp-switch-opacity-off': '0.3',
    '--rp-switch-root-margin': '0 0 30px 0',
    '--rp-switch-thumb-bg-color': '#fafbfc',
  },
  rpTextArea: {
    '--rp-textarea-bg-color': '#f9f9f9',
    '--rp-textarea-bg-color-disabled': '#f9f9f980',
    '--rp-textarea-border': '1px solid rgba(45, 45, 45, 0.1)',
    '--rp-textarea-border-color-disabled': '#2d2d2d80',
    '--rp-textarea-border-color-errored': '#ea4c5b',
    '--rp-textarea-border-color-focus': '#2d2d2d',
    '--rp-textarea-border-radius': '2px',
    '--rp-textarea-line-height': '20px',
    '--rp-textarea-placeholder-color': '#2d2d2d80',
    '--rp-textarea-resize': 'none',
    '--rp-textarea-text-color': '#2d2d2d',
  },
  aboutWindow: {
    '--theme-about-window-background-color': '#fffffff5',
    '--theme-about-window-header-bottom-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-about-window-daedalus-icon-color': '#29b595',
    '--theme-about-window-cardano-icon-color': '#29b595',
    '--theme-about-window-title-varsion-color': '#2d2d2d',
    '--theme-about-window-title-stroke-color': '#2d2d2d',
    '--theme-about-window-content-color': '#2d2d2d',
    '--theme-about-window-content-text-color': '#2d2d2d',
    '--theme-about-window-content-bottom-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-about-window-icon-close-button-color': '#2d2d2d',
    '--theme-about-window-icon-close-hover-background':
      'rgba(41, 181, 149, 0.1)',
  },
  adaRedemption: {
    '--theme-ada-redemption-headline-color': '#2d2d2d',
    '--theme-ada-redemption-instructions-color': '#2d2d2d',
    '--theme-ada-redemption-success-overlay-background-color': '#ffffff',
    '--theme-ada-redemption-success-overlay-border-color': '#29b595',
    '--theme-ada-redemption-success-overlay-message-color': '#2d2d2d',
    '--theme-ada-redemption-success-overlay-button-text-color': '#29b595',
    '--theme-ada-redemption-success-overlay-button-text-color-hover': '#ffffff',
    '--theme-ada-redemption-success-overlay-button-background-color-hover':
      '#29b595',
    '--theme-ada-redemption-disclaimer-background-color':
      'rgba(171, 23, 0, 0.94)',
    '--theme-ada-redemption-disclaimer-text-color': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-check': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-checked': '#fafbfc',
    '--theme-ada-redemption-disclaimer-checkbox-color-after': '#ab1700',
    '--theme-ada-redemption-disclaimer-checkbox-label-color': '#fafbfc',
    '--theme-ada-redemption-no-wallets-instructions-color': '#2d2d2d',
    '--theme-ada-redemption-disclaimer-button-border-color': '#fdfdfd',
    '--theme-ada-redemption-success-overlay-close-button-color': '#2d2d2d',
    '--theme-ada-redemption-success-overlay-close-button-background-color-hover':
      'rgba(41, 181, 149, 0.07)',
  },
  blockConsolidation: {
    '--theme-block-consolidation-background-color': '#ffffff',
    '--theme-block-consolidation-title-text-color': '#2d2d2d',
    '--theme-block-consolidation-text-color': 'rgba(45, 45, 45, 0.7)',
    '--theme-block-consolidation-text-highlight-color': '#2d2d2d',
    '--theme-block-consolidation-epochs-text-color': '#ffffff',
    '--theme-block-consolidation-indicator-text-color': '#2d2d2d',
    '--theme-block-consolidation-indicator-container-background-color':
      'rgba(105, 203, 180, 0.1)',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-1':
      '#2d2d2d',
    '--theme-block-consolidation-indicator-epochs-behind-background-color-2':
      '#2d2d2d00',
    '--theme-block-consolidation-stripe-dark-1-background-color': '#B4E5DA',
    '--theme-block-consolidation-stripe-dark-2-background-color': '#C1E9E1',
    '--theme-block-consolidation-stripe-light-1-background-color': '#29b595',
    '--theme-block-consolidation-stripe-light-2-background-color': '#53C3AA',
    '--theme-block-consolidation-button-background-color':
      'rgba(41, 181, 149, 0.1)',
    '--theme-block-consolidation-button-background-color-active': '#25a386',
    '--theme-block-consolidation-button-background-color-hover': '#29b595',
    '--theme-block-consolidation-button-icon-color': '#29b595',
    '--theme-block-consolidation-button-icon-color-hover': '#ffffff',
    '--theme-block-consolidation-button-text-color': '#29b595',
    '--theme-block-consolidation-button-text-color-hover': '#ffffff',
    '--theme-block-consolidation-button-border-color': '#29b595',
    '--theme-block-consolidation-button-border-color-hover': 'transparent',
    '--theme-block-consolidation-epochs-image-color': 'rgba(41, 181, 149, 0.9)',
  },
  body: {
    '--theme-main-body-background-color': '#f9f9f9',
    '--theme-main-body-messages-color': '#2d2d2d',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#fdfdfd',
    '--theme-bordered-box-border': '1px solid #fff',
    '--theme-bordered-box-text-color': '#2d2d2d',
  },
  button: {
    '--theme-button-spinner-color': '#fdfdfd',
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
    '--theme-button-flat-background-color': 'rgba(41, 181, 149, 0.1)',
    '--theme-button-flat-background-color-hover': 'rgba(41, 181, 149, 0.05)',
    '--theme-button-flat-background-color-active': 'rgba(41, 181, 149, 0.12)',
    '--theme-button-flat-background-color-disabled': 'rgba(41, 181, 149, 0.02)',
    '--theme-button-flat-text-color-disabled': 'rgba(45, 45, 45, 0.3)',
    '--theme-button-flat-text-color': '#2d2d2d',
    '--theme-button-flat-outline-color': 'rgba(94, 96, 102, 0.2)',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#29b595',
    '--theme-button-primary-background-color-hover': '#54c4aa',
    '--theme-button-primary-background-color-active': '#25a386',
    '--theme-button-primary-background-color-disabled':
      'rgba(41, 181, 149, 0.3)',
    '--theme-button-primary-text-color-disabled': 'rgba(255, 255, 255, 0.7)',
    '--theme-button-primary-text-color': '#fff',
    '--theme-button-primary-outline-color': '#54c4aa',
  },
  connecting: {
    '--theme-connecting-background-color': '#ffffff',
    '--theme-connecting-text-color': '#2d2d2d',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': '#ffffff',
    '--theme-data-migration-layer-box-shadow-color': '#29b595',
    '--theme-data-migration-layer-button-background-color-hover': '#29b595',
    '--theme-data-migration-layer-text-color': '#2d2d2d',
    '--theme-data-migration-layer-text-opacity-color': '#fafbfc',
    '--theme-data-migration-button-border-color': '#29b595',
    '--theme-data-migration-button-label-color': '#29b595',
  },
  delegationSetupWizard: {
    '--theme-delegation-steps-activation-steps-indicator-color': '#2d2d2d',
    '--theme-delegation-steps-activation-description-color': '#2d2d2dcc',
    '--theme-delegation-steps-activation-fees-label-color': '#2d2d2d',
    '--theme-delegation-steps-activation-fees-amount-color': '#ea4c5b',
    '--theme-delegation-steps-activation-address-value-color': '#2d2d2d',
    '--theme-delegation-steps-choose-stake-pool-checkmark-icon-color':
      'rgba(45, 45, 45, 0.1)',
    '--theme-delegation-steps-choose-stake-pool-delegated-pools-label-color':
      '#2d2d2d',
    '--theme-delegation-steps-choose-stake-pool-slug-color':
      'rgba(45, 45, 45, 0.1)',
    '--theme-delegation-steps-choose-stake-pool-select-box-placeholder-color':
      'rgba(45, 45, 45, 0.1)',
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
    '--theme-delegation-steps-choose-stake-pool-tooltip-url-color': '#85b6f9',
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
    '--theme-delegation-steps-intro-divider-border-color':
      'rgba(45, 45, 45, 0.1)',
    '--theme-delegation-steps-intro-link-color': '#ffffff',
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
    '--theme-dialog-big-button-border-color': 'none',
    '--theme-dialog-big-button-label-color': '#2d2d2d',
    '--theme-dialog-big-button-description-color': 'rgba(45, 45, 45, 0.6)',
    '--theme-dialog-title-color': '#2d2d2d',
    '--theme-dialog-text-color': '#2d2d2d',
    '--theme-dialog-border-color': 'rgba(45, 45, 45, 0.1)',
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
    '--theme-icon-nav-color-active': '#29b595',
    '--theme-icon-sidebar-color': '#2d2d2d',
    '--theme-icon-toggle-menu-color': '#2d2d2d',
    '--theme-icon-node-update-notification-arrow-color': '#2d2d2d',
    '--theme-icon-add-wallet-from-sidebar-color': '#2d2d2d',
    '--theme-icon-ada-redemption-attention-color': '#fafbfc',
    '--theme-icon-ada-redemption-success-color': '#2d2d2d',
    '--theme-icon-ada-redemption-certificate-color': 'rgba(45, 45, 45, 0.5)',
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
    '--theme-input-background-color': '#fdfdfd',
    '--theme-input-border-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-input-focus-border-color': '#2d2d2d',
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-label-color': '#2d2d2d',
    '--theme-input-placeholder-color': '#2d2d2d80',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-right-floating-text-color': '#2d2d2d80',
    '--theme-input-text-color': '#2d2d2d',
  },
  link: {
    '--theme-link-main-color': '#29b595',
  },
  loading: {
    '--theme-loading-background-color': '#f9f9f9',
    '--theme-loading-no-disk-space-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-loading-no-disk-space-text-color': '#fafbfc',
    '--theme-loading-no-disk-space-attention-icon-color': '#fafbfc',
    '--theme-loading-status-icons-on-color': '#2dc06c',
    '--theme-loading-status-icons-off-color': '#ea4c5b',
    '--theme-loading-status-icons-unloaded-loading-color': '#2d2d2d',
    '--theme-loading-status-icons-unloaded-syncing-color': '#2d2d2d',
    '--theme-loading-status-icons-tooltip-color': '#2d2d2d',
    '--theme-loading-spinner-color': '#2d2d2d',
  },
  manualUpdate: {
    '--theme-manual-update-overlay-background-color': '#fffffff5',
    '--theme-manual-update-overlay-button-background-color': '#fff',
    '--theme-manual-update-overlay-button-background-color-hover': '#29b595',
    '--theme-manual-update-overlay-button-icon-color': '#29b595',
    '--theme-manual-update-overlay-button-icon-color-hover': '#ffffff',
    '--theme-manual-update-overlay-button-text-color-hover': '#ffffff',
    '--theme-manual-update-overlay-button-border-color': '#29b595',
    '--theme-manual-update-overlay-text-color': '#2d2d2db3',
    '--theme-manual-update-overlay-text-highlight-color': '#2d2d2d',
    '--theme-manual-update-overlay-title-text-color': '#2d2d2d',
    '--theme-manual-update-overlay-button-label-color': '#29b595',
    '--theme-manual-update-overlay-button-label-color-hover': '#fff',
  },
  mnemonic: {
    '--theme-mnemonic-background-color': 'rgba(94, 96, 102, 0.07)',
    '--theme-mnemonic-background-color-hover': '#e0e0e0',
    '--theme-backup-mnemonic-background-color': 'rgba(219, 219, 219, 0.2)',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navDropdown: {
    '--theme-nav-dropdown-item-text-color': '#2d2d2d',
    '--theme-nav-dropdown-item-background-color': '#fdfdfd',
    '--theme-nav-dropdown-item-background-color-hover':
      'rgba(94, 96, 102, 0.07)',
    '--theme-nav-dropdown-item-color-hover': '#2d2d2d',
  },
  navItem: {
    '--theme-nav-item-background-color': '#ffffff',
    '--theme-nav-item-background-color-active': 'transparent',
    '--theme-nav-item-background-color-hover': 'rgba(94, 96, 102, 0.1)',
    '--theme-nav-item-text-color': 'rgba(45, 45, 45, 0.6)',
    '--theme-nav-item-text-color-active': '#29b595',
  },
  network: {
    '--theme-network-window-background-color': '#fffffff5',
    '--theme-network-window-text-color': '#2d2d2d',
    '--theme-network-window-icon-close-hover-background':
      'rgba(41, 181, 149, 0.1)',
    '--theme-network-window-red-color': '#f06e05',
    '--theme-network-window-green-color': 'rgb(0, 129, 20)',
    '--theme-network-window-white-color': '#2d2d2d',
    '--theme-network-window-transparent-color': 'transparent',
    '--theme-network-window-border-color': 'rgba(45, 45, 45, 0.2)',
    '--theme-network-window-button-text-color': '#fafbfc',
    '--theme-network-window-button-background-color': 'rgba(94, 96, 102, 0.4)',
    '--theme-network-window-button-background-color-hover':
      'rgba(94, 96, 102, 0.6)',
    '--theme-network-window-button-background-color-active':
      'rgba(94, 96, 102, 0.8)',
  },
  nodeUpdate: {
    '--theme-node-update-background-color': '#f9f9f9',
    '--theme-node-update-title-color': '#2d2d2d',
    '--theme-node-update-message-color': '#2d2d2d',
    '--theme-node-sync-icon-color': '#2d2d2d',
    '--theme-node-sync-info-message-background-color': '#f9f9f9',
    '--theme-node-sync-info-message-text-color': '#2d2d2d',
    '--theme-node-update-accept-button-background-color': '#29b595',
    '--theme-node-update-accept-button-background-color-hover': '#54c4aa',
    '--theme-node-update-accept-button-background-color-active': '#25a386',
    '--theme-node-update-deny-button-background-color':
      'rgba(41, 181, 149, 0.3)',
    '--theme-node-update-deny-button-background-color-hover':
      'rgba(84, 196, 170, 0.3)',
    '--theme-node-update-deny-button-background-color-active':
      'rgba(37, 163, 134, 0.3)',
    '--theme-node-update-button-text-color': '#fafbfc',
  },
  notification: {
    '--theme-notification-message-background-color': 'rgba(41, 181, 149, 0.9)',
    '--theme-notification-message-text-color': '#fafbfc',
    '--theme-legacy-badge-background-color': '#d34452',
    '--theme-legacy-notification-background-color': '#ab2712',
    '--theme-legacy-notification-learn-more-button-text-color': '#fafbfc',
    '--theme-legacy-notification-learn-more-button-background-color':
      '#f9f9f933',
    '--theme-legacy-notification-learn-more-button-background-color-hover':
      '#f9f9f94d',
    '--theme-legacy-notification-learn-more-button-background-color-active':
      '#f9f9f966',
    '--theme-legacy-notification-move-button-text-color': '#ab2712',
    '--theme-legacy-notification-move-button-background-color': '#fafbfc',
    '--theme-legacy-notification-move-button-background-color-hover':
      '#f9f9f9e6',
    '--theme-legacy-notification-move-button-background-color-active':
      '#f9f9f9cc',
    '--theme-legacy-notification-title-color': '#fafbfc',
    '--theme-legacy-notification-description-color': '#fafbfc',
    '--theme-notification-message-checkmark-icon-color': '#fafbfc',
    '--theme-notification-message-close-icon-color': '#fafbfc',
  },
  paperWallet: {
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-color':
      '#29b595',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(94, 96, 102, 0.07)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': 'rgba(45, 45, 45, 0.3)',
    '--theme-progress-bar-foreground-color': 'rgba(45, 45, 45, 0.7)',
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': 'transparent',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#29b595',
    '--theme-report-issue-button-background-color-hover': '#54c4aa',
    '--theme-report-issue-button-background-color-active': '#25a386',
    '--theme-report-issue-connecting-background-color': '#f9f9f9',
    '--theme-report-issue-icon-color': '#f9f9f9',
    '--theme-report-issue-connecting-text-color': '#2d2d2d',
    '--theme-report-issue-syncing-background-color': 'rgba(45, 45, 45, 0.05)',
    '--theme-report-issue-syncing-text-color': '#2d2d2d',
    '--theme-report-issue-syncing-download-logs-text-color': '#2d2d2d',
    '--theme-report-issue-syncing-download-logs-text-color-connecting':
      '#2d2d2d',
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': '#c7c7c7',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  settings: {
    '--theme-settings-body-background-color': '#f9f9f9',
    '--theme-settings-pane-background-color': '#fdfdfd',
    '--theme-settings-pane-border': 'none',
    '--theme-settings-menu-box-background-color': '#fdfdfd',
    '--theme-settings-menu-box-border': 'none',
    '--theme-settings-menu-item-text-color': '#2d2d2d',
    '--theme-settings-menu-item-text-color-active': '#2d2d2d',
    '--theme-settings-menu-item-text-color-disabled': '#2d2d2d80',
    '--theme-settings-menu-item-background-color-active':
      'rgba(45, 45, 45, 0.07)',
    '--theme-settings-menu-item-left-border-color-active': '#29b595',
    '--theme-settings-theme-select-title-color': '#2d2d2d',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#ffffff',
    '--theme-sidebar-category-background-color-hover': '#f9f9f9',
    '--theme-sidebar-category-background-color-active': '#f4f4f4',
    '--theme-sidebar-category-text-color': '#2d2d2d',
    '--theme-sidebar-menu-background-color': '#f4f4f4',
    '--theme-sidebar-menu-item-background-color-hover': '#eeeeee',
    '--theme-sidebar-menu-item-background-color-active': '#e9e9e9',
    '--theme-sidebar-menu-item-wallet-name-color': '#2d2d2d',
    '--theme-sidebar-menu-item-wallet-info-color': '#2d2d2d',
    '--theme-sidebar-menu-add-button-background-color': '#f4f4f4',
    '--theme-sidebar-menu-add-button-background-color-active': '#e9e9e9a8',
    '--theme-sidebar-menu-add-button-background-color-hover': '#eeeeeea8',
    '--theme-sidebar-menu-add-button-text-color': '#2d2d2d',
  },
  stakePools: {
    '--theme-staking-stake-pools-title-color': '#2d2d2d',
    '--theme-staking-stake-pools-search-button-color': '#2d2d2d',
    '--theme-staking-stake-pool-background-color': '#fdfdfd',
    '--theme-staking-stake-pool-border-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-staking-stake-pool-glow-color': 'rgba(94, 96, 102, 0.07)',
    '--theme-staking-stake-pools-search-icon-color': '#2d2d2d',
    '--theme-staking-stake-pool-selected-background-color': '#5da377',
    '--theme-staking-stake-pool-selected-checkmark-icon-color': '#fafbfc',
    '--theme-staking-stake-pool-selected-slug-color': '#fafbfc',
    '--theme-staking-stake-pool-slug-color': '#2d2d2d',
    '--theme-staking-stake-pool-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-background-color': '#fdfdfdf7',
    '--theme-staking-stake-pool-tooltip-border-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-staking-stake-pool-tooltip-text-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-link-color': '#29b595',
    '--theme-staking-stake-pool-tooltip-table-title-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-table-param-color': '#2d2d2d',
    '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-delegate-button-background-color':
      '#29b595',
    '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color':
      '#54c4aa',
    '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color':
      '#25a386',
    '--theme-staking-stake-pool-tooltip-delegate-button-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color':
      '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
      'transparent',
    '--theme-staking-progress-label-light': '#fafbfc',
  },
  staking: {
    '--theme-staking-content-background-color': '#fdfdfd',
    '--theme-staking-content-border-color': 'none',
    '--theme-staking-font-color-accent': '#2d2d2d',
    '--theme-staking-font-color-regular': '#2d2d2d',
    '--theme-staking-font-color-light': '#2d2d2db3',
    '--theme-staking-font-color-lighter': '#2d2d2d80',
    '--theme-staking-table-head-background-color': '#f9f9f9',
    '--theme-staking-table-border-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-staking-link-color': '#29b595',
    '--theme-staking-link-color-light': '#29b595',
    '--theme-staking-progress-bar-background-color': 'rgba(94, 96, 102, 0.07)',
    '--theme-staking-progress-stripe-dark-1-background-color': '#29b595',
    '--theme-staking-progress-stripe-dark-2-background-color': '#53C3AA',
    '--theme-staking-table-body-highlighted-text-color': '#29b595',
    '--theme-staking-info-learn-more-button-text-color': '#fdfdfd',
    '--theme-staking-info-learn-more-icon-color': '#fafbfc',
    '--theme-staking-learn-more-button-color': '#fdfdfd',
    '--theme-staking-learn-more-icon-color': '#fafbfc',
    '--theme-staking-donut-ring-completed-color': '#ea4c5b',
    '--theme-staking-donut-ring-remaining-color': '#fbdbde',
    '--theme-staking-wallet-row-border-color': 'rgba(45, 45, 45, 0.1)',
    '--theme-staking-dropdown-item-text-color-hover': '#2d2d2d',
    '--theme-staking-dropdown-item-background-color': '#fdfdfd',
    '--theme-staking-dropdown-item-background-color-hover':
      'rgba(94, 96, 102, 0.07)',
    '--theme-staking-delegation-center-gear-icon-fill-color': '#2d2d2d80',
    '--theme-staking-delegation-center-gear-icon-fill-color-active': '#2d2d2d',
    '--theme-staking-delegation-center-no-wallets-instructions-color':
      '#2d2d2d',
  },
  support: {
    '--theme-support-settings-item-color': '#2d2d2d',
    '--theme-support-settings-link-color': '#ffffff',
    '--theme-support-settings-text-color': '#2d2d2d',
  },
  syncing: {
    '--theme-syncing-background-color': '#f9f9f9',
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
    '--theme-topbar-background-color': '#ffffff',
    '--theme-topbar-layout-body-background-color': '#ffffff',
    '--theme-topbar-wallet-name-color': '#2d2d2d',
    '--theme-topbar-wallet-info-color': '#2d2d2d',
    '--theme-topbar-logo-color': '#2d2d2d',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#fff',
    '--theme-transactions-list-border-color': 'transparent',
    '--theme-transactions-list-group-date-color': '#2d2d2d',
    '--theme-transactions-list-item-details-color': '#2d2d2d',
    '--theme-transactions-state-failed-background-color':
      'rgba(189, 196, 206, 0.8)',
    '--theme-transactions-state-pending-background-color': '#e0e0e0',
    '--theme-transactions-state-pending-stripes-color': '#c7c7c7',
    '--theme-transactions-priority-color': '#f9f9f9',
    '--theme-transactions-priority-low-background-color': 'rgba(171, 23, 0, 1)',
    '--theme-transactions-priority-medium-background-color':
      'rgba(230, 170, 0, 1)',
    '--theme-transactions-priority-high-background-color':
      'rgba(0, 118, 0, 1);',
    '--theme-transactions-search-background-color': '#f9f9f9',
    '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
    '--theme-transactions-icon-type-income-background-color': '#2dc06c',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-icon-type-failed-background-color': '#ee707c',
    '--theme-transactions-arrow-stroke-color': '#2d2d2d',
    '--theme-transactions-state-failed-text-color': 'rgba(45, 45, 45, 0.4)',
    '--theme-transactions-state-failed-text-secondary-color': '#fafbfc',
  },
  uploader: {
    '--theme-uploader-text-color': '#2d2d2d',
    '--theme-uploader-border-color': 'rgba(45, 45, 45, 0.1)',
  },
  utxo: {
    '--theme-utxo-background-color': 'rgba(45, 45, 45, 0.05)',
    '--theme-utxo-title-text-color': '#2d2d2d',
    '--theme-utxo-title-description-color': '#2d2d2db3',
    '--theme-utxo-bar-color': 'rgba(41, 181, 149, 0.5)',
    '--theme-utxo-label-text-color': '#2d2d2d73',
    '--theme-utxo-tick-text-color': '#2d2d2d73',
    '--theme-utxo-cursor-background-color': 'rgba(41, 181, 149, 0.1)',
    '--theme-utxo-tooltip-background-color': 'rgba(41, 181, 149, 0.9)',
    '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-utxo-tooltip-text-color': '#fafbfc',
  },
  rpTooltip: {
    '--rp-tooltip-bg-color': '#2d2d2d',
    '--rp-tooltip-text-color': '#fafbfc',
  },
};

const WHITE_THEME_PARAMS: CreateThemeParams = {
  config: WHITE_THEME_CONFIG,
};

export default createTheme(WHITE_THEME_PARAMS);
