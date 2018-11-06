//  ==== react-polymorph: theme config === //
// AUTOCOMPLETE
const rpAutocomplete = {
  '--rp-autocomplete-bg-color': '#fafbfc',
  '--rp-autocomplete-border': '1px solid #c6cdd6',
  '--rp-autocomplete-border-color-opened': '#5e6066',
  '--rp-autocomplete-input-text-color': '#5e6066',
  '--rp-autocomplete-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-autocomplete-selected-word-box-bg-color': 'rgba(68, 91, 124, 0.5)',
  '--rp-autocomplete-selected-word-text-color': '#fafbfc',
  '--rp-autocomplete-selected-words-font-family': 'NotoSans-Regular, NotoSansCJKjp-Regular',
};

// BUBBLE
const rpBubble = {
  '--rp-bubble-bg-color': '#fafbfc',
  '--rp-bubble-border-color': '#c6cdd6',
  '--rp-bubble-border-radius': '2px',
};

// BUTTON
const rpButton = {
  '--rp-button-bg-color': '#243e62',
  '--rp-button-bg-color-active': '#1e304a',
  '--rp-button-bg-color-disabled': 'rgba(36, 62, 98, 0.3)',
  '--rp-button-bg-color-hover': '#2f496e',
  '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-button-height': '50px',
  '--rp-button-line-height': '20px',
  '--rp-button-padding': '0',
  '--rp-button-text-color': '#fafbfc',
  '--rp-button-text-transform': 'none',
  '--rp-button-width': '360px',
};

// CHECKBOX
const rpCheckbox = {
  '--rp-checkbox-bg-color-checked': '#2f496e',
  '--rp-checkbox-border': '1px solid #2f496e',
  '--rp-checkbox-border-color-disabled': 'rgba(47, 73, 110, 0.2)',
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
  '--rp-formfield-bg-color-disabled': 'none !important',
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
  '--rp-input-padding': '12px 20px !important',
  '--rp-input-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-placeholder-color-disabled': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-text-color': '#5e6066',
  '--rp-input-text-color-disabled': 'rgba(94, 96, 102, 0.5)',
};

// MODAL
const rpModal = {
  '--rp-modal-bg-color': '#fafbfc',
  '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
};

// OPTIONS
const rpOptions = {
  '--rp-option-bg-color': '#fafbfc',
  '--rp-option-bg-color-highlighted': '#edf0f3',
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
  '--rp-select-input-border-color-focus': '#5e6066',
};

// SWITCH
const rpSwitch = {
  '--rp-switch-bg-color-off': '#2f496e',
  '--rp-switch-bg-color-on': '#2f496e',
  '--rp-switch-label-margin': '0 30px 0 0 !important',
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

const rpLightBlueTheme = {
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

//  ==== Theme: Light blue === //
export default {
  ...rpLightBlueTheme,
  '--preferred-font': '"Times New Roman", serif',

  '--font-ultralight': 'NotoSans-ExtraLight, NotoSansCJKjp-Thin',
  '--font-thin': 'NotoSans-Thin, NotoSansCJKjp-Thin',
  '--font-light': 'NotoSans-Light, NotoSansCJKjp-Light',
  '--font-regular': 'NotoSans-Regular, NotoSansCJKjp-Regular',
  '--font-medium': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--font-semibold': 'NotoSans-SemiBold, NotoSansCJKjp-Medium',
  '--font-bold': 'NotoSans-Bold, NotoSansCJKjp-Bold',
  '--font-heavy': 'NotoSans-ExtraBold, NotoSansCJKjp-Black',
  '--font-black': 'NotoSans-Black, NotoSansCJKjp-Black',

  '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',

  '--theme-scrollbar-thumb-background': '#c8ccce',

  '--theme-ada-redemption-headline-color': '#5e6066',
  '--theme-ada-redemption-instructions-color': '#5e6066',
  '--theme-ada-redemption-success-overlay-background-color': 'rgba(36, 61, 98, 0.88)',
  '--theme-ada-redemption-success-overlay-border-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-message-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-button-text-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-button-text-color-hover': '#243e62',
  '--theme-ada-redemption-success-overlay-button-background-color-hover': '#fafbfc',
  '--theme-ada-redemption-disclaimer-background-color': 'rgba(171, 23, 0, 0.94)',
  '--theme-ada-redemption-disclaimer-text-color': '#fafbfc',
  '--theme-ada-redemption-disclaimer-checkbox-color-check': '#fafbfc',
  '--theme-ada-redemption-disclaimer-checkbox-color-checked': '#fafbfc',
  '--theme-ada-redemption-disclaimer-checkbox-color-after': '#ab1700',
  '--theme-ada-redemption-disclaimer-checkbox-label-color': '#fafbfc',
  '--theme-ada-redemption-disclaimer-button-border-color': '#fafbfc',
  '--theme-ada-redemption-no-wallets-instructions-color': '#5e6066',

  '--theme-autocomplete-bg-color': '#fafbfc',
  '--theme-autocomplete-border': '1px solid #c6cdd6',
  '--theme-autocomplete-border-focus-color': '#5e6066',
  '--theme-autocomplete-search-color': '#5e6066',
  '--theme-autocomplete-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--theme-autocomplete-selected-word-text-color': '#fafbfc',
  '--theme-autocomplete-selected-word-box-background-color': 'rgba(68, 91, 124, 0.5)',
  '--theme-autocomplete-suggestions-list-border-color': '#c6cdd6',
  '--theme-autocomplete-suggestions-list-item-bg-color': '#fafbfc',
  '--theme-autocomplete-suggestions-list-item-font-color': '#5e6066',
  '--theme-autocomplete-suggestions-list-item-highlight-color': '#edf0f3',

  '--theme-bordered-box-background-color': '#fafbfc',
  '--theme-bordered-box-border': '1px solid #c6cdd6',
  '--theme-bordered-box-text-color': '#5e6066',

  '--theme-button-primary-background-color': '#243e62',
  '--theme-button-primary-background-color-hover': '#2f496e',
  '--theme-button-primary-background-color-active': '#1e304a',
  '--theme-button-primary-background-color-disabled': 'rgba(36, 62, 98, 0.3)',
  '--theme-button-primary-text-color-disabled': '#fafbfc',
  '--theme-button-primary-text-color': '#fafbfc',
  '--theme-button-primary-outline-color': '#4a7ec7',

  '--theme-button-flat-background-color': '#f0f3f5',
  '--theme-button-flat-background-color-hover': '#f3f5f7',
  '--theme-button-flat-background-color-active': '#ebeef0',
  '--theme-button-flat-background-color-disabled': 'rgba(240, 243, 245, 0.3)',
  '--theme-button-flat-text-color-disabled': '#5e6066',
  '--theme-button-flat-text-color': '#5e6066',
  '--theme-button-flat-outline-color': '#e1e4e6',

  '--theme-button-attention-background-color': '#ea4c5b',
  '--theme-button-attention-background-color-hover': '#ec5d6b',
  '--theme-button-attention-background-color-active': '#d34452',
  '--theme-button-attention-background-color-disabled': 'rgba(234, 76, 91, 0.3)',
  '--theme-button-attention-text-color-disabled': '#fafbfc',
  '--theme-button-attention-text-color': '#fafbfc',
  '--theme-button-attention-outline-color': '#ff707e',

  '--theme-button-disclaimer-background-color': '#ab1700',
  '--theme-button-disclaimer-background-color-hover': '#fafbfc',
  '--theme-button-disclaimer-background-color-active': '#fafbfc',
  '--theme-button-disclaimer-background-color-disabled': 'rgba(171, 23, 0, .3)',
  '--theme-button-disclaimer-text-color-disabled': 'rgba(250, 251, 252, .3)',
  '--theme-button-disclaimer-text-color': '#fafbfc',
  '--theme-button-disclaimer-outline-color': 'rgba(250, 251, 252, .3)',
  '--theme-button-disclaimer-border-color': '#fafbfc',
  '--theme-button-disclaimer-border-color-disabled': 'rgba(250, 251, 252, .3)',

  '--theme-checkbox-label-color': '#5e6066',
  '--theme-checkbox-label-disabled-color': 'rgba(94, 96, 102, 0.3)',
  '--theme-checkbox-border-color': '#2f496e',
  '--theme-checkbox-border-disabled-color': 'rgba(47, 73, 110, 0.2)',
  '--theme-checkbox-background-color-checked': '#2f496e',

  '--theme-select-arrow-color': '#c6cdd6',
  '--theme-select-arrow-color-open': '#5e6066',
  '--theme-select-background-color': '#fafbfc',
  '--theme-select-option-highlight-color': '#edf0f3',
  '--theme-select-option-text-color': '#5e6066',
  '--theme-select-options-border-color': '#c6cdd6',
  '--theme-select-options-shadow': 'none',
  '--theme-select-options-checkmark-color': '#5e6066',

  '--theme-choice-tabs-text-color': '#5e6066',
  '--theme-choice-tabs-text-color-active': '#5e6066',
  '--theme-choice-tabs-bottom-border-color-active': '#5e6066',

  '--theme-dialog-choice-tabs-text-color': '#5e6066',
  '--theme-dialog-choice-tabs-text-color-active': '#5e6066',
  '--theme-dialog-choice-tabs-bottom-border-color-active': '#5e6066',
  '--theme-dialog-big-button-background-color': '#FAFBFC',
  '--theme-dialog-big-button-border-color': '#c6cdd6',
  '--theme-dialog-big-button-label-color': '#5e6066',
  '--theme-dialog-big-button-description-color': '#5e6066',
  '--theme-dialog-title-color': '#5e6066',

  '--theme-input-border-color': '#c6cdd6',
  '--theme-input-label-color': '#5e6066',
  '--theme-input-text-color': '#5e6066',
  '--theme-input-right-floating-text-color': 'rgba(94, 96, 102, 0.5)',
  '--theme-input-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--theme-input-error-color': '#ea4c5b',
  '--theme-input-remove-color-dark': '#d34452',
  '--theme-input-remove-color-light': '#ea4c5b',
  '--theme-input-remove-color-lighter': '#ec5d6b',
  '--theme-input-remove-color-lightest': '#fac8ce',
  '--theme-input-background-color': '#fafbfc',
  '--theme-input-disabled-background-color': 'rgba(94, 96, 102, 0.05)',
  '--theme-input-disabled-border-color': 'rgba(94, 96, 102, 0.05)',
  '--theme-input-focus-border-color': '#5e6066',

  '--theme-main-body-background-color': '#ebeff2',
  '--theme-main-body-messages-color': '#5e6066',

  '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  '--theme-modal-background-color': '#fafbfc',

  '--theme-nav-item-background-color': '#1c2e47',
  '--theme-nav-item-background-color-hover': 'rgba(250, 251, 252, 0.1)',
  '--theme-nav-item-background-color-active': '#fafbfc',
  '--theme-nav-item-text-color': 'rgba(250,251,252, 0.6)',
  '--theme-nav-item-text-color-active': '#1d2e47',

  '--theme-node-update-background-color': '#ebeff2',
  '--theme-node-update-title-color': '#5e6066',
  '--theme-node-update-message-color': '#5e6066',
  '--theme-node-sync-info-message-background-color': '#fafbfc',
  '--theme-node-sync-info-message-text-color': '#5e6066',
  '--theme-node-update-accept-button-background-color': '#445b7c',
  '--theme-node-update-accept-button-background-color-hover': '#243e62',
  '--theme-node-update-accept-button-background-color-active': '#1e304a',
  '--theme-node-update-deny-button-background-color': 'rgba(68, 91, 124, 0.3)',
  '--theme-node-update-deny-button-background-color-hover': 'rgba(36, 62, 98, 0.3)',
  '--theme-node-update-deny-button-background-color-active': 'rgba(30, 48, 74, 0.3)',
  '--theme-node-update-button-text-color': '#fafbfc',

  '--theme-notification-message-background-color': 'rgba(36, 61, 98, 0.8)',
  '--theme-notification-message-text-color': '#fafbfc',

  '--theme-receive-qr-code-background-color': 'transparent',
  '--theme-receive-qr-code-foreground-color': '#000',

  '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',

  '--theme-paper-wallet-create-certificate-dialog-recovery-phrase-background-color': 'rgba(68, 91, 124, 0.05)',
  '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': '#5e6066',
  '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': 'rgba(68, 91, 124, 0.05)',

  '--theme-settings-body-background-color': '#ebeff2',
  '--theme-settings-pane-background-color': '#fafbfc',
  '--theme-settings-pane-border': '1px solid #c6cdd6',
  '--theme-settings-menu-box-background-color': '#fafbfc',
  '--theme-settings-menu-box-border': '1px solid #c6cdd6',
  '--theme-settings-menu-item-text-color': '#5e6066',
  '--theme-settings-menu-item-text-color-active': '#5e6066',
  '--theme-settings-menu-item-text-color-disabled': '#b3b3b3',
  '--theme-settings-menu-item-background-color-active': '#edf0f3',
  '--theme-settings-menu-item-left-border-color-active': '#445b7c',
  '--theme-settings-theme-select-title-color': '#5e6066',

  '--theme-sidebar-background-color': '#345078',
  '--theme-sidebar-category-background-color-hover': '#2e4669',
  '--theme-sidebar-category-background-color-active': '#233856',
  '--theme-sidebar-category-text-color': '#fafbfc',
  '--theme-sidebar-menu-background-color': '#233856',
  '--theme-sidebar-menu-item-background-color-hover': 'rgba(30, 48, 74, 0.5)',
  '--theme-sidebar-menu-item-background-color-active': '#1e304a',
  '--theme-sidebar-menu-item-wallet-name-color': '#fafbfc',
  '--theme-sidebar-menu-item-wallet-info-color': '#fafbfc',
  '--theme-sidebar-menu-add-button-background-color': '#1e304a',
  '--theme-sidebar-menu-add-button-background-color-active': '#1f324d',
  '--theme-sidebar-menu-add-button-background-color-hover': '#1f324d',
  '--theme-sidebar-menu-add-button-text-color': '#fafbfc',

  '--theme-staking-background-color': '#ebeff2',
  '--theme-staking-content-background-color': '#fafbfc',
  '--theme-staking-content-border-color': '#c6cdd6',
  '--theme-staking-font-color-accent': '#5e6066',
  '--theme-staking-font-color-regular': '#5e6066',

  '--theme-switch-background-color': '#2f496e',
  '--theme-switch-background-color-checked': '#2f496e',
  '--theme-switch-thumb-color-checked': '#fff',
  '--theme-switch-label-color': '#5e6066',

  '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
  '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
  '--theme-system-error-overlay-text-color': '#fafbfc',

  '--theme-test-environment-label-background-color': '#ab1700',
  '--theme-test-environment-label-text-color': '#fafbfc',

  '--theme-topbar-background-color': '#243e62',
  '--theme-topbar-wallet-name-color': '#fafbfc',
  '--theme-topbar-wallet-info-color': '#fafbfc',
  '--theme-topbar-layout-body-background-color': '#ebeff2',

  '--theme-transactions-list-background-color': '#fafbfc',
  '--theme-transactions-list-border-color': '#c6cdd6',
  '--theme-transactions-list-group-date-color': '#5e6066',
  '--theme-transactions-list-item-details-color': '#5e6066',
  '--theme-transactions-card-background-color': '#e6ebf2',
  '--theme-transactions-card-income-background-color': '#e6ebf2',
  '--theme-transactions-card-expend-background-color': '#f2e6e6',
  '--theme-transactions-card-exchange-background-color': '#f2e6e6',
  '--theme-transactions-state-failed-background-color': '#bdc5ce',
  '--theme-transactions-state-failed-text-color': '#bdc5ce',
  '--theme-transactions-state-pending-background-color': '#bdc5ce',
  '--theme-transactions-state-pending-stripes-color': '#b2bac2',
  '--theme-transactions-priority-color': '#fafbfc',
  '--theme-transactions-priority-low-background-color': '#ab1700',
  '--theme-transactions-priority-medium-background-color': '#e6aa00',
  '--theme-transactions-priority-heigh-background-color': '#007600',
  '--theme-transactions-search-background-color': '#fafbfc',
  '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
  '--theme-transactions-icon-type-income-background-color': '#2dc06c',
  '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
  '--theme-transactions-icon-type-failed-background-color': '#ea4c5b',
  '--theme-transactions-arrow-stroke-color': '#243e62',

  '--theme-uploader-text-color': '#5e6066',
  '--theme-uploader-border-color': '#c6cdd6',

  '--theme-icon-nav-color': 'rgba(250, 251, 252, 0.6)',
  '--theme-icon-nav-color-active': '#1d2e47',
  '--theme-icon-sidebar-color': '#fafbfc',
  '--theme-icon-toggle-menu-color': '#fafbfc',
  '--theme-icon-node-update-notification-arrow-color': '#5e6066',
  '--theme-icon-add-wallet-from-sidebar-color': '#fafbfc',
  '--theme-icon-ada-redemption-attention-color': '#fafbfc',
  '--theme-icon-ada-redemption-success-color': '#fafbfc',
  '--theme-icon-ada-redemption-certificate-color': '#9eabbb',
  '--theme-icon-ada-redemption-no-wallets': '#5e6066',
  '--theme-icon-ada-summary-wallet-amount-symbol-color': '#5e6066',
  '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color': '#5e6066',
  '--theme-icon-add-wallet-dialog-big-button-color': '#8a98ab',
  '--theme-icon-copy-address-color': '#5e6066',
  '--theme-icon-back-button-color': '#5e6066',
  '--theme-icon-close-button-color': '#5e6066',
  '--theme-icon-file-upload-color': '#5e6066',
  '--theme-icon-transactions-ada-symbol-color': '#5e6066',
  '--theme-icon-syncing-logo-color': '#5e6066',
  '--theme-icon-connecting-logo-color': '#fafbfc',
  '--theme-icon-transaction-type-color': '#fafbfc',

  '--theme-about-window-background-color': '#fafbfc',
  '--theme-about-window-header-bottom-border-color': '#dfe4e8',
  '--theme-about-window-daedalus-icon-color': '#5e6066',
  '--theme-about-window-cardano-icon-color': '#5e6066',
  '--theme-about-window-title-varsion-color': '#5e6066',
  '--theme-about-window-title-stroke-color': '#5e6066',
  '--theme-about-window-content-text-color': '#5e6066',
  '--theme-about-window-content-bottom-border-color': '#dfe4e8',
  '--theme-about-window-copyright-color': '#5e6066',

  '--theme-backup-mnemonic-background-color': 'rgba(68, 91, 124, 0.05)',

  '--theme-block-generation-info-color': '#5e6066',

  '--theme-report-issue-button-background-color': '#445b7c',
  '--theme-report-issue-button-background-color-hover': '#697b96',
  '--theme-report-issue-button-background-color-active': '#364863',
  '--theme-report-issue-connecting-background-color': 'rgba(250, 251, 252, 0.05)',
  '--theme-report-issue-connecting-text-color': '#fafbfc',
  '--theme-report-issue-syncing-background-color': 'rgba(94, 96, 102, 0.05)',
  '--theme-report-issue-syncing-text-color': '#5e6066',

  '--theme-connecting-background-color': '#1c3e65',
  '--theme-connecting-text-color': '#fff',

  '--theme-syncing-background-color': '#fafbfc',
  '--theme-syncing-text-color': '#5e6066',

  '--theme-color-error': '#ea4c5b',

  '--theme-instructions-text-color': '#5e6066',

  '--theme-label-button-color': '#5e6066',

  '--theme-loading-background-color': '#fafbfc',

  '--theme-mnemonic-background-color': '#e6e9ee',
  '--theme-mnemonic-background-color-hover': '#eaecf1',

  '--theme-separation-border-color': '#dfe4e8',

  '--theme-support-settings-text-color': '#5e6066',
  '--theme-support-settings-link-color': '#2572cc',

  '--theme-terms-of-use-text-color': '#5e6066',

  '--theme-data-migration-layer-background-color': '#2c4567',
  '--theme-data-migration-layer-box-shadow-color': 'rgba(0, 0, 0, 0.25)',
  '--theme-data-migration-layer-button-background-color-hover': '#fafbfc',
  '--theme-data-migration-layer-text-color': '#fafbfc',
  '--theme-data-migration-layer-text-opacity-color': 'rgba(250, 251, 252, 0.5)',

  '--theme-wallet-password-switch-label-color': '#5e6066',

  '--theme-password-toggler-color': '#5e6066',

  '--theme-loading-spinner-color': '#5e6066',

  '--theme-progress-bar-background-color': 'rgba(255, 255, 255, 0.3)',
  '--theme-progress-bar-foreground-color': 'rgba(255, 255, 255, 0.7)',
};
