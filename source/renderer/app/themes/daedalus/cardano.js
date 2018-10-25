//  ==== react-polymorph: theme config === //
// AUTOCOMPLETE
const rpAutocomplete = {
  '--rp-autocomplete-bg-color': '#fafbfc',
  '--rp-autocomplete-border': '1px solid #c6cdd6',
  '--rp-autocomplete-border-focus-color': '#5e6066',
  '--rp-autocomplete-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-autocomplete-search-color': '#5e6066',
  '--rp-autocomplete-selected-word-text-color': '#fafbfc',
  '--rp-autocomplete-selected-word-box-background-color': 'rgba(44, 187, 105, 0.5)',
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
  '--rp-button-bg-color-active': '#239554',
  '--rp-button-bg-color-disabled': 'rgba(44, 187, 105, 0.3)',
  '--rp-button-bg-color-hover': '#56c887',
  '--rp-button-bg-color-primary': '#2cbb69',
  '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-button-text-color': '#fafbfc',
};

// CHECKBOX
const rpCheckbox = {
  '--rp-checkbox-border-disabled-color': 'rgba(44, 187, 105, 0.2)',
  '--rp-checkbox-label-disabled-color': 'rgba(94, 96, 102, 0.3)',
};

// COLORS
const rpColors = {
  '--rp-theme-color-error': '#ea4c5b',
};

// FONTS
const rpFonts = {
  '--rp-font-thin': 'NotoSans-Thin, NotoSansCJKjp-Thin',
  '--rp-font-light': 'NotoSans-Light, NotoSansCJKjp-Light',
  '--rp-font-medium': 'NotoSans-Medium, NotoSansCJKjp-Medium',
  '--rp-font-regular': 'NotoSans-Regular, NotoSansCJKjp-Regular',
  '--rp-font-bold': 'NotoSans-Bold, NotoSansCJKjp-Bold',
};

// FORMFIELD
const rpFormfield = {
  '--rp-formfield-bg-color-disabled': 'none',
  '--rp-formfield-label-color': '#5e6066',
  '--rp-formfield-label-color-disabled': '#5e6066',
  '--rp-formfield-error-color': '#ea4c5b',
};

// INPUT
const rpInput = {
  '--rp-input-bg-color': '#fafbfc',
  '--rp-input-bg-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-input-border-color': '#c6cdd6',
  '--rp-input-border-color-disabled': 'rgba(94, 96, 102, 0.05)',
  '--rp-input-border-color-focus': '#5e6066',
  '--rp-input-error-color': '#ea4c5b',
  '--rp-input-placeholder-color': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-placeholder-color-disabled': 'rgba(94, 96, 102, 0.5)',
  '--rp-input-text-color': '#5e6066',
};

// MODAL
const rpModal = {
  '--rp-modal-bg-color': '#fafbfc',
  '--rp-modal-overlay-color': 'rgba(0, 0, 0, 0.4)',
};

// OPTIONS
const rpOptions = {
  '--rp-option-bg-color': '#fafbfc',
  '--rp-option-border-color': '#c6cdd6',
  '--rp-option-checkmark-color': '#5e6066',
  '--rp-option-font-color': '#5e6066',
  '--rp-option-highlight-color': '#edeeef',
  '--rp-options-border-color': '#c6cdd6',
  '--rp-options-shadow': 'none',
};

// SELECT
const rpSelect = {
  '--rp-select-arrow-color': '#c6cdd6',
  '--rp-select-arrow-color-open': '#5e6066',
  '--rp-select-background-color': '#fafbfc',
  '--rp-select-input-border-focus-color': '#5e6066',
};

// SWITCH
const rpSwitch = {
  '--rp-switch-label-color': '#5e6066',
  '--rp-switch-off-accent-color': '#2cbb69',
  '--rp-switch-on-accent-color': '#2cbb69',
  '--rp-switch-thumb-accent-color': '#fff',
};

// TEXTAREA
const rpTextArea = {
  '--rp-textarea-bg-color': '#fafbfc',
  '--rp-textarea-border': '1px solid #c6cdd6',
  '--rp-textarea-border-focus-color': '#5e6066',
  '--rp-textarea-color': '#5e6066',
  '--rp-textarea-disabled-bg-color': 'rgba(94, 96, 102, 0.05)',
  '--rp-textarea-disabled-border-color': 'rgba(94, 96, 102, 0.05)',
  '--rp-textarea-error-color': '#ea4c5b',
  '--rp-textarea-placeholder-color': 'rgba(94, 96, 102, 0.5)',
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

//  ==== Theme: Cardano === //
export default {
  ...rpCardanoTheme,
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
  '--theme-ada-redemption-success-overlay-background-color': 'rgba(44, 187, 105, 0.88)',
  '--theme-ada-redemption-success-overlay-border-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-message-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-button-text-color': '#fafbfc',
  '--theme-ada-redemption-success-overlay-button-text-color-hover': '#2cbb69',
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
  '--theme-autocomplete-selected-word-box-background-color': 'rgba(44, 187, 105, 0.5)',
  '--theme-autocomplete-suggestions-list-border-color': '#c6cdd6',
  '--theme-autocomplete-suggestions-list-item-bg-color': '#fafbfc',
  '--theme-autocomplete-suggestions-list-item-font-color': '#5e6066',
  '--theme-autocomplete-suggestions-list-item-highlight-color': '#edeeef',

  '--theme-bordered-box-background-color': '#fafbfc',
  '--theme-bordered-box-border': '1px solid #c6cdd6',
  '--theme-bordered-box-text-color': '#5e6066',

  '--theme-button-primary-background-color': '#2cbb69',
  '--theme-button-primary-background-color-hover': '#56c887',
  '--theme-button-primary-background-color-active': '#239554',
  '--theme-button-primary-background-color-disabled': 'rgba(44, 187, 105, 0.3)',
  '--theme-button-primary-text-color-disabled': '#fafbfc',
  '--theme-button-primary-text-color': '#fafbfc',
  '--theme-button-primary-outline-color': '#35de7e',

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
  '--theme-checkbox-border-color': '#2cbb69',
  '--theme-checkbox-border-disabled-color': 'rgba(44, 187, 105, 0.2)',
  '--theme-checkbox-background-color-checked': '#2cbb69',

  '--theme-select-arrow-color': '#c6cdd6',
  '--theme-select-arrow-color-open': '#5e6066',
  '--theme-select-background-color': '#fafbfc',
  '--theme-select-option-highlight-color': '#edeeef',
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
  '--theme-dialog-big-button-background-color': '#f5f5f5',
  '--theme-dialog-big-button-border-color': '#e1e1e1',
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

  '--theme-main-body-background-color': '#efefef',
  '--theme-main-body-messages-color': '#5e6066',

  '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  '--theme-modal-background-color': '#fafbfc',

  '--theme-nav-item-background-color': '#202225',
  '--theme-nav-item-background-color-hover': 'rgba(250, 251, 252, 0.1)',
  '--theme-nav-item-background-color-active': '#fafbfc',
  '--theme-nav-item-text-color': '#cecfd1',
  '--theme-nav-item-text-color-active': '#5e6066',

  '--theme-node-update-background-color': '#efefef',
  '--theme-node-update-title-color': '#5e6066',
  '--theme-node-update-message-color': '#5e6066',
  '--theme-node-sync-info-message-background-color': '#fafbfc',
  '--theme-node-sync-info-message-text-color': '#5e6066',
  '--theme-node-update-accept-button-background-color': '#56c887',
  '--theme-node-update-accept-button-background-color-hover': '#2cbb69',
  '--theme-node-update-accept-button-background-color-active': '#239554',
  '--theme-node-update-deny-button-background-color': 'rgba(86, 200, 135, 0.3)',
  '--theme-node-update-deny-button-background-color-hover': 'rgba(44, 187, 105, 0.3)',
  '--theme-node-update-deny-button-background-color-active': 'rgba(35, 149, 84, 0.3)',
  '--theme-node-update-button-text-color': '#fafbfc',

  '--theme-notification-message-background-color': 'rgba(44, 187, 105, 0.8)',
  '--theme-notification-message-text-color': '#fafbfc',

  '--theme-receive-qr-code-background-color': 'transparent',
  '--theme-receive-qr-code-foreground-color': '#000',

  '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',

  '--theme-paper-wallet-create-certificate-dialog-recovery-phrase-background-color': 'rgba(68, 91, 124, 0.05)',
  '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': '#5e6066',
  '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': 'rgba(68, 91, 124, 0.05)',

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

  '--theme-staking-background-color': '#efefef',
  '--theme-staking-content-background-color': '#fafbfc',
  '--theme-staking-content-border-color': '#c6cdd6',
  '--theme-staking-font-color-accent': '#5e6066',
  '--theme-staking-font-color-regular': '#5e6066',

  '--theme-switch-background-color': '#2cbb69',
  '--theme-switch-background-color-checked': '#2cbb69',
  '--theme-switch-thumb-color-checked': '#fff',
  '--theme-switch-label-color': '#5e6066',

  '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
  '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
  '--theme-system-error-overlay-text-color': '#fafbfc',

  '--theme-test-environment-label-background-color': '#ab1700',
  '--theme-test-environment-label-text-color': '#fafbfc',

  '--theme-topbar-background-color': '#202225',
  '--theme-topbar-wallet-name-color': '#fafbfc',
  '--theme-topbar-wallet-info-color': '#fafbfc',
  '--theme-topbar-layout-body-background-color': '#ebeff2',

  '--theme-transactions-list-background-color': '#fafbfc',
  '--theme-transactions-list-border-color': '#c6cdd6',
  '--theme-transactions-list-group-date-color': '#a6a7aa',
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
  '--theme-transactions-arrow-stroke-color': '#5e6066',

  '--theme-uploader-text-color': '#5e6066',
  '--theme-uploader-border-color': '#c6cdd6',

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
  '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color': '#5e6066',
  '--theme-icon-add-wallet-dialog-big-button-color': '#ababab',
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

  '--theme-report-issue-button-background-color': '#2cbb69',
  '--theme-report-issue-button-background-color-hover': '#56c887',
  '--theme-report-issue-button-background-color-active': '#239554',
  '--theme-report-issue-connecting-background-color': 'rgba(250, 251, 252, 0.05)',
  '--theme-report-issue-connecting-text-color': '#fafbfc',
  '--theme-report-issue-syncing-background-color': 'rgba(94, 96, 102, 0.05)',
  '--theme-report-issue-syncing-text-color': '#5e6066',

  '--theme-connecting-background-color': '#202225',
  '--theme-connecting-text-color': '#fff',

  '--theme-syncing-background-color': '#fafbfc',
  '--theme-syncing-text-color': '#5e6066',

  '--theme-color-error': '#ea4c5b',

  '--theme-instructions-text-color': '#5e6066',

  '--theme-label-button-color': '#5e6066',

  '--theme-loading-background-color': '#fafbfc',

  '--theme-mnemonic-background-color': '#f0f3f5',
  '--theme-mnemonic-background-color-hover': '#f3f5f7',

  '--theme-separation-border-color': '#dfe4e8',

  '--theme-support-settings-text-color': '#5e6066',
  '--theme-support-settings-link-color': '#1cac63',

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
