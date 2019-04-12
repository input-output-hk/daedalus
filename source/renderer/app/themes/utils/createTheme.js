// @flow
import { isEmpty } from 'lodash';
import type { ThemeColors, ThemeFonts, CreateThemeParams } from '../types';

type CreateThemePartial = {
  colors: ThemeColors,
  fonts: ThemeFonts,
};

const createReactPolymorphTheme = (themeParts: CreateThemePartial): Object => {
  // deconstruct colors & fonts
  const { colors, fonts } = themeParts;
  const { primary, secondary, error } = colors;

  // assign all react-polymorph CSS variables
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

const createDaedalusComponentsTheme = (
  themeParts: CreateThemePartial
): Object => {
  const { colors, fonts } = themeParts;
  const { primary, secondary, error } = colors;
  return {
    aboutWindow: {
      '--theme-about-window-background-color': `${}`,
      '--theme-about-window-header-bottom-border-color': `${}`,
      '--theme-about-window-daedalus-icon-color': `${}`,
      '--theme-about-window-cardano-icon-color': `${}`,
      '--theme-about-window-title-varsion-color': `${}`,
      '--theme-about-window-title-stroke-color': `${}`,
      '--theme-about-window-content-color': `${}`,
      '--theme-about-window-content-text-color': `${}`,
      '--theme-about-window-content-bottom-border-color': `${}`,
      '--theme-about-window-icon-close-button-color': `${}`,
      '--theme-about-window-icon-close-hover-background': `${}`,
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
      '--theme-ada-redemption-success-overlay-background-color': `${}`,
      '--theme-ada-redemption-success-overlay-button-background-color-hover': `${}`,
      '--theme-ada-redemption-disclaimer-background-color': `${}`,
      '--theme-ada-redemption-disclaimer-text-color': `${}`,
      '--theme-ada-redemption-disclaimer-checkbox-color-check': `${}`,
      '--theme-ada-redemption-disclaimer-checkbox-color-checked': `${}`,
      '--theme-ada-redemption-disclaimer-checkbox-color-after': `${}`,
      '--theme-ada-redemption-disclaimer-checkbox-label-color': `${}`,
      '--theme-ada-redemption-no-wallets-instructions-color': `${}`,
    },
    blockConsolidation: {
      '--theme-block-consolidation-background-color': `${}`,
      '--theme-block-consolidation-title-text-color': `${}`,
      '--theme-block-consolidation-text-color': `${}`,
      '--theme-block-consolidation-text-highlight-color': `${}`,
      '--theme-block-consolidation-epochs-text-color': `${}`,
      '--theme-block-consolidation-indicator-text-color': `${}`,
      '--theme-block-consolidation-indicator-container-background-color': `${}`,
      '--theme-block-consolidation-indicator-epochs-behind-background-color-1': `${}`,
      '--theme-block-consolidation-indicator-epochs-behind-background-color-2': `${}`,
      '--theme-block-consolidation-stripe-dark-1-background-color': `${}`,
      '--theme-block-consolidation-stripe-dark-2-background-color': `${}`,
      '--theme-block-consolidation-stripe-light-1-background-color': `${}`,
      '--theme-block-consolidation-stripe-light-2-background-color': `${}`,
      '--theme-block-consolidation-button-background-color': `${}`,
      '--theme-block-consolidation-button-background-color-hover': `${}`,
      '--theme-block-consolidation-button-text-color-hover': `${}`,
      '--theme-block-consolidation-button-border-color': `${}`,
    },
    body: {
      '--theme-main-body-background-color': `${}`,
      '--theme-main-body-messages-color': `${}`,
    },
    borderedBox: {
      '--theme-bordered-box-background-color': `${}`,
      '--theme-bordered-box-border': `1px solid ${}`,
      '--theme-bordered-box-text-color': `${}`,
    },
    button: {
      '--theme-label-button-color': `${}`,
    },
    buttonAttention: {
      '--theme-button-attention-background-color': `${}`,
    },
    buttonDisclaimer: {
      '--theme-button-disclaimer-background-color': `${}`,
      '--theme-button-disclaimer-text-color-disabled': `${}`,
      '--theme-button-disclaimer-border-color': `${}`,
      '--theme-button-disclaimer-border-color-disabled': `${}`,
    },
    connecting: {
      '--theme-connecting-background-color': `${}`,
      '--theme-connecting-text-color': `${}`,
    },
    dataMigration: {
      '--theme-data-migration-layer-background-color': `${}`,
      '--theme-data-migration-layer-box-shadow-color': `${}`,
      '--theme-data-migration-layer-button-background-color-hover': `${}`,
      '--theme-data-migration-layer-text-color': `${}`,
      '--theme-data-migration-layer-text-opacity-color': `${}`,
    },
    dialog: {
      '--theme-dialog-choice-tabs-text-color': `${}`,
      '--theme-dialog-choice-tabs-text-color-active': `${}`,
      '--theme-dialog-choice-tabs-bottom-border-color-active': `${}`,
      '--theme-dialog-big-button-background-color': `${}`,
      '--theme-dialog-big-button-border-color': `${}`,
      '--theme-dialog-big-button-label-color': `${}`,
      '--theme-dialog-big-button-description-color': `${}`,
      '--theme-dialog-title-color': `${}`,
      '--theme-dialog-text-color': `${}`,
      '--theme-dialog-border-color': `${}`,
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
      '--theme-icon-nav-color': `${}`,
      '--theme-icon-nav-color-active': `${}`,
      '--theme-icon-sidebar-color': `${}`,
      '--theme-icon-toggle-menu-color': `${}`,
      '--theme-icon-node-update-notification-arrow-color': `${}`,
      '--theme-icon-add-wallet-from-sidebar-color': `${}`,
      '--theme-icon-ada-redemption-attention-color': `${}`,
      '--theme-icon-ada-redemption-success-color': `${}`,
      '--theme-icon-ada-redemption-certificate-color': `${}`,
      '--theme-icon-ada-redemption-no-wallets': `${}`,
      '--theme-icon-ada-summary-wallet-amount-symbol-color': `${}`,
      '--theme-icon-ada-summary-wallet-pending-confirmation-symbol-color': `${}`,
      '--theme-icon-add-wallet-dialog-big-button-color': `${}`,
      '--theme-icon-copy-address-color': `${}`,
      '--theme-icon-back-button-color': `${}`,
      '--theme-icon-close-button-color': `${}`,
      '--theme-icon-file-upload-color': `${}`,
      '--theme-icon-transactions-ada-symbol-color': `${}`,
      '--theme-icon-syncing-logo-color': `${}`,
      '--theme-icon-connecting-logo-color': `${}`,
      '--theme-icon-transaction-type-color': `${}`,
    },
    input: {
      '--theme-input-border-color': `${}`,
      '--theme-input-label-color': `${}`,
      '--theme-input-text-color': `${}`,
      '--theme-input-right-floating-text-color': `${}`,
      '--theme-input-placeholder-color': `${}`,
      '--theme-input-remove-color-light': `${}`,
      '--theme-input-background-color': `${}`,
      '--theme-input-focus-border-color': `${}`,
      '--theme-input-hint-font': `${fonts.regular}`,
    },
    loading: {
      '--theme-loading-background-color': `${}`,
      '--theme-loading-no-disk-space-background-color': `${}`,
      '--theme-loading-no-disk-space-text-color': `${}`,
      '--theme-loading-no-disk-space-attention-icon-color': `${}`,
      '--theme-loading-status-icons-on-color': `${}`,
      '--theme-loading-status-icons-off-color': `${}`,
      '--theme-loading-status-icons-unloaded-loading-color': `${}`,
      '--theme-loading-status-icons-unloaded-syncing-color': `${}`,
      '--theme-loading-status-icons-tooltip-color': `${}`,
      '--theme-loading-spinner-color': `${}`,
    },
    mnemonic: {
      '--theme-mnemonic-background-color': `${}`,
      '--theme-mnemonic-background-color-hover': `${}`,
      '--theme-backup-mnemonic-background-color': `${}`,
    },
    modal: {
      '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
    },
    navItem: {
      '--theme-nav-item-background-color': `${}`,
      '--theme-nav-item-background-color-hover': `${}`,
      '--theme-nav-item-background-color-active': `${}`,
      '--theme-nav-item-text-color': `${}`,
      '--theme-nav-item-text-color-active': `${}`,
    },
    nodeUpdate: {
      '--theme-node-update-background-color': `${}`,
      '--theme-node-update-title-color': `${}`,
      '--theme-node-update-message-color': `${}`,
      '--theme-node-sync-info-message-background-color': `${}`,
      '--theme-node-sync-info-message-text-color': `${}`,
      '--theme-node-update-accept-button-background-color': `${}`,
      '--theme-node-update-accept-button-background-color-hover': `${}`,
      '--theme-node-update-accept-button-background-color-active': `${}`,
      '--theme-node-update-deny-button-background-color': `${}`,
      '--theme-node-update-deny-button-background-color-hover': `${}`,
      '--theme-node-update-deny-button-background-color-active': `${}`,
      '--theme-node-update-button-text-color': `${}`,
    },
    notification: {
      '--theme-notification-message-background-color': `${}`,
      '--theme-notification-message-text-color': `${}`,
    },
    paperWallet: {
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-color': `${}`,
      '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color': `${}`,
    },
    progressBar: {
      '--theme-progress-bar-background-color': `${}`,
      '--theme-progress-bar-foreground-color': `${}`,
    },
    receiveQRCode: {
      '--theme-receive-qr-code-background-color': 'transparent',
      '--theme-receive-qr-code-foreground-color': '#000',
    },
    reportIssue: {
      '--theme-report-issue-button-background-color': `${}`,
      '--theme-report-issue-button-background-color-hover': `${}`,
      '--theme-report-issue-button-background-color-active': `${}`,
      '--theme-report-issue-connecting-background-color': `${}`,
      '--theme-report-issue-connecting-text-color': `${}`,
      '--theme-report-issue-syncing-background-color': `${}`,
      '--theme-report-issue-syncing-text-color': `${}`,
      '--theme-report-issue-syncing-download-logs-text-color': `${}`,
      '--theme-report-issue-syncing-download-logs-text-color-connecting': `${}`,
    },
    scrollbar: {
      '--theme-scrollbar-thumb-background': `${}`,
    },
    sendConfirmation: {
      '--theme-send-confirmation-dialog-send-values-color': `${}`,
    },
    settings: {
      '--theme-settings-body-background-color': `${}`,
      '--theme-settings-pane-background-color': `${}`,
      '--theme-settings-pane-border': `1px solid ${}`,
      '--theme-settings-menu-box-background-color': `${}`,
      '--theme-settings-menu-box-border': `1px solid ${}`,
      '--theme-settings-menu-item-text-color': `${}`,
      '--theme-settings-menu-item-text-color-active': `${}`,
      '--theme-settings-menu-item-text-color-disabled': `${}`,
      '--theme-settings-menu-item-background-color-active': `${}`,
      '--theme-settings-menu-item-left-border-color-active': `${}`,
      '--theme-settings-theme-select-title-color': `${}`,
    },
    sidebar: {
      '--theme-sidebar-background-color': `${}`,
      '--theme-sidebar-category-background-color-hover': `${}`,
      '--theme-sidebar-category-background-color-active': `${}`,
      '--theme-sidebar-category-text-color': `${}`,
      '--theme-sidebar-menu-background-color': `${}`,
      '--theme-sidebar-menu-item-background-color-hover': `${}`,
      '--theme-sidebar-menu-item-background-color-active': `${}`,
      '--theme-sidebar-menu-item-wallet-name-color': `${}`,
      '--theme-sidebar-menu-item-wallet-info-color': `${}`,
      '--theme-sidebar-menu-add-button-background-color': `${}`,
      '--theme-sidebar-menu-add-button-background-color-active': `${}`,
      '--theme-sidebar-menu-add-button-background-color-hover': `${}`,
      '--theme-sidebar-menu-add-button-text-color': `${}`,
    },
    staking: {
      '--theme-staking-background-color': `${}`,
      '--theme-staking-content-background-color': `${}`,
      '--theme-staking-content-border-color': `${}`,
      '--theme-staking-font-color-accent': `${}`,
      '--theme-staking-font-color-regular': `${}`,
    },
    support: {
      '--theme-support-settings-text-color': `${}`,
      '--theme-support-settings-link-color': `${}`,
      '--theme-support-settings-item-color': `${}`,
    },
    syncing: {
      '--theme-syncing-background-color': `${}`,
      '--theme-syncing-text-color': `${}`,
    },
    systemError: {
      '--theme-system-error-overlay-attention-icon-color': `${}`,
      '--theme-system-error-overlay-background-color': `${}`,
      '--theme-system-error-overlay-text-color': `${}`,
    },
    tabs: {
      '--theme-choice-tabs-text-color': `${}`,
      '--theme-choice-tabs-text-color-active': `${}`,
      '--theme-choice-tabs-bottom-border-color-active': `${}`,
    },
    testEnvironment: {
      '--theme-test-environment-label-background-color': `${}`,
      '--theme-test-environment-label-text-color': `${}`,
    },
    topBar: {
      '--theme-topbar-background-color': `${}`,
      '--theme-topbar-wallet-name-color': `${}`,
      '--theme-topbar-wallet-info-color': `${}`,
      '--theme-topbar-layout-body-background-color': `${}`,
    },
    transactions: {
      '--theme-transactions-list-background-color': `${}`,
      '--theme-transactions-list-border-color': `${}`,
      '--theme-transactions-list-group-date-color': `${}`,
      '--theme-transactions-list-item-details-color': `${}`,
      '--theme-transactions-card-background-color': `${}`,
      '--theme-transactions-card-income-background-color': `${}`,
      '--theme-transactions-card-expend-background-color': `${}`,
      '--theme-transactions-card-exchange-background-color': `${}`,
      '--theme-transactions-state-failed-background-color': `${}`,
      '--theme-transactions-state-failed-text-color': `${}`,
      '--theme-transactions-state-pending-background-color': `${}`,
      '--theme-transactions-state-pending-stripes-color': `${}`,
      '--theme-transactions-priority-color': `${}`,
      '--theme-transactions-priority-low-background-color': `${}`,
      '--theme-transactions-priority-medium-background-color': '#e6aa00',
      '--theme-transactions-priority-heigh-background-color': '#007600',
      '--theme-transactions-search-background-color': `${}`,
      '--theme-transactions-icon-type-expend-background-color': '#84a2d2',
      '--theme-transactions-icon-type-income-background-color': `${}`,
      '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
      '--theme-transactions-icon-type-failed-background-color': `${}`,
      '--theme-transactions-arrow-stroke-color': `${}`,
    },
    uploader: {
      '--theme-uploader-text-color': `${}`,
      '--theme-uploader-border-color': `${}`,
    },
  };
};

export const createTheme = (fullThemeParts: CreateThemeParams): Object => {
  const { colors, config, fonts } = fullThemeParts;
  // create react-polymorph & daedalus theme, combine into a theme object
  let daedalusTheme = {
    ...createReactPolymorphTheme({ colors, fonts }),
    ...createDaedalusComponentsTheme({ colors, fonts }),
  };
  // if user passed theme config, compose with theme object and return
  if (config && !isEmpty(config)) {
    daedalusTheme = { ...daedalusTheme, ...config };
  }
  // no theme config, return theme object
  return daedalusTheme;
};
