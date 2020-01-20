// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

//  ==== dark-blue theme output for Daedalus and react-polymorph components === //
export const DARK_BLUE_THEME_OUTPUT = {
  aboutWindow: {
    '--theme-about-window-background-color': 'rgba(38, 51, 69, 0.96)',
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
  backToTopButton: {
    '--theme-back-to-top-button-background-color': '#536370',
    '--theme-back-to-top-button-text-color': '#e9f4fe',
    '--theme-back-to-top-button-box-shadow-color': 'rgba(0, 0, 0, 0.36)',
  },
  automaticUpdate: {
    '--theme-automatic-update-overlay-background-color':
      'rgba(38, 51, 69, 0.96)',
    '--theme-automatic-update-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-automatic-update-overlay-button-background-color-hover': '#fafbfc',
    '--theme-automatic-update-overlay-button-icon-color':
      'rgba(250, 251, 252, 1)',
    '--theme-automatic-update-overlay-button-icon-color-hover':
      'rgba(38, 51, 69, 1)',
    '--theme-automatic-update-overlay-button-label-color-hover':
      'rgba(38, 51, 69, 1)',
    '--theme-automatic-update-overlay-button-text-color-hover': '#263345',
    '--theme-automatic-update-overlay-button-border-color': '#fafbfc',
    '--theme-automatic-update-overlay-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-automatic-update-overlay-text-highlight-color': '#fafbfc',
    '--theme-automatic-update-overlay-title-text-color': '#fafbfc',
    '--theme-automatic-update-overlay-button-label-color': '#fafbfc',
    '--theme-automatic-update-overlay-button-label-color-light':
      'rgba(250, 251, 252, 0.8)',
    '--theme-automatic-update-overlay-close-button-color': '#fff',
    '--theme-automatic-update-overlay-close-button-hover-background':
      'rgba(0, 0, 0, 0.1)',
  },
  body: {
    '--theme-main-body-background-color': '#0b1926',
    '--theme-main-body-messages-color': '#e9f4fe',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#263345',
    '--theme-bordered-box-border': 'none',
    '--theme-bordered-box-text-color': '#e9f4fe',
  },
  button: {
    '--theme-button-spinner-color': '#fafbfc',
    '--theme-label-button-color': 'rgba(233, 244, 254, 0.5)',
  },
  buttonAttention: {
    '--theme-button-attention-background-color': '#ea4c5b',
    '--theme-button-attention-background-color-hover': '#ec5d6b',
    '--theme-button-attention-background-color-active': '#d34452',
    '--theme-button-attention-background-color-disabled': '#ea4c5b',
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
    '--theme-button-flat-background-color': 'rgba(233, 244, 254, 0.1)',
    '--theme-button-flat-background-color-hover': 'rgba(233, 244, 254, 0.05)',
    '--theme-button-flat-background-color-active': 'rgba(233, 244, 254, 0.12)',
    '--theme-button-flat-background-color-disabled': 'rgba(233, 244, 254, 0.1)',
    '--theme-button-flat-text-color-disabled': '#e9f4fe',
    '--theme-button-flat-text-color': '#e9f4fe',
    '--theme-button-flat-outline-color': 'rgba(102, 122, 138, 0.3)',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#536370',
    '--theme-button-primary-background-color-hover': '#63727e',
    '--theme-button-primary-background-color-active': '#4a5965',
    '--theme-button-primary-background-color-disabled': '#536370',
    '--theme-button-primary-text-color-disabled': '#e9f4fe',
    '--theme-button-primary-text-color': '#e9f4fe',
    '--theme-button-primary-outline-color': 'rgba(233, 244, 254, 0.2)',
  },
  connecting: {
    '--theme-connecting-background-color': '#263345',
    '--theme-connecting-background-color1': 'rgba(38, 51, 69, 1)',
    '--theme-connecting-background-color2': 'rgba(38, 51, 69, 0.91)',
    '--theme-connecting-background-color3': 'rgba(38, 51, 69, 0.31)',
    '--theme-connecting-background-color4': 'rgba(38, 51, 69, 0)',
    '--theme-connecting-background-color5': 'rgba(38, 51, 69, 0)',
    '--theme-connecting-background-color6': 'rgba(38, 51, 69, 0.31)',
    '--theme-connecting-background-color7': 'rgba(38, 51, 69, 0.91)',
    '--theme-connecting-background-color8': 'rgba(38, 51, 69, 1)',
    '--theme-connecting-text-color': '#fff',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': 'rgba(38, 51, 69, 0.96)',
    '--theme-data-migration-layer-box-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-data-migration-layer-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-data-migration-layer-button-background-color-hover': '#fafbfc',
    '--theme-data-migration-layer-text-color': '#fafbfc',
    '--theme-data-migration-layer-text-color-hover': '#263345',
    '--theme-data-migration-layer-text-opacity-color': '#fafbfc',
    '--theme-data-migration-button-border-color': '#ffffff',
    '--theme-data-migration-button-label-color': '#ffffff',
  },
  delegationSetupWizard: {
    '--theme-delegation-steps-activation-steps-indicator-color': '#e9f4fe',
    '--theme-delegation-steps-success-description-color':
      'rgba(233, 244, 254, 1)',
    '--theme-delegation-steps-success-tada-color': 'rgba(233, 244, 254, 1)',
    '--theme-delegation-steps-choose-stake-pool-checkmark-icon-color':
      '#536370',
    '--theme-delegation-steps-choose-stake-pool-delegated-pools-label-color':
      'rgba(233, 244, 254, 1)',
    '--theme-delegation-steps-choose-stake-pool-select-box-placeholder-color':
      '#9eabbb',
    '--theme-delegation-steps-choose-stake-pool-selected-checkmark-icon-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-selected-ticker-color':
      '#fafbfc',
    '--theme-delegation-steps-choose-stake-pool-ticker-color': '#536370',
    '--theme-delegation-steps-choose-stake-pool-thumb-background-color':
      '#263345',
    '--theme-delegation-steps-choose-stake-pool-thumb-border-color':
      'rgba(102, 122, 138, 0.2)',
    '--theme-delegation-steps-choose-stake-pool-title-color':
      'rgba(233, 244, 254, 1)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-arrow-color':
      'rgba(94, 96, 102, 0.9)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-background-color':
      'rgba(94, 96, 102, 0.9)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-box-shadow':
      '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-description-color':
      'rgba(250, 251, 252, 1)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-ticker-color':
      'rgba(250, 251, 252, 0.6)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-label-color':
      'rgba(250, 251, 252, 1)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-value-color':
      'rgba(250, 251, 252, 1)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-title-color':
      'rgba(250, 251, 252, 1)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-url-color': '#2ab467',
    '--theme-delegation-steps-choose-wallet-custom-value-color': '#e9f4fe',
    '--theme-delegation-steps-choose-wallet-description-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-delegation-steps-choose-wallet-description-highlighted-color':
      '#e9f4fe',
    '--theme-delegation-steps-choose-wallet-error-message-color': '#ea4c5b',
    '--theme-delegation-steps-choose-wallet-error-message-light-color':
      'rgba(234, 76, 91, 0.7)',
    '--theme-delegation-steps-choose-wallet-error-select-options-color':
      '#e9f4fe',
    '--theme-delegation-steps-choose-wallet-steps-indicator-color': '#e9f4fe',
    '--theme-delegation-steps-confirmation-description-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-delegation-steps-confirmation-fees-label-color': '#e9f4fe',
    '--theme-delegation-steps-confirmation-fees-amount-color': '#ea4c5b',
    '--theme-delegation-steps-confirmation-steps-indicator-color': '#e9f4fe',
    '--theme-delegation-steps-intro-content-text-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-delegation-steps-intro-divider-border-color': '#334152',
    '--theme-delegation-steps-intro-link-color': 'rgba(32, 181, 107, 1)',
    '--theme-delegation-steps-intro-list-label-color': '#e9f4fe',
    '--theme-delegation-steps-intro-list-numbers-color': '#e9f4fe',
    '--theme-delegation-steps-intro-list-optional-label-color':
      'rgba(233, 244, 254, 0.5)',
    '--theme-delegation-steps-not-available-description-text-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-delegation-steps-not-available-description-highlight-text-color':
      '#e9f4fe',
    '--theme-delegation-steps-not-available-icon-color': 'rgb(155, 166, 179)',
    '--theme-delegation-steps-not-available-subtitle-text-color':
      'rgba(233, 244, 254, 1)',
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
    '--theme-icon-nav-color': '#9ba6b3',
    '--theme-icon-nav-color-active': '#e9f4fe',
    '--theme-icon-sidebar-color': '#fafbfc',
    '--theme-icon-toggle-menu-color': '#fafbfc',
    '--theme-icon-node-update-notification-arrow-color': '#e9f4fe',
    '--theme-icon-add-wallet-from-sidebar-color': '#fafbfc',
    '--theme-icon-ada-summary-wallet-amount-symbol-color': '#e9f4fe',
    '--theme-icon-add-wallet-dialog-big-button-color':
      'rgba(233, 244, 254, 0.8)',
    '--theme-icon-back-button-color': '#e9f4fe',
    '--theme-icon-close-button-color': '#fafbfc',
    '--theme-icon-connecting-ada-api-logo-color': '#fafbfc',
    '--theme-icon-connecting-ada-logo-color': '#fafbfc',
    '--theme-icon-connecting-daedalus-logo-color': '#fafbfc',
    '--theme-icon-copy-address-color': '#8793a1',
    '--theme-icon-file-upload-color': '#8793a1',
    '--theme-icon-delegation-center-no-wallets': '#e9f4fe',
    '--theme-icon-syncing-ada-api-logo-color': '#e9f4fe',
    '--theme-icon-syncing-ada-logo-color': '#e9f4fe',
    '--theme-icon-syncing-daedalus-logo-color': '#e9f4fe',
    '--theme-icon-transactions-ada-symbol-color': '#e9f4fe',
    '--theme-icon-transaction-type-color': '#fafbfc',
  },
  input: {
    '--theme-input-background-color': '#263345',
    '--theme-input-border-color': 'rgba(233, 244, 254, 0.3)',
    '--theme-input-focus-border-color': 'rgba(233, 244, 254, 0.7)',
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-label-color': 'rgba(233, 244, 254, 1)',
    '--theme-input-placeholder-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-right-floating-text-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-input-text-color': '#e9f4fe',
  },
  link: {
    '--theme-link-main-color': 'rgb(32, 181, 107)',
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
    '--theme-loading-spinner-medium-color': '#e9f4fe',
  },
  manualUpdate: {
    '--theme-manual-update-overlay-background-color': 'rgba(38, 51, 69, 0.96)',
    '--theme-manual-update-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-manual-update-overlay-button-background-color-hover': '#fafbfc',
    '--theme-manual-update-overlay-button-icon-color': 'rgba(250, 251, 252, 1)',
    '--theme-manual-update-overlay-button-icon-color-hover':
      'rgba(38, 51, 69, 1)',
    '--theme-manual-update-overlay-button-label-color-hover':
      'rgba(38, 51, 69, 1)',
    '--theme-manual-update-overlay-button-text-color-hover': '#263345',
    '--theme-manual-update-overlay-button-border-color': '#fafbfc',
    '--theme-manual-update-overlay-text-color': 'rgba(250, 251, 252, 0.7)',
    '--theme-manual-update-overlay-text-highlight-color': '#fafbfc',
    '--theme-manual-update-overlay-title-text-color': '#fafbfc',
    '--theme-manual-update-overlay-button-label-color': '#fafbfc',
  },
  mnemonic: {
    '--theme-backup-mnemonic-background-color': 'rgba(233, 244, 254, 0.05)',
    '--theme-mnemonic-background-color': 'rgba(83, 99, 112, 0.3)',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navDropdown: {
    '--theme-nav-dropdown-item-text-color': '#e9f4fe',
    '--theme-nav-dropdown-item-background-color': '#314259',
    '--theme-nav-dropdown-item-background-color-hover':
      'rgba(233, 244, 254, 0.07)',
    '--theme-nav-dropdown-item-color-hover': '#e9f4fe',
  },
  navItem: {
    '--theme-nav-item-background-color': '#263345',
    '--theme-nav-item-background-color-hover': 'rgba(83, 99, 112, 0.1)',
    '--theme-nav-item-background-color-active': '#536370',
    '--theme-nav-item-text-color': '#9ba6b3',
    '--theme-nav-item-text-color-active': '#e9f4fe',
  },
  network: {
    '--theme-network-window-background-color': 'rgba(38, 51, 69, 0.96)',
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
  newsFeed: {
    '--theme-news-feed-background-color': '#263345',
    '--theme-news-feed-badge-background-color': '#ea4c5b',
    '--theme-news-feed-badge-text-color': '#e9f4fe',
    '--theme-news-feed-box-shadow-color': '-5px 0 20px 0 rgba(0, 0, 0, 0.25)',
    '--theme-news-feed-header-background-color': '#1b2430',
    '--theme-news-feed-header-title-color': '#fafbfc',
    '--theme-news-feed-icon-close-button-color': '#fff',
    '--theme-news-feed-icon-close-hover-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-icon-color': '#e9f4fe',
    '--theme-news-feed-icon-color-connecting-screen': '#e9f4fe',
    '--theme-news-feed-icon-color-syncing-screen': '#e9f4fe',
    '--theme-news-feed-icon-dot-background-color': '#ea4c5b',
    '--theme-news-feed-icon-toggle-hover-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-no-fetch-color': '#fafbfc',
  },
  newsItem: {
    '--theme-news-item-action-button-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-item-action-button-background-color-hover': '#fafbfc',
    '--theme-news-item-action-button-border-color': '#fafbfc',
    '--theme-news-item-action-button-color': '#fafbfc',
    '--theme-news-item-action-button-color-hover': '#263345',
    '--theme-news-item-alert-background-color': 'rgba(242, 162, 24, 0.5)',
    '--theme-news-item-announcement-background-color':
      'rgba(31, 193, 195, 0.2)',
    '--theme-news-item-badge-color': '#ea4c5b',
    '--theme-news-item-content-link-color': '#fafbfc',
    '--theme-news-item-info-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-item-title-color': '#fafbfc',
  },
  nodeUpdate: {
    '--theme-node-update-background-color': '#536370',
    '--theme-node-update-title-color': '#e9f4fe',
    '--theme-node-update-message-color': '#e9f4fe',
    '--theme-node-sync-info-message-background-color': '#536370',
    '--theme-node-sync-info-message-text-color': '#e9f4fe',
    '--theme-node-sync-icon-color': '#fff',
    '--theme-node-update-accept-button-background-color': '#263345',
    '--theme-node-update-accept-button-background-color-hover': '#34465e',
    '--theme-node-update-accept-button-background-color-active': '#18202b',
    '--theme-node-update-deny-button-background-color': 'rgba(38, 51, 69, 0.3)',
    '--theme-node-update-deny-button-background-color-hover':
      'rgba(52, 70, 94, 0.3)',
    '--theme-node-update-deny-button-background-color-active':
      'rgba(24, 32, 43, 0.3)',
    '--theme-node-update-deny-button-text-color': '#fafbfc',
    '--theme-node-update-button-text-color': '#fafbfc',
  },
  notification: {
    '--theme-notification-message-background-color': 'rgba(83, 99, 112, 0.95)',
    '--theme-notification-message-text-color': '#fafbfc',
    '--theme-notification-message-checkmark-icon-color': '#fafbfc',
    '--theme-notification-message-close-icon-color': '#fafbfc',
    '--theme-legacy-badge-background-color': '#ab1700',
    '--theme-legacy-notification-background-color': '#ab2712',
    '--theme-legacy-notification-learn-more-button-text-color': '#fafbfc',
    '--theme-legacy-notification-learn-more-button-background-color':
      'rgba(250, 251, 252, 0.2)',
    '--theme-legacy-notification-learn-more-button-background-color-hover':
      'rgba(250, 251, 252, 0.3)',
    '--theme-legacy-notification-learn-more-button-background-color-active':
      'rgba(250, 251, 252, 0.4)',
    '--theme-legacy-notification-move-button-text-color': '#ab1700',
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
      'rgb(32, 181, 107)',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(233, 244, 254, 0.05)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': 'rgba(233, 244, 254, 0.3)',
    '--theme-progress-bar-foreground-color': 'rgba(233, 244, 254, 0.7)',
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': '#fff',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#536370',
    '--theme-report-issue-button-background-color-hover': '#667a8a',
    '--theme-report-issue-button-background-color-active': '#495863',
    '--theme-report-issue-connecting-background-color':
      'rgba(250, 251, 252, 0.05)',
    '--theme-report-issue-connecting-text-color': '#e9f4fe',
    '--theme-report-issue-icon-color': '#e9f4fe',
    '--theme-report-issue-syncing-background-color':
      'rgba(250, 251, 252, 0.05)',
    '--theme-report-issue-syncing-text-color': '#e9f4fe',
    '--theme-report-issue-syncing-download-logs-text-color': '#e9f4fe',
    '--theme-report-issue-syncing-download-logs-text-color-connecting':
      '#e9f4fe',
  },
  rpAutocomplete: {
    '--rp-autocomplete-bg-color': '#263345',
    '--rp-autocomplete-border': '1px solid rgba(233, 244, 254, 0.3)',
    '--rp-autocomplete-border-color-opened': 'rgba(233, 244, 254, 0.7)',
    '--rp-autocomplete-input-text-color': 'rgba(233, 244, 254, 0.7)',
    '--rp-autocomplete-placeholder-color': 'rgba(233, 244, 254, 0.5)',
    '--rp-autocomplete-selected-word-box-bg-color': 'rgba(83, 99, 112, 0.5)',
    '--rp-autocomplete-selected-word-text-color': '#fafbfc',
    '--rp-autocomplete-selected-words-font-family':
      'NotoSans-Regular, NotoSansCJKjp-Regular',
  },
  rpBubble: {
    '--rp-bubble-bg-color': '#263345',
    '--rp-bubble-border-color': 'rgba(233, 244, 254, 0.7)',
    '--rp-bubble-border-radius': '2px',
    '--rp-bubble-arrow-bg-color': '#536370',
    '--rp-bubble-box-shadow':
      '0 4px 16px 0 rgba(0, 0, 0, 0.4), 0 0 8px 0 rgba(0, 0, 0, 0.2)',
  },
  rpButton: {
    '--rp-button-bg-color': '#536370',
    '--rp-button-bg-color-active': '#495863',
    '--rp-button-bg-color-disabled': 'rgba(83, 99, 112, 0.3)',
    '--rp-button-bg-color-hover': '#667a8a',
    '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--rp-button-height': '50px',
    '--rp-button-line-height': '20px',
    '--rp-button-padding': '0',
    '--rp-button-text-color': '#e9f4fe',
    '--rp-button-text-color-disabled': 'rgba(233, 244, 254, 0.5)',
    '--rp-button-text-transform': 'none',
    '--rp-button-width': '360px',
    '--rp-button-font-size': '14px',
  },
  rpCheckbox: {
    '--rp-checkbox-border': '2px solid #536370',
    '--rp-checkbox-border-color': 'rgba(102, 122, 138, 0.3)',
    '--rp-checkbox-border-color-disabled': 'rgba(83, 99, 112, 0.4)',
    '--rp-checkbox-check-bg-color': '#536370',
    '--rp-checkbox-check-icon-color': '#e9f4fe',
    '--rp-checkbox-label-text-color': '#e9f4fe',
    '--rp-checkbox-label-text-color-disabled': 'rgba(233, 244, 254, 0.3)',
    '--rp-checkbox-size': '22px',
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
    '--rp-formfield-label-text-color': '#e9f4fe',
    '--rp-formfield-label-text-color-disabled': '#e9f4fe',
    '--rp-formfield-error-text-color': '#ea4c5b',
    '--rp-formfield-error-text-opacity': '0.75',
  },
  rpInput: {
    '--rp-input-bg-color': '#263345',
    '--rp-input-bg-color-disabled': 'rgba(233, 244, 254, 0.05)',
    '--rp-input-border-color': 'rgba(233, 244, 254, 0.3)',
    '--rp-input-border-color-disabled': 'transparent',
    '--rp-input-border-color-errored': '#ea4c5b',
    '--rp-input-border-color-focus': 'rgba(233, 244, 254, 0.7)',
    '--rp-input-line-height': '22px',
    '--rp-input-padding': '12px 20px',
    '--rp-input-placeholder-color': 'rgba(233, 244, 254, 0.5)',
    '--rp-input-placeholder-color-disabled': 'rgba(233, 244, 254, 0.5)',
    '--rp-input-text-color': 'rgba(233, 244, 254, 1)',
    '--rp-input-text-color-disabled': 'rgba(233, 244, 254, 0.5)',
  },
  rpLink: {
    '--rp-link-color': 'rgb(32, 181, 107)',
  },
  rpModal: {
    '--rp-modal-bg-color': '#263345',
    '--rp-modal-max-height': '90%',
    '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
  },
  rpOptions: {
    '--rp-option-bg-color': '#263345',
    '--rp-option-bg-color-highlighted': 'rgba(233, 244, 254, 0.07)',
    '--rp-option-border-color': 'rgba(233, 244, 254, 0.7)',
    '--rp-option-checkmark-color': '#fff',
    '--rp-option-line-height': '22px',
    '--rp-option-text-color': '#e9f4fe',
    '--rp-options-border-color': 'rgba(233, 244, 254, 0.7)',
    '--rp-options-shadow': 'none',
  },
  rpRadio: {
    '--rp-radio-border': '2px solid #536370',
    '--rp-radio-border-color': '#536370',
    '--rp-radio-color': '#e9f4fe',
    '--rp-radio-label-margin': '0 0 0 10px',
    '--rp-radio-label-text-color': '#e9f4fe',
    '--rp-radio-size': '22px',
  },
  rpSelect: {
    '--rp-select-arrow-bg-color': 'rgba(233, 244, 254, 0.3)',
    '--rp-select-arrow-bg-color-open': 'rgba(233, 244, 254, 1)',
    '--rp-select-input-bg-color': 'transparent',
    '--rp-select-input-border-color': 'rgba(233, 244, 254, 0.3)',
    '--rp-select-input-border-color-focus': 'rgba(233, 244, 254, 0.7)',
    '--rp-select-input-text-color': 'rgba(233, 244, 254, 1)',
    '--rp-select-input-placeholder-color': 'rgba(233, 244, 254, 0.5)',
  },
  rpStepper: {
    '--rp-stepper-bullet-background-color-disabled': '#263345',
    '--rp-stepper-bullet-border-color': 'rgba(233, 244, 254, 0.1)',
    '--rpstepper-bullet-height': '12px',
    '--rpstepper-bullet-width': '12px',
    '--rp-stepper-label-color': 'rgba(233, 244, 254, 1)',
    '--rp-stepper-label-color-light': 'rgba(233, 244, 254, 0.3)',
    '--rp-stepper-main-color': 'rgba(233, 244, 254, 1)',
    '--rp-stepper-main-color-light': 'rgba(233, 244, 254, 0.1)',
    '--rpstepper-stepper-step-label-bottom-margin': '6px',
    '--rpstepper-steps-bar-color-disabled': 'rgba(233, 244, 254, 0.1)',
    '--rpstepper-steps-bar-top-position': '6px',
  },
  rpSwitch: {
    '--rp-switch-bg-color-off': '#536370',
    '--rp-switch-bg-color-on': '#536370',
    '--rp-switch-label-margin': '0 30px 0 0',
    '--rp-switch-label-opacity': '1',
    '--rp-switch-label-text-color': '#e9f4fe',
    '--rp-switch-label-width': '100%',
    '--rp-switch-normal-border-radius': '2px',
    '--rp-switch-opacity-off': '0.3',
    '--rp-switch-root-margin': '0 0 30px 0',
    '--rp-switch-tiny-border-radius': '1px',
    '--rp-switch-thumb-bg-color': '#fff',
    '--rp-switch-size': '22px',
  },
  rpTextArea: {
    '--rp-textarea-bg-color': '#263345',
    '--rp-textarea-bg-color-disabled': 'rgba(233, 244, 254, 0.05)',
    '--rp-textarea-border': '1px solid rgba(233, 244, 254, 0.3)',
    '--rp-textarea-border-color-disabled': 'rgba(233, 244, 254, 0.05)',
    '--rp-textarea-border-color-errored': '#ea4c5b',
    '--rp-textarea-border-color-focus': 'rgba(233, 244, 254, 0.7)',
    '--rp-textarea-border-radius': '2px',
    '--rp-textarea-line-height': '20px',
    '--rp-textarea-placeholder-color': 'rgba(233, 244, 254, 0.5)',
    '--rp-textarea-resize': 'none',
    '--rp-textarea-text-color': '#e9f4fe',
  },
  rpTooltip: {
    '--rp-tooltip-bg-color': '#e9f4fe',
    '--rp-tooltip-text-color': '#fafbfc',
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': 'rgba(255, 255, 255, 0.1)',
    '--theme-scrollbar-thumb-background-hover': 'rgba(255, 255, 255 0.3)',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  settings: {
    '--theme-settings-body-background-color': '#0b1926',
    '--theme-settings-delete-button-legacy-background-color':
      'rgba(83, 99, 112, 0.3)',
    '--theme-settings-delete-button-legacy-background-color-hover':
      'rgba(83, 99, 112, 1)',
    '--theme-settings-pane-background-color': '#263345',
    '--theme-settings-pane-border': 'none',
    '--theme-settings-menu-box-background-color': '#263345',
    '--theme-settings-menu-box-border': 'none',
    '--theme-settings-menu-item-text-color': '#cecfd1',
    '--theme-settings-menu-item-text-color-active': '#e9f4fe',
    '--theme-settings-menu-item-text-color-disabled': '#7a8691',
    '--theme-settings-menu-item-background-color-active': '#e9f4fe12',
    '--theme-settings-menu-item-left-border-color-active': '#536370',
    '--theme-settings-theme-select-title-color': '#cecfd1',
    '--theme-settings-theme-select-border-color': 'rgba(102, 122, 138, 0.3)',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#314259',
    '--theme-sidebar-category-background-color-hover': 'rgba(38, 51, 69, 0.5)',
    '--theme-sidebar-category-background-color-active': '#263345',
    '--theme-sidebar-category-text-color': '#e9f4fe',
    '--theme-sidebar-layout-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-sidebar-layout-topbar-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-sidebar-menu-background-color': '#263345',
    '--theme-sidebar-menu-item-background-color-hover': 'rgba(27, 36, 48, 0.5)',
    '--theme-sidebar-menu-item-background-color-active': '#1b2430',
    '--theme-sidebar-menu-item-wallet-name-color': '#e9f4fe',
    '--theme-sidebar-menu-item-wallet-info-color': '#bdc0c1',
    '--theme-sidebar-menu-add-button-background-color': '#1b2430',
    '--theme-sidebar-menu-add-button-background-color-active': '#1f2a38',
    '--theme-sidebar-menu-add-button-background-color-hover': '#1f2a38',
    '--theme-sidebar-menu-add-button-text-color': '#e9f4fe',
    '--theme-sidebar-wallets-scrollbar-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-sidebar-wallets-scrollbar-background-color-active':
      'rgba(255, 255, 255, 0.3)',
    '--theme-sidebar-wallets-scrollbar-background-color-hover':
      'rgba(255, 255, 255, 0.3)',
    '--theme-sidebar-category-networkInfo-background-color': '#536370',
    '--theme-sidebar-category-networkInfo-text-color': '#e9f4fe',
  },
  splash: {
    '--theme-splash-network-background-color': 'rgba(38, 51, 69, 0.96)',
    '--theme-splash-network-background-color1': 'rgba(18, 19, 38, 1)',
    '--theme-splash-network-background-color2': 'rgba(18, 19, 38, 0.91)',
    '--theme-splash-network-background-color3': 'rgba(18, 19, 38, 0.31)',
    '--theme-splash-network-background-color4': 'rgba(18, 19, 38, 0)',
    '--theme-splash-network-background-color5': 'rgba(18, 19, 38, 0)',
    '--theme-splash-network-background-color6': 'rgba(18, 19, 38, 0.31)',
    '--theme-splash-network-background-color7': 'rgba(18, 19, 38, 0.91)',
    '--theme-splash-network-background-color8': 'rgba(18, 19, 38, 1)',
    '--theme-splash-network-logo-fill-color': '#e9f4fe',
    '--theme-splash-network-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-splash-network-overlay-button-background-color-hover': '#e9f4fe',
    '--theme-splash-network-overlay-button-border-color': '#e9f4fe',
    '--theme-splash-network-overlay-button-text-color': '#e9f4fe',
    '--theme-splash-network-overlay-button-text-color-hover': '#263345',
    '--theme-splash-network-title-color': '#e9f4fe',
    '--theme-splash-network-subTitle1-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-splash-network-subTitle2-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-splash-network-description-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-splash-network-description-color': 'rgba(233, 244, 254, 0.7)',
    '--theme-splash-network-description-bold-color': '#e9f4fe',
    '--theme-splash-network-learn-more-color': '#e9f4fe',
  },
  stakePools: {
    '--theme-staking-stake-pool-background-color': '#263345',
    '--theme-staking-stake-pool-border-color': '#263345',
    '--theme-staking-stake-pool-glow-color': '#7cfeb54c',
    '--theme-staking-progress-label-light': '#fafbfc',
    '--theme-staking-stake-pool-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-selected-background-color': '#5da377',
    '--theme-staking-stake-pool-selected-checkmark-icon-color': '#fafbfc',
    '--theme-staking-stake-pool-selected-ticker-color': '#fafbfc',
    '--theme-staking-stake-pool-ticker-color': '#e9f4fe',
    '--theme-staking-stake-pool-tooltip-background-color':
      'rgba(83, 99, 112, 0.98)',
    '--theme-staking-stake-pool-tooltip-border-color': 'transparent',
    '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color':
      'rgba(250, 251, 252, 0.8)',
    '--theme-staking-stake-pool-tooltip-delegate-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
      '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color':
      '#fafbfc',
    '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color':
      '#263345',
    '--theme-staking-stake-pool-tooltip-delegate-button-text-color': '#e9f4fe',
    '--theme-staking-stake-pool-tooltip-link-color': '#2ab467',
    '--theme-staking-stake-pool-tooltip-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-staking-stake-pool-tooltip-table-param-color': '#e9f4fe',
    '--theme-staking-stake-pool-tooltip-table-title-color': '#e9f4fe',
    '--theme-staking-stake-pool-tooltip-text-color': '#e9f4fe',
    '--theme-staking-stake-pools-search-button-color': '#e9f4fe',
    '--theme-staking-stake-pools-search-icon-color': '#8793a1',
    '--theme-staking-stake-pools-search-clear-button-background-color':
      'rgba(68, 91, 124, 0.05)',
    '--theme-staking-stake-pools-title-color': '#e9f4fe',
    '--theme-staking-stake-pool-tooltip-neutral-background-color':
      'rgba(233, 244, 254, .1)',
    '--theme-staking-stake-pool-tooltip-neutral-text-color': '#e9f4fe',
  },
  staking: {
    '--theme-staking-content-background-color': '#263345',
    '--theme-staking-content-border-color': '#263345',
    '--theme-staking-font-color-accent': '#cecfd1',
    '--theme-staking-font-color-regular': '#cecfd1',
    '--theme-staking-font-color-light': 'rgba(233, 244, 254, 0.7)',
    '--theme-staking-font-color-lighter': 'rgba(233, 244, 254, 0.5)',
    '--theme-staking-table-head-background-color': '#536370',
    '--theme-staking-table-border-color': '#c6cdd6',
    '--theme-staking-link-color': 'rgba(32, 181, 107, 1)',
    '--theme-staking-link-color-light': 'rgba(32, 181, 107, 0.7)',
    '--theme-staking-progress-bar-background-color': 'rgba(233, 244, 254, 0.1)',
    '--theme-staking-progress-stripe-dark-1-background-color': '#3c4852',
    '--theme-staking-progress-stripe-dark-2-background-color': '#536370',
    '--theme-staking-table-body-highlighted-text-color': '#2ab467',
    '--theme-staking-donut-ring-completed-color': 'rgba(234, 76, 91, 0.2)',
    '--theme-staking-donut-ring-remaining-color': '#ea4c5b',
    '--theme-staking-wallet-row-border-color': 'rgba(102, 122, 138, 0.3)',
    '--theme-staking-dropdown-item-text-color-hover': '#e9f4fe',
    '--theme-staking-dropdown-item-background-color': '#314259',
    '--theme-staking-dropdown-item-background-color-hover':
      'rgba(233, 244, 254, 0.07)',
    '--theme-staking-delegation-center-gear-icon-fill-color':
      'rgba(233, 244, 254, 0.5)',
    '--theme-staking-delegation-center-gear-icon-fill-color-active': '#e9f4fe',
    '--theme-staking-delegation-center-no-wallets-instructions-color':
      '#e9f4fe',
    '--theme-staking-delegation-center-divider-border-color': '#334152',
    '--theme-staking-delegation-center-fees-amount-color': '#ea4c5b',
    '--theme-staking-info-learn-more-button-text-color': '#fafbfc',
    '--theme-staking-info-learn-more-icon-color': 'rgba(250, 251, 252, 1)',
    '--theme-staking-learn-more-button-color': '#fafbfc',
    '--theme-staking-learn-more-icon-color': 'rgba(250, 251, 252, 1)',
    '--theme-staking-countdown-widget-background-color':
      'rgba(233, 244, 254, 0.07)',
    '--theme-staking-countdown-widget-delimeter-background-color': '#e9f4fe',
    '--theme-staking-countdown-widget-field-label-color': '#e9f4fe',
    '--theme-staking-countdown-widget-field-value-color': '#e9f4fe',
    '--theme-staking-export-button-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-staking-export-button-color': '#fafbfc',
  },
  support: {
    '--theme-support-settings-item-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-support-settings-link-color': '#20b56b',
    '--theme-support-settings-text-color': '#e9f4fe',
  },
  syncing: {
    '--theme-syncing-background-color': '#0b1926',
    '--theme-syncing-background-color1': 'rgba(11, 25, 38, 1)',
    '--theme-syncing-background-color2': 'rgba(11, 25, 38, 0.91)',
    '--theme-syncing-background-color3': 'rgba(11, 25, 38, 0.31)',
    '--theme-syncing-background-color4': 'rgba(11, 25, 38, 0)',
    '--theme-syncing-background-color5': 'rgba(11, 25, 38, 0)',
    '--theme-syncing-background-color6': 'rgba(11, 25, 38, 0.31)',
    '--theme-syncing-background-color7': 'rgba(11, 25, 38, 0.91)',
    '--theme-syncing-background-color8': 'rgba(11, 25, 38, 1)',
    '--theme-syncing-text-color': '#e9f4fe',
  },
  syncingConnecting: {
    '--theme-syncing-connecting-title-color': '#fafbfc',
    '--theme-syncing-connecting-subTitle1-color': '#f69ab2',
    '--theme-syncing-connecting-subTitle2-color': '#fafbfc',
  },
  systemError: {
    '--theme-system-error-overlay-attention-icon-color': '#fafbfc',
    '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-system-error-overlay-support-link-icon-color': '#fafbfc',
    '--theme-system-error-overlay-text-color': '#fafbfc',
  },
  tabs: {
    '--theme-choice-tabs-text-color': '#e9f4fe',
    '--theme-choice-tabs-text-color-active': '#e9f4fe',
    '--theme-choice-tabs-bottom-border-color-active': '#e9f4fe',
  },
  testEnvironment: {
    '--theme-test-environment-label-background-color': '#ab1700',
    '--theme-test-environment-label-text-color': '#fafbfc',
  },
  topBar: {
    '--theme-topbar-background-color': '#263345',
    '--theme-topbar-layout-body-background-color': '#0b1926',
    '--theme-topbar-wallet-name-color': '#e9f4fe',
    '--theme-topbar-wallet-info-color': '#e9f4fe',
    '--theme-topbar-logo-color': 'rgb(250, 251, 252)',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#263345',
    '--theme-transactions-list-border-color': '#263345',
    '--theme-transactions-list-group-date-color': '#7a8691',
    '--theme-transactions-list-item-details-color': '#e9f4fe',
    '--theme-transactions-state-ok-background-color': '#274c2d',
    '--theme-transactions-state-pending-background-color':
      'rgba(233, 244, 254, 0.3)',
    '--theme-transactions-state-text-color': '#fafbfc',
    '--theme-transactions-search-background-color': '#fafbfc',
    '--theme-transactions-icon-type-expend-background-color': '#4f5f7a',
    '--theme-transactions-icon-type-income-background-color': '#3b6956',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-arrow-stroke-color': '#e9f4fe',
    '--theme-transactions-icon-type-pending-regular-background-color':
      'rgba(233, 244, 254, 0.3)',
    '--theme-transactions-icon-type-pending-warning-background-color':
      'rgba(236, 93, 107, 0.8)',
    '--theme-transactions-state-pending-warning-background-color': '#ec5d6b',
    '--theme-transactions-filter-button-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-transactions-filter-button-color': '#fafbfc',
    '--theme-transactions-date-picker-button-background-color':
      'rgba(233, 244, 254, 0.07)',
    '--theme-transactions-date-picker-button-background-color-active':
      'rgba(233, 244, 254, 0.14)',
    '--theme-transactions-filter-modal-bg-color': '#263345',
  },
  uploader: {
    '--theme-uploader-text-color': '#e9f4fe',
    '--theme-uploader-border-color': 'rgba(233, 244, 254, 0.3)',
  },
  utxo: {
    '--theme-utxo-background-color': 'rgba(233, 244, 254, 0.05)',
    '--theme-utxo-title-text-color': '#e9f4fe',
    '--theme-utxo-title-description-color': 'rgba(233, 244, 254, 0.7)',
    '--theme-utxo-bar-color': 'rgba(233, 244, 254, 0.5)',
    '--theme-utxo-label-text-color': 'rgba(233, 244, 254, 0.3)',
    '--theme-utxo-tick-text-color': 'rgba(233, 244, 254, 0.3)',
    '--theme-utxo-cursor-background-color': 'rgba(233, 244, 254, 0.1)',
    '--theme-utxo-tooltip-background-color': 'rgba(94, 96, 102, 0.9)',
    '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-utxo-tooltip-text-color': '#fafbfc',
  },
  recoveryPhrase: {
    '--theme-recovery-phrase-normal-background-color': 'rgba(83, 99, 112, .3)',
    '--theme-recovery-phrase-normal-border-color': 'transparent',
    '--theme-recovery-phrase-warning-background-color': 'rgba(83, 99, 112, .3)',
    '--theme-recovery-phrase-attention-background-color':
      'rgba(234, 76, 91, .2)',
  },
  walletRestoreDialog: {
    '--theme-wallet-restore-dialog-new-label-background-color':
      'rgba(233, 244, 254, 0.1)',
    '--theme-wallet-restore-dialog-new-label-color': '#e9f4fe',
    '--theme-wallet-restore-dialog-step-walletType-hardwareWalletDisclaimer-text-color':
      '#ea4c5b',
  },
};

const DARK_BLUE_THEME_PARAMS: CreateThemeParams = {
  config: DARK_BLUE_THEME_OUTPUT,
};

export default createTheme(DARK_BLUE_THEME_PARAMS);
