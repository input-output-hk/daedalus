import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';
//  ==== dark-cardano theme output for Daedalus and react-polymorph components === //
export const DARK_CARDANO_THEME_OUTPUT = {
  aboutWindow: {
    '--theme-about-window-background-color': '#36374df5',
    '--theme-about-window-header-bottom-border-color': '#ffffff4d',
    '--theme-about-window-daedalus-icon-color': '#ffffff',
    '--theme-about-window-cardano-icon-color': '#ffffff',
    '--theme-about-window-title-varsion-color': '#ffffff',
    '--theme-about-window-title-stroke-color': '#ffffff',
    '--theme-about-window-content-color': '#ffffff',
    '--theme-about-window-content-text-color': '#ffffff',
    '--theme-about-window-content-bottom-border-color': '#ffffff4d',
    '--theme-about-window-icon-close-button-color': '#fafbfc',
    '--theme-about-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.16)',
  },
  appUpdate: {
    '--theme-node-update-background-color': '#121326',
    '--theme-node-update-title-color': '#ffffff',
    '--theme-node-update-message-color': '#ffffff',
    '--theme-node-sync-info-message-background-color': '#56576b',
    '--theme-node-sync-info-message-text-color': '#ffffff',
    '--theme-node-sync-icon-color': '#fff',
    '--theme-node-update-accept-button-background-color': '#36374d',
    '--theme-node-update-accept-button-background-color-hover': '#5e5f71',
    '--theme-node-update-accept-button-background-color-active': '#262736',
    '--theme-node-update-deny-button-background-color': 'rgba(54, 55, 77, 0.3)',
    '--theme-node-update-deny-button-background-color-hover':
      'rgba(94, 95, 113, 0.3)',
    '--theme-node-update-deny-button-background-color-active':
      'rgba(38, 39, 54, 0.3)',
    '--theme-node-update-deny-button-text-color': '#ffffff',
    '--theme-node-update-button-text-color': '#ffffff',
  },
  appUpdateOverlay: {
    '--theme-app-update-overlay-background-color': '#36374df5',
    '--theme-app-update-overlay-content-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-app-update-overlay-button-background-color': '#313245',
    '--theme-app-update-overlay-button-background-color-hover': '#ffffff',
    '--theme-app-update-overlay-button-icon-color': '#ffffff',
    '--theme-app-update-overlay-button-icon-color-hover': '#36374d',
    '--theme-app-update-overlay-button-text-color-hover': '#36374d',
    '--theme-app-update-overlay-button-border-color': '#ffffff',
    '--theme-app-update-overlay-opacity-text-color': '#ffffffb3',
    '--theme-app-update-overlay-text-highlight-color': '#ffffff',
    '--theme-app-update-overlay-text-color': '#ffffff',
    '--theme-app-update-overlay-button-label-color': '#ffffff',
    '--theme-app-update-overlay-button-label-color-hover': '#000000',
    '--theme-app-update-overlay-manual-update-text-color': '#ea4c5b',
  },
  automaticUpdate: {
    '--theme-automatic-update-overlay-background-color': '#36374df5',
    '--theme-automatic-update-overlay-button-background-color': '#313245',
    '--theme-automatic-update-overlay-button-background-color-hover': '#ffffff',
    '--theme-automatic-update-overlay-button-icon-color': '#ffffff',
    '--theme-automatic-update-overlay-button-icon-color-hover': '#36374d',
    '--theme-automatic-update-overlay-button-text-color-hover': '#36374d',
    '--theme-automatic-update-overlay-button-border-color': '#ffffff',
    '--theme-automatic-update-overlay-text-color': '#ffffffb3',
    '--theme-automatic-update-overlay-text-highlight-color': '#ffffff',
    '--theme-automatic-update-overlay-title-text-color': '#ffffff',
    '--theme-automatic-update-overlay-button-label-color': '#ffffff',
    '--theme-automatic-update-overlay-button-label-color-hover': '#000000',
    '--theme-automatic-update-overlay-button-label-color-light':
      'rgba(255, 255, 255, 0.8)',
    '--theme-automatic-update-overlay-close-button-color': '#fff',
    '--theme-automatic-update-overlay-close-button-hover-background':
      'rgba(0, 0, 0, 0.1)',
  },
  backToTopButton: {
    '--theme-back-to-top-button-background-color': '#3f404f',
    '--theme-back-to-top-button-text-color': '#ffffff',
    '--theme-back-to-top-button-box-shadow-color': 'rgba(0, 0, 0, 0.36)',
  },
  body: {
    '--theme-main-body-background-color': '#121326',
    '--theme-main-body-messages-color': '#ffffff',
  },
  borderedBox: {
    '--theme-bordered-box-background-color': '#1e1f31',
    '--theme-bordered-box-border': '1px solid 1e1f31',
    '--theme-bordered-box-text-color': '#ffffff',
  },
  button: {
    '--theme-button-spinner-color': '#121326',
    '--theme-label-button-color': '#ffffff',
  },
  buttonAttention: {
    '--theme-button-attention-background-color': '#ea4c5b',
    '--theme-button-attention-background-color-hover': '#ee707c',
    '--theme-button-attention-background-color-active': '#a43540',
    '--theme-button-attention-background-color-disabled': '#ea4c5b',
    '--theme-button-attention-delete-text-color': '#121326',
    '--theme-button-attention-delete-text-color-disabled': '#121326',
    '--theme-button-attention-text-color': '#ffffff',
    '--theme-button-attention-text-color-disabled': '#ffffff',
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
    '--theme-button-flat-background-color': 'rgba(255, 255, 255, 0.1)',
    '--theme-button-flat-background-color-hover': 'rgba(255, 255, 255, 0.05)',
    '--theme-button-flat-background-color-active': 'rgba(255, 255, 255, 0.12)',
    '--theme-button-flat-background-color-disabled': 'rgba(255, 255, 255, 0.1)',
    '--theme-button-flat-text-color-disabled': '#ffffff',
    '--theme-button-flat-text-color': '#fff',
    '--theme-button-flat-outline-color': 'rgba(94, 96, 102, 0.3)',
  },
  buttonFlatInvert: {
    '--theme-button-flat-invert-background-color': 'rgba(255, 255, 255, 0.2)',
    '--theme-button-flat-invert-background-color-hover':
      'rgba(255, 255, 255, 0.3)',
    '--theme-button-flat-invert-background-color-active':
      'rgba(255, 255, 255, 0.4)',
    '--theme-button-flat-invert-text-color': '#fff',
  },
  buttonPrimary: {
    '--theme-button-primary-background-color': '#1fc1c3',
    '--theme-button-primary-background-color-hover': '#26e7eb',
    '--theme-button-primary-background-color-active': '#1cabad',
    '--theme-button-primary-background-color-disabled': '#1fc1c3',
    '--theme-button-primary-text-color-disabled': '#121326',
    '--theme-button-primary-text-color': '#121326',
    '--theme-button-primary-outline-color': 'rgba(255, 255, 255, 0.5)',
  },
  buttonPrimaryInvert: {
    '--theme-button-primary-invert-background-color': '#fff',
    '--theme-button-primary-invert-background-color-hover':
      'rgba(255, 255, 255, 0.9)',
    '--theme-button-primary-invert-background-color-active':
      'rgba(255, 255, 255, 0.8)',
    '--theme-button-primary-invert-text-color': '#414251',
  },
  connecting: {
    '--theme-connecting-background-color': '#36374d',
    '--theme-connecting-background-color1': 'rgba(54, 55, 77, 1)',
    '--theme-connecting-background-color2': 'rgba(54, 55, 77, 0.91)',
    '--theme-connecting-background-color3': 'rgba(54, 55, 77, 0.31)',
    '--theme-connecting-background-color4': 'rgba(54, 55, 77, 0)',
    '--theme-connecting-background-color5': 'rgba(54, 55, 77, 0)',
    '--theme-connecting-background-color6': 'rgba(54, 55, 77, 0.31)',
    '--theme-connecting-background-color7': 'rgba(54, 55, 77, 0.91)',
    '--theme-connecting-background-color8': 'rgba(54, 55, 77, 1)',
    '--theme-connecting-text-color': '#ffffff',
    '--theme-connecting-description-color': 'rgba(255, 255, 255, 0.7)',
  },
  dapps: {
    '--theme-dapp-transaction-request-separator': '#ffffff',
    '--theme-dapp-transaction-request-fees-text-color': '#ea4c5b',
    '--theme-dapp-transaction-request-toggle-button-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-dapp-transaction-request-code-background-color':
      'rgba(255, 255, 255, 0.05)',
  },
  dataMigration: {
    '--theme-data-migration-layer-background-color': '#36374d',
    '--theme-data-migration-layer-box-shadow-color': '#36374d',
    '--theme-data-migration-layer-button-background-color': '#36374d',
    '--theme-data-migration-layer-button-background-color-hover': '#ffffff',
    '--theme-data-migration-layer-button-border-color': '#ffffff',
    '--theme-data-migration-layer-button-label-color': '#ffffff',
    '--theme-data-migration-layer-text-color': '#ffffff',
    '--theme-data-migration-layer-text-color-hover': '#36374d',
    '--theme-data-migration-layer-text-opacity-color': '#000000',
  },
  delegationSetupWizard: {
    '--theme-delegation-steps-activation-steps-indicator-color': '#ffffff',
    '--theme-delegation-steps-success-description-color': '#ffffff',
    '--theme-delegation-steps-success-tada-color': 'rgba(255, 255, 255, 1)',
    '--theme-delegation-steps-choose-stake-pool-checkmark-icon-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-delegation-steps-choose-stake-pool-delegated-pools-label-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-ticker-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-delegation-steps-choose-stake-pool-select-box-placeholder-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-delegation-steps-choose-stake-pool-selected-checkmark-icon-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-selected-ticker-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-thumb-background-color':
      'rgba(42, 43, 60, 1)',
    '--theme-delegation-steps-choose-stake-pool-thumb-border-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-delegation-steps-choose-stake-pool-title-color': '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-tooltip-arrow-color':
      '#ffffffe6',
    '--theme-delegation-steps-choose-stake-pool-tooltip-background-color':
      '#ffffffe6',
    '--theme-delegation-steps-choose-stake-pool-tooltip-box-shadow':
      '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
    '--theme-delegation-steps-choose-stake-pool-tooltip-description-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-tooltip-ticker-color':
      '#ffffff99',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-label-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-tooltip-table-value-color':
      '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-tooltip-title-color': '#ffffff',
    '--theme-delegation-steps-choose-stake-pool-tooltip-url-color': '#85b6f9',
    '--theme-delegation-steps-choose-wallet-custom-value-color': '#ffffff',
    '--theme-delegation-steps-choose-wallet-description-color': '#ffffffcc',
    '--theme-delegation-steps-choose-wallet-description-highlighted-color':
      '#ffffff',
    '--theme-delegation-steps-choose-wallet-error-message-color': '#ea4c5b',
    '--theme-delegation-steps-choose-wallet-error-message-light-color':
      '#ea4c5bb3',
    '--theme-delegation-steps-choose-wallet-error-select-options-color':
      '#ffffff',
    '--theme-delegation-steps-choose-wallet-steps-indicator-color': '#ffffff',
    '--theme-delegation-steps-confirmation-steps-indicator-color': '#ffffff',
    '--theme-delegation-steps-confirmation-description-color': '#ffffffcc',
    '--theme-delegation-steps-confirmation-fees-label-color': '#ffffff',
    '--theme-delegation-steps-confirmation-fees-amount-color': '#ea4c5b',
    '--theme-delegation-steps-intro-content-text-color': '#ffffffcc',
    '--theme-delegation-steps-intro-divider-border-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-delegation-steps-intro-link-color': '#36374d',
    '--theme-delegation-steps-intro-list-label-color': '#ffffff',
    '--theme-delegation-steps-intro-list-numbers-color': '#ffffff',
    '--theme-delegation-steps-intro-list-optional-label-color': '#ffffff80',
    '--theme-delegation-steps-not-available-description-text-color':
      '#ffffffcc',
    '--theme-delegation-steps-not-available-description-highlight-text-color':
      '#ffffff',
    '--theme-delegation-steps-not-available-icon-color': '#ffffff',
    '--theme-delegation-steps-not-available-subtitle-text-color': '#ffffff',
  },
  dialog: {
    '--theme-dialog-choice-tabs-text-color': '#ffffff',
    '--theme-dialog-choice-tabs-text-color-active': '#ffffff',
    '--theme-dialog-choice-tabs-bottom-border-color-active':
      'rgba(255, 255, 255, 0.4)',
    '--theme-dialog-big-button-background-color': 'rgba(255, 255, 255, 0.05)',
    '--theme-dialog-big-button-border-color': 'none',
    '--theme-dialog-big-button-label-color': '#ffffff',
    '--theme-dialog-big-button-description-color': 'rgba(255, 255, 255, 0.6)',
    '--theme-dialog-set-wallet-password-background-color':
      'rgba(42, 43, 60, 0.96)',
    '--theme-dialog-set-wallet-password-box-shadow': 'rgba(0, 0, 0, 0.25)',
    '--theme-dialog-set-wallet-password-message-color': '#fff',
    '--theme-dialog-set-wallet-password-title-color': '#fafbfc',
    '--theme-dialog-set-wallet-password-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-dialog-set-wallet-password-button-background-color-hover': '#fff',
    '--theme-dialog-set-wallet-password-button-border-color': '#fafbfc',
    '--theme-dialog-set-wallet-password-button-color': '#fff',
    '--theme-dialog-set-wallet-password-button-color-hover': '#2a2b3c',
    '--theme-dialog-title-color': '#ffffff',
    '--theme-dialog-text-color': '#ffffff',
    '--theme-dialog-border-color': 'rgba(255, 255, 255, 0.2)',
    '--theme-dialog-fullsize-background-color': '#2A2B3C',
    '--theme-dialog-fullsize-background-color-opaque': '#353545',
    '--theme-dialog-fullsize-field-background-color':
      'rgba(255, 255, 255, 0.07)',
    '--theme-dialog-fullsize-button-background-color-hover': '#313245',
    '--theme-dialog-fullsize-button-background-color': '#000000',
    '--theme-dialog-fullsize-button-icon-color': '#000000',
    '--theme-dialog-fullsize-button-icon-color-hover': '#36374d',
    '--theme-dialog-fullsize-button-text-color-hover': '#36374d',
    '--theme-dialog-fullsize-button-border-color': '#000000',
    '--theme-dialog-fullsize-text-color': 'rgba(255, 255, 255, 0.7)',
    '--theme-dialog-fullsize-text-highlight-color': '#ffffff',
    '--theme-dialog-fullsize-title-text-color': '#ffffff',
    '--theme-dialog-fullsize-button-label-color': '#000000',
    '--theme-dialog-fullsize-button-label-color-hover': '#000000',
  },
  errors: {
    '--theme-color-error': '#ea4c5b',
    '--theme-background-color-error': 'rgba(234, 76, 91, .1)',
    '--theme-background-color-dark-error': 'rgba(234, 76, 91, .3)',
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
  hardwareWallet: {
    '--theme-hardware-wallet-status-background-color': '#1e1f31',
    '--theme-hardware-wallet-status-border-color': 'rgba(255, 255, 255, 0.3)',
    '--theme-hardware-wallet-status-color': '#ffffff',
    '--theme-hardware-wallet-status-ready-color': '#2dc06c',
    '--theme-hardware-wallet-title-color': '#ffffff',
    '--theme-hardware-wallet-message-color': '#ffffff',
    '--theme-hardware-wallet-status-passphrase-label-color':
      'rgba(255, 255, 255, 0.5)',
  },
  icon: {
    '--theme-icon-nav-color': 'rgba(255, 255, 255, 0.6)',
    '--theme-icon-nav-color-active': 'rgba(255, 255, 255, 1)',
    '--theme-icon-sidebar-color': '#ffffff',
    '--theme-icon-toggle-menu-color': '#ffffff',
    '--theme-icon-node-update-notification-arrow-color': '#ffffff',
    '--theme-icon-add-wallet-from-sidebar-color': '#ffffff',
    '--theme-icon-ada-summary-wallet-amount-symbol-color': '#ffffff',
    '--theme-icon-add-wallet-dialog-big-button-color':
      'rgba(255, 255, 255, 0.5)',
    '--theme-icon-back-button-color': '#ffffff',
    '--theme-icon-close-button-color': '#ffffff',
    '--theme-icon-connecting-ada-api-logo-color': '#ffffff',
    '--theme-icon-connecting-ada-logo-color': '#ffffff',
    '--theme-icon-connecting-daedalus-logo-color': '#ffffff',
    '--theme-icon-copy-address-color': '#ffffff',
    '--theme-icon-delegation-center-no-wallets': '#ffffff',
    '--theme-icon-file-upload-color': '#ffffff',
    '--theme-icon-syncing-ada-api-logo-color': '#ffffff',
    '--theme-icon-syncing-ada-logo-color': '#ffffff',
    '--theme-icon-syncing-daedalus-logo-color': '#ffffff',
    '--theme-icon-transactions-ada-symbol-color': '#ffffff',
    '--theme-icon-transaction-type-color': '#121326',
  },
  input: {
    '--theme-input-background-color': '#1e1f31',
    '--theme-input-border-color': 'rgba(255, 255, 255, 0.3)',
    '--theme-input-focus-border-color': 'rgba(255, 255, 255, 0.7)',
    '--theme-input-hint-font': 'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--theme-input-label-color': '#ffffff',
    '--theme-input-placeholder-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-input-remove-color-light': '#ea4c5b',
    '--theme-input-right-floating-text-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-input-right-floating-text-success-color': 'rgba(255, 255, 255, 1)',
    '--theme-input-text-color': '#ffffff',
  },
  link: {
    '--theme-link-main-color': '#1fc1c3',
  },
  loading: {
    '--theme-loading-background-color': '#121326',
    '--theme-loading-no-disk-space-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-loading-no-disk-space-text-color': '#ffffff',
    '--theme-loading-no-disk-space-attention-icon-color': '#ffffff',
    '--theme-loading-status-icons-on-color': '#2dc06c',
    '--theme-loading-status-icons-off-color': '#ea4c5b',
    '--theme-loading-status-icons-unloaded-loading-color': '#ffffff',
    '--theme-loading-status-icons-unloaded-syncing-color': '#ffffff',
    '--theme-loading-status-icons-tooltip-color': 'var(--rp-pop-over-bg-color)',
    '--theme-loading-spinner-color': '#ffffff',
    '--theme-loading-spinner-medium-color': '#ffffff',
  },
  mnemonic: {
    '--theme-backup-mnemonic-background-color': '#414251',
    '--theme-mnemonic-background-color': 'rgba(255, 255, 255, 0.07)',
  },
  modal: {
    '--theme-modal-overlay-background-color': 'rgba(0, 0, 0, 0.4)',
  },
  navDropdown: {
    '--theme-nav-dropdown-item-text-color': '#ffffff',
    '--theme-nav-dropdown-item-background-color': '#3f404f',
    '--theme-nav-dropdown-item-background-color-hover':
      'rgba(255, 255, 255, 0.07)',
    '--theme-nav-dropdown-item-color-hover': '#ffffff',
  },
  navItem: {
    '--theme-nav-item-background-color': '#2a2b3c',
    '--theme-nav-item-background-color-active': 'rgba(255, 255, 255, 0.1)',
    '--theme-nav-item-background-color-hover': '#afafb833',
    '--theme-nav-item-text-color': 'rgba(255, 255, 255, 0.6)',
    '--theme-nav-item-text-color-active': '#ffffff',
  },
  network: {
    '--theme-network-window-background-color': '#36374df5',
    '--theme-network-window-text-color': '#ffffff',
    '--theme-network-window-icon-close-hover-background': 'rgba(0, 0, 0, 0.16)',
    '--theme-network-window-red-color': '#f06e05',
    '--theme-network-window-green-color': '#05f079',
    '--theme-network-window-white-color': '#ffffff',
    '--theme-network-window-transparent-color': 'transparent',
    '--theme-network-window-border-color': '#ffffffb3',
    '--theme-network-window-button-text-color': '#ffffff',
    '--theme-network-window-button-background-color': '#afafb866',
    '--theme-network-window-button-background-color-hover': '#afafb899',
    '--theme-network-window-button-background-color-active': '#afafb8cc',
  },
  newsFeed: {
    '--theme-news-feed-background-color': '#2a2b3c',
    '--theme-news-feed-badge-background-color': '#ea4c5b',
    '--theme-news-feed-badge-text-color': '#ffffff',
    '--theme-news-feed-box-shadow-color': '-5px 0 20px 0 rgba(0, 0, 0, 0.25)',
    '--theme-news-feed-header-background-color': '#20212e',
    '--theme-news-feed-header-box-shadow-color':
      '0 10px 10px -5px rgba(0, 0, 0, 0.25)',
    '--theme-news-feed-header-title-color': '#fafbfc',
    '--theme-news-feed-icon-close-button-color': '#ffffff',
    '--theme-news-feed-icon-close-hover-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-icon-color': '#ffffff',
    '--theme-news-feed-icon-color-connecting-screen': '#ffffff',
    '--theme-news-feed-icon-color-syncing-screen': '#ffffff',
    '--theme-news-feed-icon-green-dot-background-color': '#2dc06c',
    '--theme-news-feed-icon-red-dot-background-color': '#ea4c5b',
    '--theme-news-feed-icon-toggle-hover-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-no-fetch-color': '#fafbfc',
    '--theme-news-feed-incident-overlay-background-color':
      'rgba(42, 43, 60, 0.98)',
    '--theme-news-feed-incident-overlay-text-color': '#ffffff',
    '--theme-news-feed-incident-overlay-button-color': '#ffffff',
    '--theme-news-feed-incident-overlay-scrollbar-thumb-background':
      'rgba(255, 255, 255, 0.3)',
    '--theme-news-feed-incident-overlay-scrollbar-thumb-background-hover':
      'rgba(255, 255, 255, 0.5)',
    '--theme-news-feed-incident-overlay-content-list-color':
      'rgba(255, 255, 255, 0.7)',
    '--theme-news-feed-incident-overlay-content-background':
      'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-incident-overlay-button-background':
      'rgba(0, 0, 0, 0.1)',
    '--theme-news-feed-incident-overlay-button-color-hover':
      'rgba(42, 43, 60, 0.98)',
    '--theme-news-feed-incident-grey-overlay-background-color':
      'rgba(68, 68, 68, 0.98)',
    '--theme-news-feed-incident-grey-overlay-text-color': '#ffffff',
    '--theme-news-feed-incident-grey-overlay-button-color': '#ffffff',
    '--theme-news-feed-incident-grey-overlay-scrollbar-thumb-background':
      'rgba(255, 255, 255, 0.3)',
    '--theme-news-feed-incident-grey-overlay-scrollbar-thumb-background-hover':
      'rgba(255, 255, 255, 0.5)',
    '--theme-news-feed-incident-grey-overlay-content-list-color':
      'rgba(255, 255, 255, 0.7)',
    '--theme-news-feed-incident-red-overlay-background-color':
      'rgba(171, 23, 0, 0.98)',
    '--theme-news-feed-incident-red-overlay-text-color': '#ffffff',
    '--theme-news-feed-incident-red-overlay-button-color': '#ffffff',
    '--theme-news-feed-incident-red-overlay-scrollbar-thumb-background':
      'rgba(255, 255, 255, 0.3)',
    '--theme-news-feed-incident-red-overlay-scrollbar-thumb-background-hover':
      'rgba(255, 255, 255, 0.5)',
    '--theme-news-feed-incident-red-overlay-content-list-color':
      'rgba(255, 255, 255, 0.7)',
  },
  newsItem: {
    '--theme-news-item-action-button-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-item-action-button-background-color-hover': '#ffffff',
    '--theme-news-item-action-button-border-color': '#ffffff',
    '--theme-news-item-action-button-color': '#ffffff',
    '--theme-news-item-action-button-color-hover': '#2a2b3c',
    '--theme-news-item-alert-background-color': 'rgba(242, 162, 24, 0.5)',
    '--theme-news-item-announcement-background-color':
      'rgba(31, 193, 195, 0.2)',
    '--theme-news-item-badge-green-color': '#2DC06C',
    '--theme-news-item-badge-red-color': '#ea4c5b',
    '--theme-news-item-content-link-color': '#ffffff',
    '--theme-news-item-info-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-news-item-title-color': '#ffffff',
  },
  newsUpdateOverlay: {
    '--theme-news-overlay-update-background-color': 'rgba(36, 62, 98, 0.96)',
    '--theme-news-overlay-update-text-color': '#fafbfc',
    '--theme-news-overlay-update-subtitle-text-color':
      'rgba(250, 251, 252, 0.7)',
    '--theme-news-overlay-update-content-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-news-overlay-update-content-scroll-background-color':
      'rgba(255, 255, 255, 0.3)',
    '--theme-news-overlay-update-content-scroll-hover-background-color':
      'rgba(255, 255, 255, 0.5)',
    '--theme-news-overlay-update-button-background-color': '#36374d',
    '--theme-news-overlay-update-button-text-color': '#ffffff',
    '--theme-news-overlay-update-button-hover-text-color': '#273d5b',
    '--theme-news-overlay-update-button-background-color-hover': '#ffffff',
    '--theme-news-overlay-update-button-border-color': '#ffffff',
  },
  notification: {
    '--theme-notification-message-background-color': '#36374df2',
    '--theme-notification-message-text-color': '#ffffff',
    '--theme-legacy-badge-background-color': '#ab1700',
    '--theme-legacy-notification-background-color': '#ab2712',
    '--theme-legacy-notification-learn-more-button-text-color': '#ffffff',
    '--theme-legacy-notification-learn-more-button-background-color':
      '#12132633',
    '--theme-legacy-notification-learn-more-button-background-color-hover':
      '#1213264d',
    '--theme-legacy-notification-learn-more-button-background-color-active':
      '#12132666',
    '--theme-legacy-notification-move-button-text-color': '#ab1700',
    '--theme-legacy-notification-move-button-background-color': '#fafbfc',
    '--theme-legacy-notification-move-button-background-color-hover':
      'rgba(250, 251, 252, 0.9)',
    '--theme-legacy-notification-move-button-background-color-active':
      'rgb(250, 251, 252, 0.8)',
    '--theme-legacy-notification-title-color': '#ffffff',
    '--theme-legacy-notification-description-color': '#ffffff',
    '--theme-notification-message-checkmark-icon-color': '#fafbfc',
    '--theme-notification-message-close-icon-color': '#fafbfc',
  },
  paperWallet: {
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-color':
      '#1fc1c3',
    '--theme-paper-wallet-create-certificate-dialog-explorer-link-background-color':
      'rgba(255, 255, 255, 0.07)',
  },
  progressBar: {
    '--theme-progress-bar-background-color': 'rgba(255, 255, 255, 0.3)',
    '--theme-progress-bar-foreground-color': 'rgba(255, 255, 255, 0.7)',
    '--theme-progress-bar-large-progress-stripe1': '#e0e5eb',
    '--theme-progress-bar-large-progress-stripe2': '#fafbfc',
    '--theme-progress-bar-large-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-progress-bar-large-progress-dark-stripe1': '#e0e5eb',
    '--theme-progress-bar-large-progress-dark-stripe2': '#fafbfc',
    '--theme-progress-bar-large-progress-light-stripe1': '#e0e5eb',
    '--theme-progress-bar-large-progress-light-stripe2': '#fafbfc',
    '--theme-progress-bar-large-progress-light-stripe-1': '#0da2a4',
    '--theme-progress-bar-large-progress-light-stripe-2-background-color':
      '#1fc1c3',
  },
  receiveQRCode: {
    '--theme-receive-qr-code-background-color': '#fff',
    '--theme-receive-qr-code-foreground-color': '#000',
  },
  recoveryPhrase: {
    '--theme-recovery-phrase-normal-background-color':
      'rgba(255, 255, 255, .1)',
    '--theme-recovery-phrase-normal-border-color': 'transparent',
    '--theme-recovery-phrase-warning-background-color':
      'rgba(255, 255, 255, .1)',
    '--theme-recovery-phrase-attention-background-color':
      'rgba(234, 76, 91, .2)',
    '--theme-recovery-phrase-paper-wallet-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-recovery-phrase-paper-wallet-question-background-color':
      'rgba(255, 255, 255, 0.3)',
    '--theme-recovery-phrase-paper-wallet-question-text-color': '#343546',
    '--theme-recovery-phrase-paper-wallet-text-color':
      'rgba(255, 255, 255, 0.8)',
  },
  reportIssue: {
    '--theme-report-issue-button-background-color': '#1fc1c3',
    '--theme-report-issue-button-background-color-hover': '#26e7eb',
    '--theme-report-issue-button-background-color-active': '#1cabad',
    '--theme-report-issue-connecting-background-color':
      'rgba(255, 255, 255, 0.05)',
    '--theme-report-issue-connecting-text-color': '#ffffff',
    '--theme-report-issue-icon-color': '#121326',
  },
  rpAutocomplete: {
    '--rp-autocomplete-bg-color': '#1e1f31',
    '--rp-autocomplete-border': '1px solid rgba(255, 255, 255, 0.3)',
    '--rp-autocomplete-border-color-opened': 'rgba(255, 255, 255, 0.7)',
    '--rp-autocomplete-input-text-color': 'rgba(255, 255, 255, 0.7)',
    '--rp-autocomplete-placeholder-color': 'rgba(255, 255, 255, 0.5)',
    '--rp-autocomplete-selected-word-box-bg-color': '#1fc1c3',
    '--rp-autocomplete-selected-word-dialog-box-bg-color': '#1fc1c3',
    '--rp-autocomplete-selected-word-dialog-text-color': '#121326',
    '--rp-autocomplete-selected-word-text-color': '#121326',
    '--rp-autocomplete-selected-words-font-family':
      'NotoSans-Regular, NotoSansCJKjp-Regular',
    '--rp-autocomplete-required-words-color': 'rgba(255, 255, 255, 0.5)',
    '--rp-autocomplete-required-words-invert-color': 'rgba(255, 255, 255, 0.5)',
    '--rp-autocomplete-required-words-offset-top': '2px',
  },
  rpBubble: {
    '--rp-bubble-bg-color': '#1e1f31',
    '--rp-bubble-border-color': 'rgba(255, 255, 255, 0.7)',
    '--rp-bubble-border-radius': '5px',
    '--rp-bubble-arrow-bg-color': '#343646',
    '--rp-bubble-box-shadow':
      '0 4px 16px 0 rgba(0, 0, 0, 0.4), 0 0 8px 0 rgba(0, 0, 0, 0.2)',
  },
  rpButton: {
    '--rp-button-bg-color': '#1fc1c3',
    '--rp-button-bg-color-active': '#1cabad',
    '--rp-button-bg-color-disabled': 'rgba(31, 193, 195, 0.3)',
    '--rp-button-bg-color-hover': '#26e7eb',
    '--rp-button-font-family': 'NotoSans-Medium, NotoSansCJKjp-Medium',
    '--rp-button-font-size': '14px',
    '--rp-button-height': '50px',
    '--rp-button-line-height': '20px',
    '--rp-button-padding': '0',
    '--rp-button-text-color': 'rgba(18, 19, 38, 1)',
    '--rp-button-text-color-disabled': 'rgba(18, 19, 38, 0.5)',
    '--rp-button-text-transform': 'none',
    '--rp-button-width': '360px',
  },
  rpCheckbox: {
    '--rp-checkbox-border': '2px solid #1fc1c3',
    '--rp-checkbox-border-color': 'rgba(255, 255, 255, 0.2)',
    '--rp-checkbox-border-color-disabled': '#1fc1c366',
    '--rp-checkbox-check-bg-color': '#1fc1c3',
    '--rp-checkbox-check-icon-color': '#121326',
    '--rp-checkbox-label-text-color': '#ffffff',
    '--rp-checkbox-label-text-color-disabled': '#ffffff4d',
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
    '--rp-formfield-label-text-color': '#ffffff',
    '--rp-formfield-label-text-color-disabled': '#ffffff',
    '--rp-formfield-error-text-color': '#ea4c5b',
    '--rp-formfield-error-text-opacity': '0.75',
  },
  rpInput: {
    '--rp-input-bg-color': '#1e1f31',
    '--rp-input-bg-color-disabled': 'rgba(255, 255, 255, 0.05)',
    '--rp-input-border-color': 'rgba(255, 255, 255, 0.3)',
    '--rp-input-border-color-disabled': 'transparent',
    '--rp-input-border-color-errored': '#ea4c5b',
    '--rp-input-border-color-focus': 'rgba(255, 255, 255, 0.7)',
    '--rp-input-line-height': '22px',
    '--rp-input-padding': '12px 20px',
    '--rp-input-placeholder-color': 'rgba(255, 255, 255, 0.5)',
    '--rp-input-placeholder-color-disabled': 'rgba(255, 255, 255, 0.5)',
    '--rp-input-text-color': '#ffffff',
    '--rp-input-text-color-disabled': 'rgba(255, 255, 255, 0.5)',
  },
  rpLink: {
    '--rp-link-color': '#1fc1c3',
  },
  rpModal: {
    '--rp-modal-bg-color': '#1e1f31',
    '--rp-modal-max-height': '90%',
    '--rp-modal-overlay-bg-color': 'rgba(0, 0, 0, 0.4)',
  },
  rpOptions: {
    '--rp-option-bg-color': '#1e1f31',
    '--rp-option-bg-color-highlighted': 'rgba(255, 255, 255, 0.07)',
    '--rp-option-border-color': 'rgba(255, 255, 255, 0.7)',
    '--rp-option-checkmark-color': 'rgba(255, 255, 255, 0.7)',
    '--rp-option-line-height': '22px',
    '--rp-option-text-color': '#ffffff',
    '--rp-options-border-color': 'rgba(255, 255, 255, 0.7)',
    '--rp-options-shadow': 'none',
    '--rp-option-search-highlight-background-color': 'rgba(242, 162, 24, 0.3)',
  },
  rpPasswordInput: {
    '--rp-password-input-tooltip-border-radius': '5px',
    '--rp-password-input-error-score-color': '#ea4c5b',
    '--rp-password-input-error-bg-color': 'rgba(234, 76, 91, 0.4)',
    '--rp-password-input-warning-score-color': '#f2a218',
    '--rp-password-input-warning-bg-color': 'rgba(242, 162, 24, 0.4)',
    '--rp-password-input-success-score-color': '#2dc06c',
    '--rp-password-input-success-bg-color': 'rgba(45, 192, 108, 0.4)',
    '--rp-password-input-tooltip-font-family': 'var(--rp-theme-font-medium)',
  },
  rpPopOver: {
    '--rp-pop-over-bg-color': 'rgba(63,64,79,0.9)',
    '--rp-pop-over-text-color': 'white',
  },
  rpRadio: {
    '--rp-radio-border': '2px solid #1fc1c3',
    '--rp-radio-border-color': '#1fc1c3',
    '--rp-radio-color': '#121326',
    '--rp-radio-label-margin': '0 0 0 10px',
    '--rp-radio-label-text-color': '#ffffff',
    '--rp-radio-size': '22px',
  },
  rpScrollBar: {
    '--rp-scrollbar-thumb-bg-color:': 'rgba(255, 255, 255, 0.3)',
    '--rp-scrollbar-thumb-bg-color-active': 'rgba(255, 255, 255, 0.5)',
    '--rp-scrollbar-thumb-bg-color-hover': 'rgba(255, 255, 255, 0.5)',
  },
  rpSelect: {
    '--rp-select-arrow-bg-color': 'rgba(255, 255, 255, 0.3)',
    '--rp-select-arrow-bg-color-open': 'rgba(255, 255, 255, 0.7)',
    '--rp-select-input-bg-color': 'transparent',
    '--rp-select-input-border-color': 'rgba(255, 255, 255, 0.3)',
    '--rp-select-input-border-color-focus': 'rgba(255, 255, 255, 0.7)',
    '--rp-select-input-text-color': '#ffffff',
    '--rp-select-input-placeholder-color': 'rgba(255, 255, 255, 0.5)',
  },
  rpStepper: {
    '--rp-stepper-bullet-background-color-disabled': '#1e1f31',
    '--rp-stepper-bullet-border-color': 'rgba(255, 255, 255, 0.1)',
    '--rpstepper-bullet-height': '12px',
    '--rpstepper-bullet-width': '12px',
    '--rp-stepper-label-color': 'rgba(255, 255, 255, 1)',
    '--rp-stepper-label-color-light': 'rgba(255, 255, 255, 0.3)',
    '--rp-stepper-main-color': '#1fc1c3',
    '--rp-stepper-main-color-light': '#1fc1c31a',
    '--rpstepper-stepper-step-label-bottom-margin': '6px',
    '--rpstepper-steps-bar-color-disabled': 'rgba(255, 255, 255, 0.1)',
    '--rpstepper-steps-bar-top-position': '6px',
  },
  rpSwitch: {
    '--rp-switch-bg-color-off': 'rgba(31, 193, 195, 0.3)',
    '--rp-switch-bg-color-on': '#1fc1c3',
    '--rp-switch-label-margin': '0 30px 0 0',
    '--rp-switch-label-opacity': '1',
    '--rp-switch-label-text-color': '#ffffff',
    '--rp-switch-label-width': '100%',
    '--rp-switch-normal-border-radius': '2px',
    '--rp-switch-opacity-off': '1',
    '--rp-switch-root-margin': '0 0 30px 0',
    '--rp-switch-tiny-border-radius': '1px',
    '--rp-switch-thumb-bg-color': '#ffffff',
    '--rp-switch-size': '22px',
  },
  rpTextArea: {
    '--rp-textarea-bg-color': '#121326',
    '--rp-textarea-bg-color-disabled': 'rgba(255, 255, 255, 0.05)',
    '--rp-textarea-border': '1px solid rgba(255, 255, 255, 0.3)',
    '--rp-textarea-border-color-disabled': 'rgba(255, 255, 255, 0.05)',
    '--rp-textarea-border-color-errored': '#ea4c5b',
    '--rp-textarea-border-color-focus': 'rgba(255, 255, 255, 0.7)',
    '--rp-textarea-border-radius': '2px',
    '--rp-textarea-line-height': '20px',
    '--rp-textarea-placeholder-color': 'rgba(255, 255, 255, 0.5)',
    '--rp-textarea-resize': 'none',
    '--rp-textarea-text-color': '#ffffff',
  },
  rpTooltip: {
    '--rp-tooltip-bg-color': '#56576b',
    '--rp-tooltip-text-color': '#ffffff',
  },
  scrollbar: {
    '--theme-scrollbar-thumb-background': 'rgba(255, 255, 255, 0.3)',
  },
  sendConfirmation: {
    '--theme-send-confirmation-dialog-send-values-color': '#ea4c5b',
  },
  settings: {
    '--theme-settings-body-background-color': '#121326',
    '--theme-settings-delete-button-legacy-background-color': '#414251',
    '--theme-settings-delete-button-legacy-background-color-hover': '#414251',
    '--theme-settings-pane-background-color': '#1e1f31',
    '--theme-settings-pane-border': 'none',
    '--theme-settings-menu-box-background-color': '#1e1f31',
    '--theme-settings-menu-box-border': 'none',
    '--theme-settings-menu-item-text-color': '#ffffff',
    '--theme-settings-menu-item-text-color-active': '#ffffff',
    '--theme-settings-menu-item-text-color-disabled': '#ffffff80',
    '--theme-settings-menu-item-background-color-active':
      'rgba(255, 255, 255, 0.07)',
    '--theme-settings-menu-item-left-border-color-active': '#1fc1c3',
    '--theme-settings-theme-select-title-color': '#ffffff',
    '--theme-settings-theme-select-border-color': 'rgba(255, 255, 255, 0.2)',
    '--theme-settings-undelegate-wallet-divider-border-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-settings-undelegate-wallet-deposit-amount-color': '#2dc06c',
    '--theme-settings-undelegate-wallet-fees-amount-color': '#ea4c5b',
  },
  sidebar: {
    '--theme-sidebar-background-color': '#36374d',
    '--theme-sidebar-category-background-color-hover': '#313245',
    '--theme-sidebar-category-background-color-active': '#2b2c3e',
    '--theme-sidebar-category-text-color': '#ffffff',
    '--theme-sidebar-layout-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-sidebar-layout-topbar-shadow-color': 'rgba(0, 0, 0, 0.12)',
    '--theme-sidebar-menu-background-color': '#2b2c3e',
    '--theme-sidebar-menu-item-background-color-hover': 'rgba(32, 33, 46, 0.5)',
    '--theme-sidebar-menu-item-background-color-active': '#20212e',
    '--theme-sidebar-menu-item-wallet-name-color': '#ffffff',
    '--theme-sidebar-menu-item-wallet-info-color': '#ffffff',
    '--theme-sidebar-menu-add-button-background-color': '#20212e',
    '--theme-sidebar-menu-add-button-background-color-active': '#20212ea8',
    '--theme-sidebar-menu-add-button-background-color-hover': '#20212ea8',
    '--theme-sidebar-menu-add-button-text-color': '#ffffff',
    '--theme-sidebar-wallets-scrollbar-background-color':
      'rgba(255, 255, 255, 0.3)',
    '--theme-sidebar-wallets-scrollbar-background-color-active':
      'rgba(255, 255, 255, 0.5)',
    '--theme-sidebar-wallets-scrollbar-background-color-hover':
      'rgba(255, 255, 255, 0.5)',
    '--theme-sidebar-category-networkInfo-background-color': '#1fc1c3',
    '--theme-sidebar-category-networkInfo-text-color': '#121326',
  },
  splash: {
    '--theme-splash-network-background-color': 'rgba(42, 43, 60, 0.96)',
    '--theme-splash-network-background-color1': 'rgba(18, 19, 38, 1)',
    '--theme-splash-network-background-color2': 'rgba(18, 19, 38, 0.91)',
    '--theme-splash-network-background-color3': 'rgba(18, 19, 38, 0.31)',
    '--theme-splash-network-background-color4': 'rgba(18, 19, 38, 0)',
    '--theme-splash-network-background-color5': 'rgba(18, 19, 38, 0)',
    '--theme-splash-network-background-color6': 'rgba(18, 19, 38, 0.31)',
    '--theme-splash-network-background-color7': 'rgba(18, 19, 38, 0.91)',
    '--theme-splash-network-background-color8': 'rgba(18, 19, 38, 1)',
    '--theme-splash-network-logo-fill-color': '#fff',
    '--theme-splash-network-overlay-button-background-color':
      'rgba(0, 0, 0, 0.1)',
    '--theme-splash-network-overlay-button-background-color-hover': '#fff',
    '--theme-splash-network-overlay-button-border-color': '#fff',
    '--theme-splash-network-overlay-button-text-color': '#fff',
    '--theme-splash-network-overlay-button-text-color-hover': '#2a2b3c',
    '--theme-splash-network-title-color': '#fff',
    '--theme-splash-network-scrollbar-thumb-background':
      'rgba(255, 255, 255, 0.3)',
    '--theme-splash-network-scrollbar-thumb-background-hover':
      'rgba(255, 255, 255, 0.5)',
    '--theme-splash-network-subTitle1-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-splash-network-subTitle2-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-splash-network-description-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-splash-network-description-color': 'rgba(255, 255, 255, 0.7)',
    '--theme-splash-network-description-bold-color': '#fff',
    '--theme-splash-network-learn-more-color': '#fff',
  },
  stakePools: {
    '--theme-staking-stake-pools-title-color': '#ffffff',
    '--theme-staking-stake-pools-search-button-color': '#ffffff',
    '--theme-staking-stake-pool-background-color': 'rgba(42, 43, 60, 1)',
    '--theme-staking-stake-pool-border-color': 'rgba(42, 43, 60, 1)',
    '--theme-staking-stake-pool-glow-color': '#1fc1c34c',
    '--theme-staking-stake-pool-grey-color': '#434554',
    '--theme-staking-stake-pool-grey-bg-color': 'rgba(255, 255, 255, 0.1)',
    '--theme-staking-stake-pools-search-icon-color': '#ffffff',
    '--theme-staking-stake-pool-saturation-background-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-staking-stake-pool-saturation-green-color': '#1ccc5d',
    '--theme-staking-stake-pool-saturation-orange-color': '#ff8800',
    '--theme-staking-stake-pool-saturation-red-color': '#ea4c5b',
    '--theme-staking-stake-pool-saturation-yellow-color': '#ffcc00',
    '--theme-staking-stake-pool-selected-background-color': '#5da377',
    '--theme-staking-stake-pool-selected-checkmark-icon-color': '#ffffff',
    '--theme-staking-stake-pool-selected-ticker-color': '#ffffff',
    '--theme-staking-stake-pool-ticker-color': '#ffffff',
    '--theme-staking-stake-pool-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-background-color':
      'rgba(42, 43, 60, 0.98)',
    '--theme-staking-stake-pool-tooltip-border-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-staking-stake-pool-tooltip-shadow-color': 'rgba(0, 0, 0, 0.25)',
    '--theme-staking-stake-pool-tooltip-text-color': '#ffffff',
    '--theme-staking-stake-pool-tooltip-link-color': '#1fc1c3',
    '--theme-staking-stake-pool-tooltip-table-title-color': '#ffffff',
    '--theme-staking-stake-pool-tooltip-table-param-color': '#ffffff',
    '--theme-staking-stake-pool-tooltip-retirement-text-color': '#fafbfc',
    '--theme-staking-stake-pool-tooltip-retirement-background-color': '#ea4c5b',
    '--theme-staking-stake-pool-tooltip-delegate-button-background-color':
      '#1fc1c3',
    '--theme-staking-stake-pool-tooltip-delegate-button-hover-background-color':
      '#26e7eb',
    '--theme-staking-stake-pool-tooltip-delegate-button-active-background-color':
      '#1cabad',
    '--theme-staking-stake-pool-tooltip-delegate-button-text-color':
      'rgba(18, 19, 38, 1)',
    '--theme-staking-stake-pool-tooltip-delegate-button-inverse-text-color':
      'rgba(18, 19, 38, 1)',
    '--theme-staking-stake-pool-tooltip-delegate-button-border-color':
      'transparent',
    '--theme-staking-progress-label-light': '#121326',
    '--theme-staking-stake-pools-search-clear-button-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-staking-stake-pools-search-clear-button-color': '#ffffff',
    '--theme-staking-stake-pools-search-clear-button-hover-background-color':
      'rgba(255, 255, 255, 0.05)',
    '--theme-staking-stake-pool-tooltip-neutral-background-color':
      'rgba(255, 255, 255, .1)',
    '--theme-staking-stake-pool-tooltip-neutral-text-color': '#ffffff',
    '--theme-staking-stake-pool-tooltip-id-background-color':
      'rgba(255, 255, 255, 0.05)',
    '--theme-staking-stake-pool-tooltip-id-shadow-1': 'rgba(0, 0, 0, 0.25)',
    '--theme-staking-stake-pool-tooltip-id-shadow-2': 'rgba(42, 43, 60, 0)',
  },
  staking: {
    '--theme-staking-content-background-color': '#1e1f31',
    '--theme-staking-content-border-color': '#1e1f31',
    '--theme-staking-font-color-accent': '#ffffff',
    '--theme-staking-font-color-regular': '#ffffff',
    '--theme-staking-font-color-light': '#ffffffb3',
    '--theme-staking-font-color-lighter': 'rgba(255, 255, 255, 0.5)',
    '--theme-staking-table-head-background-color': 'rgba(255, 255, 255, 0.07)',
    '--theme-staking-table-border-color': 'rgba(255, 255, 255, 0.2)',
    '--theme-staking-link-color': '#1fc1c3',
    '--theme-staking-link-color-light': 'rgba(31, 193, 195, 0.7)',
    '--theme-staking-progress-bar-background-color':
      'rgba(255, 255, 255, 0.07)',
    '--theme-staking-progress-stripe-dark-1-background-color': '#0da2a4',
    '--theme-staking-progress-stripe-dark-2-background-color': '#1fc1c3',
    '--theme-staking-slider-background-color-1': '#1fc1c3',
    '--theme-staking-slider-background-color-2': '#ffffff',
    '--theme-staking-slider-box-shadow-color': 'rgba(0, 0, 0, 0.24)',
    '--theme-staking-table-body-highlighted-text-color': '#313245',
    '--theme-staking-info-learn-more-button-text-color': 'rgba(18, 19, 38, 1)',
    '--theme-staking-info-learn-more-icon-color': 'rgba(18, 19, 38, 1)',
    '--theme-staking-learn-more-button-color': 'rgba(18, 19, 38, 1)',
    '--theme-staking-learn-more-icon-color': 'rgba(18, 19, 38, 1)',
    '--theme-staking-donut-ring-completed-color': 'rgba(234, 76, 91, 0.2)',
    '--theme-staking-donut-ring-remaining-color': '#ea4c5b',
    '--theme-staking-wallet-row-border-color': 'rgba(255, 255, 255, 0.2)',
    '--theme-staking-dropdown-item-text-color-hover': '#ffffff',
    '--theme-staking-dropdown-item-background-color': '#3f404f',
    '--theme-staking-dropdown-item-background-color-hover':
      'rgba(255, 255, 255, 0.07)',
    '--theme-staking-delegation-center-gear-icon-fill-color': '#ffffff80',
    '--theme-staking-delegation-center-gear-icon-fill-color-active': '#ffffff',
    '--theme-staking-delegation-center-no-wallets-instructions-color':
      '#ffffff',
    '--theme-staking-delegation-center-divider-border-color':
      'rgba(255, 255, 255, 0.2)',
    '--theme-staking-delegation-center-fees-amount-color': '#ea4c5b',
    '--theme-staking-countdown-widget-background-color':
      'rgba(255, 255, 255, 0.07)',
    '--theme-staking-countdown-widget-delimeter-background-color': '#ffffff',
    '--theme-staking-countdown-widget-field-label-color':
      'rgba(255, 255, 255, 0.7)',
    '--theme-staking-countdown-widget-field-value-color': '#ffffff',
    '--theme-staking-export-button-shadow-color': 'rgba(31, 193, 195, 0.18)',
    '--theme-staking-export-button-color': 'rgba(18, 19, 38, 1)',
    '--theme-staking-wallet-row-action-undelegate-text-color': '#ea4c5b',
    '--theme-staking-wallet-row-ticker-background-color':
      'rgba(255, 255, 255, 0.07)',
    '--theme-staking-wallet-row-ticker-text-color': '#ffffff',
    '--theme-staking-wallet-row-ticker-ada-icon-fill-color': '#ffffff80',
    '--theme-staking-redeemItnRewards-text-color': '#ffffff',
    '--theme-staking-redeemItnRewards-separator-color': '#313245',
    '--theme-staking-redeemItnRewards-attention-text-color': '#ea4c5b',
    '--theme-staking-redeemItnRewards-description-text-color': '#414251b3',
    '--theme-staking-redeemItnRewards-icon-color': '#ffffff',
  },
  support: {
    '--theme-support-settings-item-color': '#ffffff',
    '--theme-support-settings-link-color': '#1fc1c3',
    '--theme-support-settings-text-color': '#ffffff',
  },
  syncing: {
    '--theme-syncing-background-color': '#121326',
    '--theme-syncing-text-color': '#ffffff',
  },
  systemError: {
    '--theme-system-error-overlay-attention-icon-color': '#ffffff',
    '--theme-system-error-overlay-background-color': 'rgba(171, 23, 0, 0.94)',
    '--theme-system-error-overlay-support-link-icon-color': '#ffffff',
    '--theme-system-error-overlay-text-color': '#ffffff',
  },
  tabs: {
    '--theme-choice-tabs-text-color': '#ffffff',
    '--theme-choice-tabs-text-color-active': '#ffffff',
    '--theme-choice-tabs-bottom-border-color-active': '#ffffff',
  },
  testEnvironment: {
    '--theme-test-environment-label-background-color': '#ab1700',
    '--theme-test-environment-label-text-color': '#ffffff',
  },
  topBar: {
    '--theme-topbar-background-color': '#2a2b3c',
    '--theme-topbar-layout-body-background-color': '#121326',
    '--theme-topbar-wallet-name-color': '#ffffff',
    '--theme-topbar-wallet-info-color': '#ffffff',
    '--theme-topbar-logo-color': '#ffffff',
  },
  transactions: {
    '--theme-transactions-list-background-color': '#1e1f31',
    '--theme-transactions-list-border-color': '1e1f31',
    '--theme-transactions-list-group-date-color': '#ffffff',
    '--theme-transactions-list-item-details-color': '#ffffff',
    '--theme-transactions-list-item-highlight-color': '#ea4c5b',
    '--theme-transactions-state-ok-background-color': '#2cbb69',
    '--theme-transactions-state-pending-background-color':
      'rgba(255, 255, 255, 0.5)',
    '--theme-transactions-state-text-color': '#121326',
    '--theme-transactions-search-background-color': '#121326',
    '--theme-transactions-icon-type-expend-background-color': '#709cf0',
    '--theme-transactions-icon-type-income-background-color': '#2cbb69',
    '--theme-transactions-icon-type-exchange-background-color': '#10aca4',
    '--theme-transactions-arrow-stroke-color': '#ffffff',
    '--theme-transactions-icon-type-pending-regular-background-color':
      'rgba(255, 255, 255, 0.5)',
    '--theme-transactions-icon-type-pending-warning-background-color':
      'rgba(236, 93, 107, 0.8)',
    '--theme-transactions-icon-type-failed-background-color':
      'rgba(236, 93, 107, 1)',
    '--theme-transactions-state-pending-warning-background-color': '#ec5d6b',
    '--theme-transactions-filter-button-shadow-color':
      'rgba(31, 193, 195, 0.18)',
    '--theme-transactions-filter-button-color': 'rgba(18, 19, 38, 1)',
    '--theme-transactions-date-picker-button-background-color':
      'rgba(255, 255, 255, 0.14)',
    '--theme-transactions-date-picker-button-background-color-hover':
      'rgba(255, 255, 255, 0.07)',
    '--theme-transactions-date-picker-button-color': '#ffffff',
    '--theme-transactions-filter-modal-bg-color': '#3f404f',
    '--theme-transactions-filter-date-picker-shadow':
      '0 0 20px 0 rgba(0, 0, 0, 0.4)',
    '--theme-transactions-filter-title-button-text-color': '#ffffff',
    '--theme-transactions-filter-title-button-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-transactions-filter-title-button-background-color-hover':
      'rgba(255, 255, 255, 0.05)',
    '--theme-transactions-filter-title-button-background-color-active':
      'rgba(255, 255, 255, 0.12)',
    '--theme-transactions-transfer-funds-selected-wallet-background-color':
      '#ffffff0d',
    '--theme-transactions-header-background-color': '#121326',
    '--theme-transactions-header-texct-color': '#ffffff80',
  },
  tokens: {
    '--theme-tokens-list-header-text-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-tokens-list-header-expanded-background-color':
      'rgba(255, 255, 255, 0.05)',
    '--theme-tokens-list-header-expanded-background-color-hover':
      'rgba(255, 255, 255, 0.03)',
  },
  uploader: {
    '--theme-uploader-text-color': '#ffffff',
    '--theme-uploader-border-color': 'rgba(255, 255, 255, 0.3)',
  },
  utxo: {
    '--theme-utxo-background-color': 'rgba(255, 255, 255, 0.05)',
    '--theme-utxo-title-text-color': '#ffffff',
    '--theme-utxo-title-description-color': '#ffffffb3',
    '--theme-utxo-bar-color': 'rgba(255, 255, 255, 0.5)',
    '--theme-utxo-label-text-color': '#ffffff73',
    '--theme-utxo-tick-text-color': '#ffffff73',
    '--theme-utxo-cursor-background-color': 'rgba(255, 255, 255, 0.1)',
    '--theme-utxo-tooltip-background-color': 'rgba(63, 64, 79, 0.9)',
    '--theme-utxo-tooltip-shadow-color': 'rgba(0, 0, 0, 0.18)',
    '--theme-utxo-tooltip-text-color': '#fff',
  },
  voting: {
    '--theme-voting-font-color-accent': '#ffffff',
    '--theme-voting-font-color-light': '#ffffffb3',
    '--theme-voting-font-color-regular': '#ffffff',
    '--theme-voting-info-background-color': 'rgba(255, 255, 255, 0.1)',
    '--theme-voting-info-font-color': '#ffffff',
    '--theme-voting-registration-steps-activation-steps-indicator-color':
      '#ffffff',
    '--theme-voting-registration-steps-choose-wallet-error-message-color':
      '#ea4c5b',
    '--theme-voting-registration-steps-choose-wallet-error-message-light-color':
      '#ea4c5bb3',
    '--theme-voting-registration-steps-deposit-fees-amount-color': '#ea4c5b',
    '--theme-voting-registration-steps-deposit-fees-label-color': '#ffffff',
    '--theme-voting-registration-steps-description-color': '#ffffffcc',
    '--theme-voting-registration-steps-description-highlighted-color':
      '#ffffff',
    '--theme-voting-separator-color': 'rgba(255, 255, 255, 0.15)',
    '--theme-voting-footer-separator-color': 'rgba(255, 255, 255, 0.5)',
  },
  walletImportDialog: {
    '--theme-wallet-import-background-color': 'rgba(42, 43, 60, 0.98)',
    '--theme-wallet-import-button-background-color': 'rgba(0, 0, 0, 0.1)',
    '--theme-wallet-import-button-border-color': '#fff',
    '--theme-wallet-import-button-text-color': '#fff',
    '--theme-wallet-import-button-text-color-hover': '#2a2b3c',
    '--theme-wallet-import-checkbox-border': '2px solid #ffffff',
    '--theme-wallet-import-checkbox-border-color': 'rgba(255, 255, 255, 1)',
    '--theme-wallet-import-checkbox-border-color-disabled': '#fff',
    '--theme-wallet-import-checkbox-check-bg-color': '#fff',
    '--theme-wallet-import-checkbox-check-color': '#243e62',
    '--theme-wallet-import-description-close-color': '#fafbfc',
    '--theme-wallet-import-description-close-hover-background':
      'rgba(0, 0, 0, 0.16)',
    '--theme-wallet-import-description-color': 'rgba(255, 255, 255, 0.7)',
    '--theme-wallet-import-description-bold-color': '#fff',
    '--theme-wallet-import-error-color': '#ea4c5b',
    '--theme-wallet-import-input-background-color': '#313245',
    '--theme-wallet-import-input-background-color-disabled':
      'rgba(255, 255, 255, 0.05)',
    '--theme-wallet-import-input-border-color': 'rgba(255, 255, 255, 0.3)',
    '--theme-wallet-import-stateFolder-border-color':
      'rgba(255, 255, 255, 0.5)',
    '--theme-wallet-import-stateFolder-button-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-wallet-import-stateFolder-button-background-color-hover':
      'rgba(255, 255, 255, 0.05)',
    '--theme-wallet-import-stateFolder-button-background-color-active':
      'rgba(255, 255, 255, 0.12)',
    '--theme-wallet-import-stateFolder-label-color': '#fff',
    '--theme-wallet-import-title-color': '#fff',
  },
  walletNotRespondingOverlay: {
    '--theme-wallet-not-responding-background-color': '#36374df5',
    '--theme-wallet-not-responding-button-background-color': '#313245',
    '--theme-wallet-not-responding-button-background-color-hover': '#ffffff',
    '--theme-wallet-not-responding-button-border-color': '#ffffff',
    '--theme-wallet-not-responding-button-text-color': '#ffffff',
    '--theme-wallet-not-responding-button-text-color-hover': '#2a2b3c',
    '--theme-wallet-not-responding-description-background-color': '#313245',
    '--theme-wallet-not-responding-description-text-color':
      'rgba(255, 255, 255, 0.7)',
    '--theme-wallet-not-responding-icon-color': '#ffffff',
    '--theme-wallet-not-responding-link-text-color': '#ffffff',
    '--theme-wallet-not-responding-title-text-color': '#ffffff',
  },
  walletRestoreDialog: {
    '--theme-wallet-restore-dialog-new-label-background-color':
      'rgba(255, 255, 255, 0.1)',
    '--theme-wallet-restore-dialog-new-label-color': '#ffffff',
    '--theme-wallet-restore-dialog-step-walletType-hardwareWalletDisclaimer-text-color':
      '#ea4c5b',
  },
  walletSettings: {
    '--theme-wallet-settings-section-separator-color':
      'rgba(255, 255, 255, 0.15)',
  },
  widgets: {
    '--theme-widgets-asset-token-fingerprint-background-color':
      'rgba(255,255,255,0.1)',
    '--theme-widgets-asset-token-text-color': '#ffffff',
    '--theme-widgets-asset-token-background-color': 'rgba(42, 43, 60, 0.98)',
    '--theme-widgets-asset-token-box-shadow': 'rgba(0, 0, 0, 0.25)',
    '--theme-widgets-itemsDropdown-option-label-text-color': '#ffffff',
  },
};
const DARK_CARDANO_THEME_PARAMS: CreateThemeParams = {
  config: DARK_CARDANO_THEME_OUTPUT,
};
export default createTheme(DARK_CARDANO_THEME_PARAMS);
