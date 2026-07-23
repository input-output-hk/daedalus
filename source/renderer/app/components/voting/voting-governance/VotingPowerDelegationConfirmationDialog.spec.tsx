import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import VotingPowerDelegationConfirmationDialog from './VotingPowerDelegationConfirmationDialog';

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

const softwareWallet = {
  id: 'wallet-1',
  isHardwareWallet: false,
  name: 'Test Wallet',
} as any;

const hardwareWallet = {
  id: 'hw-wallet-1',
  isHardwareWallet: true,
  name: 'HW Test Wallet',
} as any;

const renderDialog = (overrides: Record<string, unknown> = {}) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <VotingPowerDelegationConfirmationDialog
          chosenOption={VALID_DREP_ID}
          drepIdentity={{ credentialType: 'key', raw: VALID_DREP_ID }}
          fees={new BigNumber('0.174257')}
          hwDeviceStatus={HwDeviceStatuses.READY}
          isTrezor={false}
          onClose={jest.fn()}
          onExternalLinkClick={jest.fn()}
          onSubmit={jest.fn(async () => ({ success: true as const }))}
          redirectToWallet={jest.fn()}
          selectedWallet={softwareWallet}
          {...overrides}
        />
      </IntlProvider>
    </ThemeProvider>
  );

describe('VotingPowerDelegationConfirmationDialog — DRep identity', () => {
  afterEach(cleanup);

  it('renders the full raw DRep ID (byte-equal) instead of the generic label', () => {
    renderDialog();

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    const idNode = screen.getByText(VALID_DREP_ID);
    expect(idNode.textContent).toBe(VALID_DREP_ID);
    expect(
      screen.queryByText('Delegate to DRep (default)')
    ).not.toBeInTheDocument();
  });

  it('still renders the Abstain sentinel label', () => {
    renderDialog({ chosenOption: 'abstain', drepIdentity: null });

    expect(screen.getByText('Vote')).toBeInTheDocument();
    expect(screen.getByText('Abstain')).toBeInTheDocument();
    expect(screen.queryByText(VALID_DREP_ID)).not.toBeInTheDocument();
  });

  it('still renders the No Confidence sentinel label', () => {
    renderDialog({ chosenOption: 'no_confidence', drepIdentity: null });

    expect(screen.getByText('No Confidence')).toBeInTheDocument();
    expect(screen.queryByText(VALID_DREP_ID)).not.toBeInTheDocument();
  });

  it('never renders a name field, even if extra fields sneak into the identity', () => {
    renderDialog({
      drepIdentity: {
        credentialType: 'key',
        givenName: 'Sneaky Unverified Name',
        raw: VALID_DREP_ID,
      } as any,
    });

    expect(
      screen.queryByText('Sneaky Unverified Name')
    ).not.toBeInTheDocument();
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
  });
});

describe('VotingPowerDelegationConfirmationDialog — hardware-wallet device states', () => {
  afterEach(cleanup);

  // The AC device states map onto the real HwDeviceStatuses: disconnected and
  // locked surface as CONNECTING/CONNECTING_FAILED (PIN-unlock copy),
  // app-not-open as LAUNCHING_CARDANO_APP, signing-rejected as
  // VERIFYING_TRANSACTION_FAILED, Trezor invalid-state as UNRECOGNIZED_WALLET.
  it.each([
    [
      HwDeviceStatuses.CONNECTING,
      'Connect the "HW Test Wallet" device and enter your PIN to unlock it',
    ],
    [
      HwDeviceStatuses.CONNECTING_FAILED,
      'Disconnect and reconnect your hardware wallet to restart the process.',
    ],
    [
      HwDeviceStatuses.LAUNCHING_CARDANO_APP,
      'Launch Cardano application on your device',
    ],
    [
      HwDeviceStatuses.VERIFYING_TRANSACTION,
      'Confirm the transaction using the "HW Test Wallet" device',
    ],
    [
      HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED,
      'Transaction confirmation failed',
    ],
    [
      HwDeviceStatuses.UNRECOGNIZED_WALLET,
      'We do not recognize this wallet on your device. Please ensure that you are using the same device that you selected for pairing "HW Test Wallet" and that you have entered the correct passphrase.',
    ],
  ])('renders the %s device state', (hwDeviceStatus, expectedText) => {
    renderDialog({ hwDeviceStatus, selectedWallet: hardwareWallet });
    expect(screen.getByText(expectedText)).toBeInTheDocument();
  });

  it('shows the Trezor passphrase hint while the device verifies the transaction', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      isTrezor: true,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByText('Enter passphrase if needed')).toBeInTheDocument();
  });

  it('shows the byte-equal DRep ID and no passphrase input on the hardware-wallet confirmation', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
    expect(document.querySelector('input[type="password"]')).toBeNull();
  });

  it('enables Confirm only after the device reports signing success', () => {
    const { unmount } = renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    unmount();
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByRole('button', { name: 'Confirm' })).not.toBeDisabled();
  });
});
