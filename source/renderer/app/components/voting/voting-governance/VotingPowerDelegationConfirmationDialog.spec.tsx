import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import VotingPowerDelegationConfirmationDialog from './VotingPowerDelegationConfirmationDialog';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

const softwareWallet = {
  id: 'wallet-1',
  isHardwareWallet: false,
  name: 'Test Wallet',
} as Wallet;

const hardwareWallet = {
  id: 'hw-wallet-1',
  isHardwareWallet: true,
  name: 'HW Test Wallet',
} as Wallet;

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
          drepIdentity={normalizeDRepIdentity(VALID_DREP_ID)}
          fees={new BigNumber('0.174257')}
          hwDeviceStatus={HwDeviceStatuses.READY}
          isTrezor={false}
          onClose={jest.fn()}
          onExternalLinkClick={jest.fn()}
          onSubmit={jest.fn(async () => ({ success: true as const }))}
          redirectToWallet={jest.fn()}
          selectedWallet={softwareWallet}
          verifiedName={null}
          {...overrides}
        />
      </IntlProvider>
    </ThemeProvider>
  );

describe('governance confirmation authorization', () => {
  afterEach(cleanup);

  it('shows the estimated fee and requires a spending password', () => {
    renderDialog();
    expect(screen.getByText('0.174257 ADA')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
  });

  it.each([VALID_DREP_ID, 'abstain', 'no_confidence'])(
    'authorizes %s only after explicit confirmation',
    async (chosenOption) => {
      const onSubmit = jest.fn(async () => ({ success: true as const }));
      renderDialog({ chosenOption, onSubmit });
      expect(onSubmit).not.toHaveBeenCalled();
      fireEvent.change(document.querySelector('input[type="password"]'), {
        target: { value: 'real-password' },
      });
      fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));
      await waitFor(() =>
        expect(onSubmit).toHaveBeenCalledWith('real-password')
      );
    }
  );

  it('recovers from an incorrect password without losing the reviewed choice', async () => {
    const onSubmit = jest
      .fn()
      .mockResolvedValueOnce({
        success: false,
        errorCode: 'wrong_encryption_passphrase',
      })
      .mockResolvedValueOnce({ success: true });
    const redirectToWallet = jest.fn();
    renderDialog({ onSubmit, redirectToWallet });
    fireEvent.change(document.querySelector('input[type="password"]'), {
      target: { value: 'wrong-password' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'Confirm' })).toBeEnabled()
    );
    expect(redirectToWallet).not.toHaveBeenCalled();
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
    fireEvent.change(document.querySelector('input[type="password"]'), {
      target: { value: 'correct-password' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));
    await waitFor(() =>
      expect(redirectToWallet).toHaveBeenCalledWith(softwareWallet.id)
    );
    expect(onSubmit).toHaveBeenLastCalledWith('correct-password');
  });

  it('cancels without authorizing a transaction', () => {
    const onClose = jest.fn();
    const onSubmit = jest.fn();
    renderDialog({ onClose, onSubmit });
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(onClose).toHaveBeenCalledTimes(1);
    expect(onSubmit).not.toHaveBeenCalled();
  });

  it.each([
    HwDeviceStatuses.VERIFYING_TRANSACTION,
    HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED,
  ])(
    'prevents hardware submission before signing success: %s',
    (hwDeviceStatus) => {
      renderDialog({ selectedWallet: hardwareWallet, hwDeviceStatus });
      expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
      expect(document.querySelector('input[type="password"]')).toBeNull();
    }
  );

  it('allows the signed hardware transaction to be submitted without software signing', async () => {
    const onSubmit = jest.fn(async () => ({ success: true as const }));
    renderDialog({
      selectedWallet: hardwareWallet,
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      onSubmit,
    });
    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));
    await waitFor(() => expect(onSubmit).toHaveBeenCalledWith(''));
  });
});

describe('governance signing identity', () => {
  afterEach(cleanup);

  it('keeps the selected identifier unchanged when displaying a verified name', () => {
    renderDialog({
      verifiedName: { host: 'verified.example', name: 'Verified DRep' },
    });
    expect(screen.getByText('Verified DRep')).toBeInTheDocument();
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
  });

  it('does not promote a name embedded in an unverified identity', () => {
    renderDialog({
      drepIdentity: { raw: VALID_DREP_ID, givenName: 'Unverified DRep' },
    });
    expect(screen.queryByText('Unverified DRep')).not.toBeInTheDocument();
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
  });

  it.each(['abstain', 'no_confidence'])(
    'does not attach DRep identity or verified metadata to %s',
    (chosenOption) => {
      renderDialog({
        chosenOption,
        drepIdentity: null,
        verifiedName: { host: 'verified.example', name: 'Not a sentinel name' },
      });
      expect(screen.queryByText('Not a sentinel name')).not.toBeInTheDocument();
      expect(screen.queryByText(VALID_DREP_ID)).not.toBeInTheDocument();
      expect(JSON.parse(document.querySelector('pre').textContent)).toEqual({
        vote: { type: chosenOption },
      });
    }
  );
});
