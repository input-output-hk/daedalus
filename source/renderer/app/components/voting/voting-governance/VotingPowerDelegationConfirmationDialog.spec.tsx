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
