import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { bech32 } from 'bech32';
import { Cardano } from '@cardano-sdk/core';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import VotingPowerDelegationConfirmationDialog from './VotingPowerDelegationConfirmationDialog';
import { messages } from './VotingPowerDelegationConfirmationDialog.messages';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';

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
          verifiedName={null}
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

  it('never renders a name carried on the identity object', () => {
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

describe('VotingPowerDelegationConfirmationDialog — fee, hardware and passphrase sections', () => {
  afterEach(cleanup);

  it('renders the fee row with the formatted amount', () => {
    renderDialog();

    expect(screen.getByText('Transaction fee')).toBeInTheDocument();
    expect(screen.getByText('0.174257 ADA')).toBeInTheDocument();
  });

  it('renders the labelled passphrase input for a software wallet', () => {
    renderDialog();

    expect(screen.getByText('Spending password')).toBeInTheDocument();
    expect(document.querySelector('input[type="password"]')).not.toBeNull();
    expect(
      screen.queryByText(
        'Confirm the transaction using the "HW Test Wallet" device'
      )
    ).not.toBeInTheDocument();
  });

  it('renders the device status instead of the passphrase input for a hardware wallet', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });

    expect(
      screen.getByText(
        'Confirm the transaction using the "HW Test Wallet" device'
      )
    ).toBeInTheDocument();
    expect(document.querySelector('input[type="password"]')).toBeNull();
    expect(screen.queryByText('Spending password')).not.toBeInTheDocument();
  });

  it('keeps the dialog chrome and introduces no comparison rows', () => {
    renderDialog();

    expect(screen.getByText('Confirm Transaction')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Cancel' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeInTheDocument();
    expect(screen.queryByText(/previous vote/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/new vote/i)).not.toBeInTheDocument();
    expect(messages).not.toHaveProperty('previousVote');
    expect(messages).not.toHaveProperty('newVote');
  });
});

describe('VotingPowerDelegationConfirmationDialog — identity block', () => {
  const KEY_CIP129 =
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
  const KEY_CIP105 =
    'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
  const KEY_CREDENTIAL_HEX =
    'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
  const SCRIPT_CIP129 =
    'drep1ydwykw3frpmsda0y60ptrgyl3e7kck628y5pwph4unfu9vg6sn5zd';
  const SCRIPT_CIP105 =
    'drep_script1t39n52gcwur0texnc2c6p8uw04k9kj3e9qtsda0y60ptzae75nh';
  const SCRIPT_CREDENTIAL_HEX =
    '5c4b3a29187706f5e4d3c2b1a09f8e7d6c5b4a39281706f5e4d3c2b1';
  const LEGACY_DREP_ID =
    'drep1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg4v077e';

  // CIP-129 carries a one-byte credential-type header ahead of the credential;
  // CIP-105 carries the bare credential.
  const credentialHexOf = (id: string): string => {
    const decoded = bech32.decode(id);
    const bytes = bech32.fromWords(decoded.words);
    const credential = decoded.prefix === 'drep' ? bytes.slice(1) : bytes;
    return credential.map((b) => b.toString(16).padStart(2, '0')).join('');
  };

  const renderIdentity = (drepId: string) =>
    renderDialog({
      chosenOption: drepId,
      drepIdentity: normalizeDRepIdentity(drepId),
    });

  afterEach(cleanup);

  it('renders all four parts for a key DRep', () => {
    renderIdentity(KEY_CIP129);

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    expect(screen.getByText(KEY_CIP129).textContent).toBe(KEY_CIP129);
    expect(screen.getByText('!!!CIP-105 DRep ID')).toBeInTheDocument();
    expect(screen.getByText(KEY_CIP105).textContent).toBe(KEY_CIP105);
    expect(screen.getByText('!!!Signed payload')).toBeInTheDocument();
    expect(
      screen.getByText(`{"vote":{"type":"drep","id":"${KEY_CREDENTIAL_HEX}"}}`)
    ).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();

    const templateOrder = [
      '!!!DRep ID',
      '!!!CIP-105 DRep ID',
      '!!!Signed payload',
      'Transaction fee',
    ];
    expect(
      Array.from(document.querySelectorAll('p'))
        .map((node) => node.textContent ?? '')
        .filter((text) => templateOrder.includes(text))
    ).toEqual(templateOrder);
  });

  it('renders the script CIP-105 form for a script DRep', () => {
    renderIdentity(SCRIPT_CIP129);

    expect(screen.getByText(SCRIPT_CIP129).textContent).toBe(SCRIPT_CIP129);
    expect(screen.getByText(SCRIPT_CIP105).textContent).toBe(SCRIPT_CIP105);
    expect(
      screen.getByText(
        `{"vote":{"type":"drep","id":"${SCRIPT_CREDENTIAL_HEX}"}}`
      )
    ).toBeInTheDocument();
  });

  it('renders one bech32 line when the id is already the CIP-105 form', () => {
    renderIdentity(SCRIPT_CIP105);

    expect(screen.getAllByText(SCRIPT_CIP105)).toHaveLength(1);
    expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    expect(screen.getByText('!!!Signed payload')).toBeInTheDocument();
    expect(
      screen.getByText(
        `{"vote":{"type":"drep","id":"${SCRIPT_CREDENTIAL_HEX}"}}`
      )
    ).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
  });

  it.each([
    [KEY_CIP129, KEY_CIP105],
    [SCRIPT_CIP129, SCRIPT_CIP105],
  ])(
    'renders three representations of one credential for %s',
    (cip129, cip105) => {
      renderIdentity(cip129);

      const payload = JSON.parse(screen.getByText(/"vote"/).textContent);
      expect(payload.vote.type).toBe('drep');
      expect(payload.vote.id).toHaveLength(56);
      expect(credentialHexOf(screen.getByText(cip129).textContent)).toBe(
        payload.vote.id
      );
      expect(credentialHexOf(screen.getByText(cip105).textContent)).toBe(
        payload.vote.id
      );
    }
  );

  it.each([
    [KEY_CIP129, KEY_CREDENTIAL_HEX, Cardano.CredentialType.KeyHash],
    [SCRIPT_CIP129, SCRIPT_CREDENTIAL_HEX, Cardano.CredentialType.ScriptHash],
  ])(
    'renders the same credential hex the hardware path sends for %s',
    (cip129, expectedHex, expectedType) => {
      // Both hardware mappers hand the device this hash as keyHashHex /
      // scriptHashHex, while the dialog shows bech32 — the two are only
      // comparable through the credential.
      const { hash, type } = Cardano.DRepID.toCredential(
        Cardano.DRepID(cip129)
      );
      expect(hash).toBe(expectedHex);
      expect(type).toBe(expectedType);

      renderIdentity(cip129);
      expect(
        screen.getByText(`{"vote":{"type":"drep","id":"${expectedHex}"}}`)
      ).toBeInTheDocument();
    }
  );

  it('renders only the verbatim primary line when the decoder rejects the id', () => {
    expect(normalizeDRepIdentity(LEGACY_DREP_ID)).toBeNull();
    renderIdentity(LEGACY_DREP_ID);

    expect(screen.getByText('!!!DRep ID')).toBeInTheDocument();
    expect(screen.getByText(LEGACY_DREP_ID).textContent).toBe(LEGACY_DREP_ID);
    expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!Signed payload')).not.toBeInTheDocument();
    expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
    expect(screen.queryByText('Vote')).not.toBeInTheDocument();
  });

  it.each(['abstain', 'no_confidence'])(
    'renders no identity block for the %s sentinel',
    (option) => {
      renderDialog({ chosenOption: option, drepIdentity: null });

      expect(screen.getByText('Vote')).toBeInTheDocument();
      expect(screen.queryByText('!!!DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!CIP-105 DRep ID')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!Signed payload')).not.toBeInTheDocument();
      expect(screen.queryByText('!!!On-chain')).not.toBeInTheDocument();
    }
  );
});

describe('VotingPowerDelegationConfirmationDialog — verified name', () => {
  const KEY_CIP129 =
    'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
  const KEY_CIP105 =
    'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
  const KEY_CREDENTIAL_HEX =
    'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
  const verified = {
    host: 'raw.githubusercontent.com',
    name: 'Daedalus Test DRep',
  };

  const renderVerified = (overrides: Record<string, unknown> = {}) =>
    renderDialog({
      chosenOption: KEY_CIP129,
      drepIdentity: normalizeDRepIdentity(KEY_CIP129),
      verifiedName: verified,
      ...overrides,
    });

  afterEach(cleanup);

  it('renders the verified name above the DRep ID', () => {
    renderVerified();

    expect(screen.getByText('!!!Verified name')).toBeInTheDocument();
    expect(screen.getByText('Daedalus Test DRep')).toBeInTheDocument();

    const order = [
      '!!!Verified name',
      '!!!DRep ID',
      '!!!CIP-105 DRep ID',
      '!!!Signed payload',
    ];
    expect(
      Array.from(document.querySelectorAll('p'))
        .map((node) => node.textContent ?? '')
        .filter((text) => order.includes(text))
    ).toEqual(order);
  });

  it('labels the verified name with both source labels and the host tooltip', () => {
    renderVerified();

    const label = screen.getByText('!!!Verified off-chain content');
    expect(label.getAttribute('title')).toEqual(
      expect.stringContaining('raw.githubusercontent.com')
    );
    // The source paragraph's own text node is " · !!!Name: ", which no exact
    // text matcher reaches; only the spans hold single matchable text nodes.
    const sourceLine = label.closest('p');
    expect(sourceLine?.textContent).toContain('!!!On-chain');
    expect(sourceLine?.textContent).toContain('!!!Name:');
  });

  it('keeps CIP-129, CIP-105 and the signed payload byte-equal when a name is added', () => {
    renderVerified();

    expect(screen.getByText(KEY_CIP129).textContent).toBe(KEY_CIP129);
    expect(screen.getByText(KEY_CIP105).textContent).toBe(KEY_CIP105);
    expect(
      screen.getByText(`{"vote":{"type":"drep","id":"${KEY_CREDENTIAL_HEX}"}}`)
    ).toBeInTheDocument();
  });

  it('renders no name and only the on-chain label when no verified metadata exists', () => {
    renderVerified({ verifiedName: null });

    expect(screen.queryByText('!!!Verified name')).not.toBeInTheDocument();
    expect(
      screen.queryByText('!!!Verified off-chain content')
    ).not.toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
  });

  it.each(['abstain', 'no_confidence'])(
    'renders no name for the %s sentinel',
    (option) => {
      renderVerified({
        chosenOption: option,
        drepIdentity: null,
        verifiedName: verified,
      });

      expect(screen.queryByText('!!!Verified name')).not.toBeInTheDocument();
      expect(screen.queryByText('Daedalus Test DRep')).not.toBeInTheDocument();
      expect(
        screen.queryByText('!!!Verified off-chain content')
      ).not.toBeInTheDocument();
      expect(screen.getByText('Vote')).toBeInTheDocument();
    }
  );
});
