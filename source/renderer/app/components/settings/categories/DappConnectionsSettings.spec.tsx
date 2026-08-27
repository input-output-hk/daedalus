import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../../i18n/locales/en-US.json';
import DappConnectionsSettings from './DappConnectionsSettings';
import type { DappConnectionsSettingsProps } from './DappConnectionsSettings';

const grant = {
  schemaVersion: 1 as const,
  origin: 'https://example.com',
  walletId: 'wallet-1',
  networkGenesis: 'genesis-1',
  networkMagic: 1,
  readScopes: ['connection', 'read', 'governance-key-disclosure'] as const,
  enabledExtensionScopes: [95],
  launch: { kind: 'diagnostics' as const },
  grantedAt: '2026-08-27T00:00:00.000Z',
};

const renderPage = (overrides: Partial<DappConnectionsSettingsProps> = {}) => {
  const props: DappConnectionsSettingsProps = {
    connections: [{ grant, walletName: 'Main wallet' }],
    corrupt: false,
    loading: false,
    failed: false,
    onDisconnect: jest.fn(),
    onForget: jest.fn(),
    onRevoke: jest.fn(),
    onRepair: jest.fn(),
    ...overrides,
  };
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <DappConnectionsSettings {...props} />
      </IntlProvider>
    </StoryDecorator>
  );
  return props;
};

describe('DappConnectionsSettings', () => {
  afterEach(cleanup);

  it('explains close, disconnect, and forget and invokes exact actions', () => {
    const props = renderPage();

    expect(screen.getByText(/Close dApp closes/)).toBeVisible();
    expect(screen.getByText(/Disconnect ends/)).toBeVisible();
    expect(screen.getByText(/Forget connection disconnects/)).toBeVisible();
    fireEvent.click(
      screen.getByRole('button', { name: 'Disconnect https://example.com' })
    );
    fireEvent.click(
      screen.getByRole('button', { name: 'Forget https://example.com' })
    );
    expect(props.onDisconnect).toHaveBeenCalledWith(grant);
    expect(props.onForget).toHaveBeenCalledWith(grant);
  });

  it('shows CIP-95 separately and never presents CIP-104 as available', () => {
    const props = renderPage();

    expect(
      screen.getByText(
        'CIP-104 account public-key disclosure is unavailable and grants no access.'
      )
    ).toBeVisible();
    fireEvent.click(
      screen.getByRole('button', {
        name: /Revoke CIP-95 governance public-key disclosure/,
      })
    );
    expect(props.onRevoke).toHaveBeenCalledWith(
      grant,
      'governance-key-disclosure'
    );
    expect(
      screen.queryByRole('button', { name: /Legacy CIP-104/ })
    ).not.toBeInTheDocument();
  });

  it('fails closed on corruption and exposes only repair', () => {
    const onRepair = jest.fn();
    renderPage({ corrupt: true, onRepair });

    expect(screen.getByRole('alert')).toHaveTextContent('Access is blocked');
    expect(screen.queryByText('https://example.com')).not.toBeInTheDocument();
    fireEvent.click(
      screen.getByRole('button', {
        name: 'Repair and remove saved connections',
      })
    );
    expect(onRepair).toHaveBeenCalledTimes(1);
  });
});
