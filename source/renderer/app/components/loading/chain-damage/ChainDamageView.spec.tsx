import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';

import type { ChainDamage } from '../../../../../common/types/watchdog.types';
import ChainDamageView from './ChainDamageView';

const damage = (overrides: Partial<ChainDamage> = {}): ChainDamage => ({
  kind: 'ledger_point_missing',
  slot: 200102375,
  lastGoodSlot: null,
  detectedAt: '2026-10-16T12:00:00.000Z',
  ...overrides,
});

const renderView = (value: ChainDamage = damage(), handlers = {}) => {
  const props = {
    onRebuild: jest.fn(),
    onDismiss: jest.fn(),
    ...handlers,
  };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <ChainDamageView damage={value} {...props} />
    </IntlProvider>
  );
  return props;
};

const clickButton = (name: string) =>
  fireEvent.click(screen.getByRole('button', { name }));

describe('ChainDamageView', () => {
  afterEach(cleanup);

  it.each([
    [
      'node_rejects_canonical_block',
      'Daedalus cannot finish syncing because part of the blockchain data stored on this computer is missing.',
    ],
    [
      'ledger_point_missing',
      'Cardano node cannot start because part of the blockchain data stored on this computer is missing.',
    ],
    [
      'wallet_chain_discontinuity',
      'A wallet cannot finish syncing because part of the blockchain data stored on this computer is missing.',
    ],
  ])('shows the symptom for %s', (kind, symptom) => {
    renderView(damage({ kind: kind as ChainDamage['kind'] }));
    expect(
      screen.getByRole('heading', {
        name: 'Blockchain data needs to be rebuilt',
      })
    ).toBeInTheDocument();
    expect(screen.getByText(symptom)).toBeInTheDocument();
    expect(screen.getByText(/Your wallets and funds are safe/)).toBeVisible();
  });

  it('shows the kind and slot for a support request', () => {
    renderView(damage({ kind: 'wallet_chain_discontinuity', slot: 200102408 }));
    expect(
      screen.getByText('Detected: wallet_chain_discontinuity at slot 200102408')
    ).toBeInTheDocument();
  });

  it('shows the kind alone when no slot is known', () => {
    renderView(damage({ slot: null }));
    expect(
      screen.getByText('Detected: ledger_point_missing')
    ).toBeInTheDocument();
  });

  it('opens the confirmation from Rebuild blockchain data without rebuilding', () => {
    const { onRebuild } = renderView();
    clickButton('Rebuild blockchain data');
    expect(
      screen.getByRole('heading', { name: 'Rebuild blockchain data?' })
    ).toBeInTheDocument();
    expect(onRebuild).not.toHaveBeenCalled();
  });

  it('returns to the choice view from Back', () => {
    const { onRebuild } = renderView();
    clickButton('Rebuild blockchain data');
    clickButton('Back');
    expect(
      screen.getByRole('heading', {
        name: 'Blockchain data needs to be rebuilt',
      })
    ).toBeInTheDocument();
    expect(onRebuild).not.toHaveBeenCalled();
  });

  it('rebuilds once from the confirmation, however often Rebuild is clicked', () => {
    const { onRebuild } = renderView();
    clickButton('Rebuild blockchain data');
    clickButton('Rebuild');
    fireEvent.click(screen.getByRole('button', { name: 'Rebuild' }));
    expect(onRebuild).toHaveBeenCalledTimes(1);
  });

  it('dismisses from Not now without rebuilding', () => {
    const { onRebuild, onDismiss } = renderView();
    clickButton('Not now');
    expect(onDismiss).toHaveBeenCalledTimes(1);
    expect(onRebuild).not.toHaveBeenCalled();
  });
});
