import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';

import type { ChainDamage } from '../../../../common/types/watchdog.types';
import ChainDataSection from './ChainDataSection';

const damage: ChainDamage = {
  kind: 'wallet_chain_discontinuity',
  slot: 200102408,
  lastGoodSlot: 200080892,
  detectedAt: '2026-10-16T12:00:00.000Z',
};

const renderSection = (overrides = {}) => {
  const props = {
    chainDamage: null,
    isActionBlocked: false,
    onRestoreFocus: jest.fn(),
    onRebuildChain: jest.fn(),
    ...overrides,
  };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <ChainDataSection {...props} />
    </IntlProvider>
  );
  return props;
};

describe('ChainDataSection', () => {
  afterEach(cleanup);

  it('shows no damage by default and still offers the rebuild', () => {
    renderSection();
    expect(screen.getByText('No damage detected')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    ).toBeEnabled();
  });

  it('shows the slot and time of the last detection', () => {
    renderSection({ chainDamage: damage });
    const time = new Date(damage.detectedAt).toLocaleString('en-US');
    expect(
      screen.getByText(`Damage detected at slot 200102408 (${time})`)
    ).toBeInTheDocument();
  });

  it('shows the time alone when the detection has no slot', () => {
    renderSection({ chainDamage: { ...damage, slot: null } });
    const time = new Date(damage.detectedAt).toLocaleString('en-US');
    expect(screen.getByText(`Damage detected (${time})`)).toBeInTheDocument();
  });

  it('opens the confirmation without rebuilding', () => {
    const { onRebuildChain } = renderSection({ chainDamage: damage });
    fireEvent.click(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    );
    expect(screen.getByText('Rebuild blockchain data?')).toBeInTheDocument();
    expect(onRebuildChain).not.toHaveBeenCalled();
  });

  it('rebuilds only after Rebuild is clicked in the confirmation', () => {
    const { onRebuildChain } = renderSection();
    fireEvent.click(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Rebuild' }));
    expect(onRebuildChain).toHaveBeenCalledTimes(1);
  });

  it('returns to the row from Back without rebuilding', () => {
    const { onRebuildChain } = renderSection();
    fireEvent.click(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Back' }));
    expect(screen.getByText('No damage detected')).toBeInTheDocument();
    expect(onRebuildChain).not.toHaveBeenCalled();
  });

  it('disables the rebuild while Mithril work is active', () => {
    renderSection({ isActionBlocked: true });
    expect(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    ).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });
});
