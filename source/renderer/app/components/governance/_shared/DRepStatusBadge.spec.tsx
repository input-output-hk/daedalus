import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import DRepStatusBadge from './DRepStatusBadge';
import { getDRepStanding } from './drepExpiry';

const EPOCH_LENGTH = 432000;
const SLOT_LENGTH = 1;

const renderBadge = (props: {
  status: 'active' | 'inactive';
  drepActivity?: number | null;
  epochLength?: number | null;
  slotLength?: number | null;
}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DRepStatusBadge {...props} />
    </IntlProvider>
  );

describe('getDRepStanding', () => {
  it('reads a healthy active DRep as active', () => {
    expect(getDRepStanding('active', 12)).toBe('active');
  });

  it('reads an active DRep inside the window as inactive soon', () => {
    expect(getDRepStanding('active', 4)).toBe('inactiveSoon');
  });

  it('reads a lapsed DRep as inactive whatever its epoch count', () => {
    // The two states are ordered rather than independent: once the voting
    // power is gone there is nothing left to expire.
    expect(getDRepStanding('inactive', 4)).toBe('inactive');
    expect(getDRepStanding('inactive', 12)).toBe('inactive');
  });

  it('reads an active DRep with no epoch count as active', () => {
    expect(getDRepStanding('active', null)).toBe('active');
  });
});

describe('DRepStatusBadge', () => {
  afterEach(cleanup);

  it('labels a healthy DRep active and offers no explanation', () => {
    renderBadge({
      status: 'active',
      drepActivity: 12,
      epochLength: EPOCH_LENGTH,
      slotLength: SLOT_LENGTH,
    });

    const badge = screen.getByLabelText('!!!Active');
    expect(badge).toBeInTheDocument();
    expect(badge).not.toHaveAttribute('title');
  });

  it('labels a lapsed DRep inactive rather than inactive soon', () => {
    renderBadge({ status: 'inactive', drepActivity: 4 });

    expect(screen.getByLabelText('!!!Inactive')).toBeInTheDocument();
    expect(screen.queryByText('!!!Inactive Soon')).not.toBeInTheDocument();
  });

  it('states the remaining epochs and days while expiring', () => {
    renderBadge({
      status: 'active',
      drepActivity: 4,
      epochLength: EPOCH_LENGTH,
      slotLength: SLOT_LENGTH,
    });

    expect(screen.getByText('!!!Inactive Soon')).toBeInTheDocument();
    expect(
      screen.getByText('!!!Inactive Soon').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Will become inactive in 4 epochs (20 days) without on-chain activity.'
    );
  });

  it('states epochs alone when the network parameters have not loaded', () => {
    renderBadge({ status: 'active', drepActivity: 4 });

    expect(
      screen.getByText('!!!Inactive Soon').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Will become inactive in 4 epochs without on-chain activity.'
    );
  });

  it('names both the label and the rule for screen readers', () => {
    renderBadge({
      status: 'active',
      drepActivity: 2,
      epochLength: EPOCH_LENGTH,
      slotLength: SLOT_LENGTH,
    });

    expect(
      screen.getByLabelText(
        /Inactive Soon\. .*Will become inactive in 2 epochs/
      )
    ).toBeInTheDocument();
  });
});
