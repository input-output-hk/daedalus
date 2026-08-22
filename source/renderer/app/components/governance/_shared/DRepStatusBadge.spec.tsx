import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import DRepStatusBadge, { getDRepStanding } from './DRepStatusBadge';

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

  it('reads an active DRep inside the window as expiring', () => {
    expect(getDRepStanding('active', 4)).toBe('expiring');
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

  it('labels a lapsed DRep inactive rather than expiring', () => {
    renderBadge({ status: 'inactive', drepActivity: 4 });

    expect(screen.getByLabelText('!!!Inactive')).toBeInTheDocument();
    expect(screen.queryByText('!!!Expiring soon')).not.toBeInTheDocument();
  });

  it('states the remaining epochs and days while expiring', () => {
    renderBadge({
      status: 'active',
      drepActivity: 4,
      epochLength: EPOCH_LENGTH,
      slotLength: SLOT_LENGTH,
    });

    expect(screen.getByText('!!!Expiring soon')).toBeInTheDocument();
    expect(
      screen.getByText('!!!Expiring soon').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Voting power lapses in 4 epochs (20 days) unless this DRep records activity by voting, updating its metadata or re-registering.'
    );
  });

  it('states epochs alone when the network parameters have not loaded', () => {
    renderBadge({ status: 'active', drepActivity: 4 });

    expect(
      screen.getByText('!!!Expiring soon').closest('span[title]')
    ).toHaveAttribute(
      'title',
      '!!!Voting power lapses in 4 epochs unless this DRep records activity by voting, updating its metadata or re-registering.'
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
      screen.getByLabelText(/Expiring soon\. .*lapses in 2 epochs/)
    ).toBeInTheDocument();
  });
});
