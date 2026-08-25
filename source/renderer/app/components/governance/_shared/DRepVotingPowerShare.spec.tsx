import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import DRepVotingPowerShare from './DRepVotingPowerShare';

const TOTAL = new BigNumber('5000000000000000'); // 5B ADA in lovelace
const ABOVE = new BigNumber('100000000000000'); // 100M ADA -> 2%
const BELOW = new BigNumber('50000000000000'); // 50M ADA -> 1%

const renderShare = (
  votingPower: BigNumber | null,
  totalDRepStake: BigNumber | null = TOTAL
) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DRepVotingPowerShare
        votingPower={votingPower}
        totalDRepStake={totalDRepStake}
      />
    </IntlProvider>
  );

describe('DRepVotingPowerShare', () => {
  afterEach(cleanup);

  it('states the share for every DRep, not only concentrated ones', () => {
    renderShare(BELOW);
    expect(screen.getByText('!!!1%')).toBeInTheDocument();
  });

  it('names the total the share is measured against, not the share again', () => {
    renderShare(BELOW);
    // 5B is the denominator; the DRep's own 50M is already on screen beside
    // the badge, so repeating it would leave the percentage unanchored. The
    // explanation hangs off the icon, which is what tells a reader it exists.
    expect(
      screen.getByRole('button', {
        name: '!!!This DRep controls 1% of active voting power (₳ 5.0B).',
      })
    ).toBeInTheDocument();
  });

  it('states the share above the threshold without arguing about it', () => {
    renderShare(ABOVE);
    expect(
      screen.getByRole('button', {
        name: '!!!This DRep controls 2% of active voting power (₳ 5.0B).',
      })
    ).toBeInTheDocument();
  });

  it('offers the explanation as a focusable control, not a hover target', () => {
    // A figure gives a reader no reason to hover it, and a pointer-only
    // tooltip is unreachable by keyboard either way.
    renderShare(BELOW);
    expect(screen.getByRole('button')).toBeInTheDocument();
  });

  it('reports a share too small to display as under the minimum, never as zero', () => {
    // 940 ADA against a 5B total rounds to 0% at two decimal places. A DRep
    // that holds voting power does not hold none of it.
    renderShare(new BigNumber('940000000'));
    expect(screen.getByText('!!!<0.01%')).toBeInTheDocument();
    expect(screen.queryByText('!!!0%')).toBeNull();
    // The symbol is compact and needs no translation, but a screen reader
    // cannot be relied on to announce it, so the explanation says it in words
    // and does not round the share to zero either.
    expect(
      screen.getByRole('button', {
        name: '!!!This DRep controls less than 0.01% of active voting power (₳ 5.0B).',
      })
    ).toBeInTheDocument();
  });

  it('renders nothing when the DRep totals are unavailable', () => {
    const { container } = renderShare(ABOVE, null);
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing when the voting power is unknown', () => {
    const { container } = renderShare(null);
    expect(container).toBeEmptyDOMElement();
  });
});
