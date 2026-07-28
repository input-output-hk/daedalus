import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import CurrentVoteSummary from './CurrentVoteSummary';
import type { WalletVotingTarget } from '../../../api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';

const DREP_VOTE: WalletVotingTarget = {
  kind: 'drep',
  drep: {
    raw: KEY_CIP129,
    cip129: KEY_CIP129,
    cip105: KEY_CIP105,
    credentialHex: 'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c',
    credentialType: 'key',
  },
  source: 'onchain',
};

const renderSummary = (currentVote: WalletVotingTarget | null) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <CurrentVoteSummary currentVote={currentVote} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('CurrentVoteSummary core states', () => {
  afterEach(cleanup);

  it('renders the noDelegation warning, subline, and CTA (snapshot)', () => {
    const { container } = renderSummary(null);
    expect(screen.getByText('!!!No governance delegation')).toBeInTheDocument();
    expect(screen.getByRole('alert')).toBeInTheDocument();
    expect(screen.getByText('!!!Choose a delegation')).toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders the DRep id row with the on-chain label and no badge (snapshot)', () => {
    const { container } = renderSummary(DREP_VOTE);
    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    // DRepIdDisplay truncates the visible text but exposes the full raw id.
    expect(screen.getByLabelText(KEY_CIP129)).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
    expect(
      screen.queryByText(/Active|Inactive|Expiring/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders abstain with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'abstain' });
    expect(screen.getByText('!!!Abstain')).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders no confidence with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'no_confidence' });
    expect(screen.getByText('!!!No Confidence')).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });
});
