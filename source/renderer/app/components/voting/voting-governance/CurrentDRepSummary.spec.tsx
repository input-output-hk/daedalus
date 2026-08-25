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
import CurrentDRepSummary from './CurrentDRepSummary';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';

const DREP_VOTE: DRepDelegation = {
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

const ACTIVE_ENTRY: AppDRepDirectoryEntry = {
  drepId: KEY_CIP129,
  votingPower: null,
  status: 'active',
  drepActivity: 30,
  anchor: null,
  verifiedName: null,
  doNotList: false,
};

const renderSummary = (
  currentDRep: DRepDelegation | null,
  drepEntry?: AppDRepDirectoryEntry | null
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <CurrentDRepSummary currentDRep={currentDRep} drepEntry={drepEntry} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('CurrentDRepSummary core states', () => {
  afterEach(cleanup);

  it('renders nothing when there is no current vote', () => {
    const { container } = renderSummary(null);
    expect(container.firstChild).toBeNull();
  });

  it('renders the DRep id row with the on-chain label and the neutral status caption when no directory entry is supplied (snapshot)', () => {
    const { container } = renderSummary(DREP_VOTE);
    // DRepIdDisplay truncates the visible text but exposes the full raw id.
    expect(screen.getByLabelText(KEY_CIP129)).toBeInTheDocument();
    expect(
      screen.getByText(
        "Could not load this DRep's data. It may have retired."
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('Active')).not.toBeInTheDocument();
    expect(screen.queryByText('Inactive')).not.toBeInTheDocument();
    expect(screen.queryByText('Inactive Soon')).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders abstain with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'abstain' });
    expect(screen.getByText('Abstain')).toBeInTheDocument();
    expect(
      screen.getByText(
        'Your stake is recorded on chain as not participating in governance.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders no confidence with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'no_confidence' });
    expect(screen.getByText('No Confidence')).toBeInTheDocument();
    expect(
      screen.getByText(
        'Your stake counts as Yes on every motion of no confidence, and as No on every other governance action.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });
});

describe('CurrentDRepSummary DRep status badge', () => {
  afterEach(cleanup);

  it('renders the shared active badge with no status caption', () => {
    const { container } = renderSummary(DREP_VOTE, ACTIVE_ENTRY);
    expect(screen.getByText('Active')).toBeInTheDocument();
    expect(
      screen.queryByText(/lapse in|currently inactive|status is loading/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('leaves the inactive-soon badge to say it, with no caption beneath', () => {
    const { container } = renderSummary(DREP_VOTE, {
      ...ACTIVE_ENTRY,
      drepActivity: 4,
    });
    expect(screen.getByText('Inactive Soon')).toBeInTheDocument();
    // This panel is on the screen for changing a delegation, so telling the
    // reader to consider redelegating tells them what they are already doing.
    expect(screen.queryByText(/redelegating/)).not.toBeInTheDocument();
    expect(screen.queryByText('Active')).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('leaves the inactive badge to say it, with no caption beneath', () => {
    const { container } = renderSummary(DREP_VOTE, {
      ...ACTIVE_ENTRY,
      status: 'inactive',
      drepActivity: 0,
    });
    expect(screen.getByText('Inactive')).toBeInTheDocument();
    expect(screen.queryByText(/redelegating/)).not.toBeInTheDocument();
    expect(screen.queryByText('Inactive Soon')).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('treats the threshold as expiring', () => {
    // Six epochs, the same threshold the directory badge and its filter use.
    // This panel had kept a twelve-epoch window of its own, which is sixty of
    // a DRep's hundred days.
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: 6 });
    expect(screen.getByText('Inactive Soon')).toBeInTheDocument();
  });

  it('treats one epoch beyond the threshold as active', () => {
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: 7 });
    expect(screen.getByText('Active')).toBeInTheDocument();
    expect(screen.queryByText('Inactive Soon')).not.toBeInTheDocument();
  });

  it('keeps the active badge when the remaining epochs are unknown', () => {
    renderSummary(DREP_VOTE, { ...ACTIVE_ENTRY, drepActivity: null });
    expect(screen.getByText('Active')).toBeInTheDocument();
    expect(screen.queryByText('Inactive Soon')).not.toBeInTheDocument();
  });

  it('renders no status badge or caption for the abstain sentinel', () => {
    renderSummary({ kind: 'abstain' }, ACTIVE_ENTRY);
    expect(screen.queryByText('Active')).not.toBeInTheDocument();
    expect(
      screen.queryByText(/lapse in|currently inactive|status is loading/)
    ).not.toBeInTheDocument();
  });
});
