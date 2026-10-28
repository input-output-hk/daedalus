import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import BigNumber from 'bignumber.js';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import CurrentDRepSummary from './CurrentDRepSummary';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';

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

const renderState = (props: React.ComponentProps<typeof CurrentDRepSummary>) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <CurrentDRepSummary {...props} />
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    </ThemeProvider>
  );

// A delegation the ledger holds, with the lookup of its DRep as given: an
// entry that arrived, or none after the lookup failed.
const renderSummary = (
  currentDRep: DRepDelegation | null,
  drepEntry?: AppDRepDirectoryEntry | null
) =>
  renderState({
    state:
      currentDRep == null
        ? { kind: 'none' }
        : { kind: 'current', voting: currentDRep, source: 'ledger' },
    lookup: drepEntry
      ? { status: 'found', entry: drepEntry }
      : { status: 'failed' },
  });

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
      screen.getByText("Could not load this DRep's data.")
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

describe('CurrentDRepSummary after the delegation ended', () => {
  afterEach(cleanup);

  const ended = (registeredAgain: boolean) =>
    ({ kind: 'ended', drep: DREP_VOTE.drep, registeredAgain }) as const;

  it('names the DRep and says the retirement ended the delegation', () => {
    renderState({
      state: ended(false),
      lookup: { status: 'not_found' },
      heldReward: new BigNumber(0),
    });
    expect(screen.getByText('Voting power not delegated')).toBeInTheDocument();
    expect(screen.getByLabelText(KEY_CIP129)).toBeInTheDocument();
    expect(
      screen.getByText(
        'This wallet last delegated its voting power to this DRep. The DRep retired, which ended the delegation.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/staking rewards/)).not.toBeInTheDocument();
  });

  it('says a new registration does not restore the delegation', () => {
    renderState({
      state: ended(true),
      lookup: { status: 'found', entry: ACTIVE_ENTRY },
    });
    expect(
      screen.getByText(
        /has since registered again\. A new registration does not restore earlier delegations\./
      )
    ).toBeInTheDocument();
  });

  it('adds the held reward when the wallet holds one', () => {
    renderState({
      state: ended(false),
      lookup: { status: 'idle' },
      heldReward: new BigNumber(12.5),
    });
    expect(
      screen.getByText(
        /of staking rewards cannot be withdrawn until the voting power is delegated to any DRep, Abstain or No Confidence\./
      )
    ).toBeInTheDocument();
  });

  it('says the wallet registry does not hold a DRep it cannot find', () => {
    renderState({
      state: { kind: 'not_in_registry', drep: DREP_VOTE.drep },
      lookup: { status: 'not_found' },
    });
    expect(screen.getByText('Currently Delegated To')).toBeInTheDocument();
    expect(
      screen.getByText("This DRep is not in the wallet's DRep registry.")
    ).toBeInTheDocument();
  });
});
