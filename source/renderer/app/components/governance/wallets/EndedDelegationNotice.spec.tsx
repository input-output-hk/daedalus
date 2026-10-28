import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import EndedDelegationNotice from './EndedDelegationNotice';
import type { EndedDelegation } from './EndedDelegationNotice';

const DREP_ID = 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const drep = normalizeDRepIdentity(DREP_ID)!;

const renderNotice = (
  delegations: Array<EndedDelegation>,
  onDelegate = jest.fn()
) =>
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
            <EndedDelegationNotice
              delegations={delegations}
              onDelegate={onDelegate}
            />
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    </ThemeProvider>
  );

describe('EndedDelegationNotice', () => {
  afterEach(cleanup);

  it('renders nothing when no delegation ended', () => {
    const { container } = renderNotice([]);
    expect(container.firstChild).toBeNull();
  });

  it('names each wallet, the DRep and what ended the delegation', () => {
    renderNotice([
      {
        walletId: 'a',
        walletName: 'Savings',
        state: { kind: 'ended', drep, registeredAgain: false },
        heldReward: new BigNumber(0),
      },
      {
        walletId: 'b',
        walletName: 'Spending',
        state: { kind: 'ended', drep, registeredAgain: true },
        heldReward: new BigNumber(0),
      },
      {
        walletId: 'c',
        walletName: 'Old',
        state: { kind: 'not_in_registry', drep },
        heldReward: new BigNumber(0),
      },
    ]);

    expect(
      screen.getByText('Voting power no longer delegated')
    ).toBeInTheDocument();
    expect(screen.getByText('Savings')).toBeInTheDocument();
    expect(screen.getByText('Spending')).toBeInTheDocument();
    expect(screen.getByText('Old')).toBeInTheDocument();
    expect(screen.getAllByLabelText(DREP_ID)).toHaveLength(3);
    expect(screen.getAllByText(/The DRep retired/)).toHaveLength(2);
    expect(screen.getByText(/has since registered again/)).toBeInTheDocument();
    expect(
      screen.getByText("This DRep is not in the wallet's DRep registry.")
    ).toBeInTheDocument();
    expect(screen.queryByText(/staking rewards/)).not.toBeInTheDocument();
  });

  it('states the held reward only for a wallet that holds one', () => {
    renderNotice([
      {
        walletId: 'a',
        walletName: 'Savings',
        state: { kind: 'ended', drep, registeredAgain: false },
        heldReward: new BigNumber(3),
      },
    ]);

    expect(
      screen.getByText(/of staking rewards cannot be withdrawn until/)
    ).toBeInTheDocument();
  });

  it('opens the delegation form for the wallet', () => {
    const onDelegate = jest.fn();
    renderNotice(
      [
        {
          walletId: 'a',
          walletName: 'Savings',
          state: { kind: 'ended', drep, registeredAgain: false },
          heldReward: new BigNumber(0),
        },
      ],
      onDelegate
    );

    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate voting power' })
    );

    expect(onDelegate).toHaveBeenCalledWith('a');
  });
});
