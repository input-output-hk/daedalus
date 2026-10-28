import React from 'react';
import { IntlProvider } from 'react-intl';
import BigNumber from 'bignumber.js';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import {
  DiscreetModeFeatureProvider,
  DiscreetModeToggle,
} from '../../features/discreet-mode';
import { AnalyticsProvider } from '../analytics';
import { noopAnalyticsTracker } from '../../analytics';
import HeldRewardsNotification from './HeldRewardsNotification';

const HELD_REWARD = new BigNumber('3754573734').dividedBy(1000000);

const renderNotification = (
  onDelegate = jest.fn(),
  isDelegationSubmitted = false
) => {
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <AnalyticsProvider tracker={noopAnalyticsTracker}>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <>
              <DiscreetModeToggle />
              <div data-testid="notification">
                <HeldRewardsNotification
                  heldReward={HELD_REWARD}
                  onDelegate={onDelegate}
                  isDelegationSubmitted={isDelegationSubmitted}
                />
              </div>
            </>
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </AnalyticsProvider>
    </IntlProvider>
  );
  return onDelegate;
};

describe('HeldRewardsNotification', () => {
  afterEach(cleanup);

  it('shows the title and the held amount in the description', () => {
    renderNotification();
    const notification = screen.getByTestId('notification');
    expect(notification).toHaveTextContent('Staking rewards on hold');
    expect(notification).toHaveTextContent(
      '3,754.573734 ADA of staking rewards cannot be withdrawn from this wallet'
    );
    expect(notification).toHaveTextContent(
      'Delegate the voting power to any DRep, to Abstain or to No Confidence'
    );
  });

  it('hides the amount in discreet mode', () => {
    renderNotification();
    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );
    const notification = screen.getByTestId('notification');
    expect(notification).not.toHaveTextContent('3,754');
    expect(notification).toHaveTextContent(
      '*** ADA of staking rewards cannot be withdrawn from this wallet'
    );
  });

  it('calls its handler when the button is clicked', () => {
    const onDelegate = renderNotification();
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate voting power' })
    );
    expect(onDelegate).toHaveBeenCalledTimes(1);
  });

  it('says the delegation was submitted and offers no button after one', () => {
    renderNotification(jest.fn(), true);
    const notification = screen.getByTestId('notification');
    expect(notification).toHaveTextContent('Staking rewards on hold');
    expect(notification).toHaveTextContent(
      'The voting power delegation has been submitted. Once it is confirmed, 3,754.573734 ADA of staking rewards can be withdrawn.'
    );
    expect(
      screen.queryByRole('button', { name: 'Delegate voting power' })
    ).not.toBeInTheDocument();
  });
});
