import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import translations from '../../../i18n/locales/en-US.json';
import WalletsSettings from './WalletsSettings';

const renderSettings = (withdrawRewardsDefault: boolean) => {
  const onToggleWithdrawRewardsDefault = jest.fn();
  const view = render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <WalletsSettings
          currencySelected={null}
          currencyList={[]}
          currencyIsActive={false}
          onSelectCurrency={jest.fn()}
          onToggleCurrencyIsActive={jest.fn()}
          onOpenExternalLink={jest.fn()}
          withdrawRewardsDefault={withdrawRewardsDefault}
          onToggleWithdrawRewardsDefault={onToggleWithdrawRewardsDefault}
        />
      </IntlProvider>
    </StoryDecorator>
  );
  return { ...view, onToggleWithdrawRewardsDefault };
};

// The currency switch comes first, the withdraw-rewards switch second.
const switches = (container: HTMLElement) =>
  Array.from(
    container.querySelectorAll<HTMLInputElement>('input[type="checkbox"]')
  );

describe('WalletsSettings withdraw rewards', () => {
  afterEach(cleanup);

  it('explains the setting', () => {
    renderSettings(true);
    expect(
      screen.getByText('Withdraw staking rewards with transactions')
    ).toBeVisible();
    expect(
      screen.getByText(
        /Rewards cannot be withdrawn while a wallet's voting power is not delegated/
      )
    ).toBeVisible();
  });

  it('shows the switch on by default and switches it off through the action', () => {
    const { container, onToggleWithdrawRewardsDefault } = renderSettings(true);
    const [, rewards] = switches(container);
    expect(rewards).toBeChecked();
    fireEvent.click(rewards.parentElement);
    expect(onToggleWithdrawRewardsDefault).toHaveBeenCalledWith(false);
  });

  it('switches it on from off', () => {
    const { container, onToggleWithdrawRewardsDefault } = renderSettings(false);
    const [, rewards] = switches(container);
    expect(rewards).not.toBeChecked();
    fireEvent.click(rewards.parentElement);
    expect(onToggleWithdrawRewardsDefault).toHaveBeenCalledWith(true);
  });
});
