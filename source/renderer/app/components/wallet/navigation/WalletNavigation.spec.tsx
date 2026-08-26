import React from 'react';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { IntlProvider } from 'react-intl';
import translations from '../../../i18n/locales/en-US.json';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import WalletNavigation from './WalletNavigation';

const renderNavigation = (isLegacy: boolean) =>
  render(
    <StoryDecorator>
      <IntlProvider locale="en-US" messages={translations}>
        <WalletNavigation
          activeItem="summary"
          isActiveNavItem={() => false}
          isLegacy={isLegacy}
          onNavItemClick={jest.fn()}
        />
      </IntlProvider>
    </StoryDecorator>
  );

describe('WalletNavigation dApp item', () => {
  afterEach(cleanup);

  it('shows DApps for Shelley wallets', () => {
    renderNavigation(false);

    expect(screen.getByText('DApps')).toBeVisible();
  });

  it('hides DApps for Byron wallets', () => {
    renderNavigation(true);

    expect(screen.queryByText('DApps')).not.toBeInTheDocument();
  });
});
