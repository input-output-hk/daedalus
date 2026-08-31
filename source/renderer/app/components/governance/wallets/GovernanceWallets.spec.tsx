import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import GovernanceWallets from './GovernanceWallets';

describe('GovernanceWallets', () => {
  afterEach(cleanup);

  it('starts delegation with the wallet selected in the Governance Center', () => {
    const onChangeDelegation = jest.fn();

    render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <GovernanceWallets
            wallets={[
              {
                walletId: 'wallet-2',
                walletName: 'Savings',
                currentDRep: null,
                drepEntry: null,
              },
            ]}
            favoriteDRepIds={new Set()}
            onToggleFavorite={jest.fn()}
            onChangeDelegation={onChangeDelegation}
            onViewDetails={jest.fn()}
            onExternalLinkClick={jest.fn()}
          />
        </IntlProvider>
      </ThemeProvider>
    );

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(onChangeDelegation).toHaveBeenCalledWith('wallet-2');
  });
});
