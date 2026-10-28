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
import { messages as errorMessages } from '../../../api/errors';
import UndelegateWalletConfirmationDialog from './UndelegateWalletConfirmationDialog';

jest.mock('../../widgets/Dialog', () => ({
  __esModule: true,
  default: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
}));

const renderDialog = (error: unknown) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <UndelegateWalletConfirmationDialog
          selectedWallet={null}
          stakePoolName="Pool"
          stakePoolTicker="POOL"
          onConfirm={jest.fn()}
          onCancel={jest.fn()}
          onExternalLinkClick={jest.fn()}
          isSubmitting={false}
          error={error as never}
          fees={null}
          hwDeviceStatus={'ready' as never}
          isTrezor={false}
        />
      </IntlProvider>
    </ThemeProvider>
  );

describe('UndelegateWalletConfirmationDialog', () => {
  afterEach(cleanup);

  it('renders the withdrawal message as text with a link to the delegation form', () => {
    renderDialog({
      ...errorMessages.withdrawalNotPossibleWithoutVote,
      values: {},
    });
    expect(
      screen.getByText(
        /This transaction withdraws this wallet's staking rewards\./
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/<a href/)).not.toBeInTheDocument();
    expect(
      screen.getByRole('link', { name: 'Governance Center' })
    ).toHaveAttribute('href', '#/governance/delegate');
  });
});
