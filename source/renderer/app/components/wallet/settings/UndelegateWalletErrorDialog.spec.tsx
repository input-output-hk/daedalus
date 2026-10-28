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
import { messages as errorMessages } from '../../../api/errors';
import UndelegateWalletErrorDialog from './UndelegateWalletErrorDialog';

jest.mock('../../widgets/Dialog', () => ({
  __esModule: true,
  default: ({
    title,
    children,
    actions,
  }: {
    title: string;
    children: React.ReactNode;
    actions: Array<{ label: React.ReactNode; onClick: () => void }>;
  }) => (
    <div>
      <h1>{title}</h1>
      {children}
      {actions.map((action, index) => (
        <button key={index} onClick={action.onClick}>
          {action.label}
        </button>
      ))}
    </div>
  ),
}));

describe('UndelegateWalletErrorDialog', () => {
  afterEach(cleanup);

  const renderDialog = () => {
    const onClose = jest.fn();
    render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <UndelegateWalletErrorDialog
            error={
              {
                ...errorMessages.withdrawalNotPossibleWithoutVote,
                values: {},
              } as never
            }
            onClose={onClose}
          />
        </IntlProvider>
      </ThemeProvider>
    );
    return onClose;
  };

  it('shows the undelegation title and the error with its link', () => {
    renderDialog();
    expect(
      screen.getByRole('heading', { name: 'Undelegate' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        /This transaction withdraws this wallet's staking rewards\./
      )
    ).toBeInTheDocument();
    expect(
      screen.getByRole('link', { name: 'Governance Center' })
    ).toHaveAttribute('href', '#/governance/delegate');
  });

  it('calls its handler from Close', () => {
    const onClose = renderDialog();
    fireEvent.click(screen.getByRole('button', { name: 'Close' }));
    expect(onClose).toHaveBeenCalledTimes(1);
  });
});
