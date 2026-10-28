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
import VotingRegistrationStepsRegister from './VotingRegistrationStepsRegister';

jest.mock('./widgets/VotingRegistrationDialog', () => ({
  __esModule: true,
  default: ({ children }: { children: React.ReactNode }) => (
    <div>{children}</div>
  ),
}));

describe('VotingRegistrationStepsRegister', () => {
  afterEach(cleanup);

  it('renders the withdrawal message as text with a link to the delegation form', () => {
    render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <VotingRegistrationStepsRegister
            stepsList={[]}
            activeStep={0}
            transactionFee={null}
            transactionError={
              {
                ...errorMessages.withdrawalNotPossibleWithoutVote,
                values: {},
              } as never
            }
            hwDeviceStatus={'ready' as never}
            selectedWallet={null}
            isTrezor={false}
            isHardwareWallet={false}
            isSubmitting={false}
            onConfirm={jest.fn()}
            onClose={jest.fn()}
            onBack={jest.fn()}
            onExternalLinkClick={jest.fn()}
          />
        </IntlProvider>
      </ThemeProvider>
    );
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
