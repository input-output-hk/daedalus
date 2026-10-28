import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../../i18n/locales/en-US.json';
import { messages } from '../../../../api/errors';
import { ConfirmationError } from './Error';

const withdrawalError = {
  ...messages.withdrawalNotPossibleWithoutVote,
  values: {},
} as never;

describe('ConfirmationError', () => {
  afterEach(cleanup);

  it('renders nothing without an error', () => {
    const { container } = render(
      <IntlProvider locale="en-US" messages={translations}>
        <ConfirmationError error={null} onExternalLinkClick={jest.fn()} />
      </IntlProvider>
    );
    expect(container).toBeEmptyDOMElement();
  });

  it('renders the withdrawal message as text with a link to the delegation form', () => {
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <ConfirmationError
          error={withdrawalError}
          onExternalLinkClick={jest.fn()}
        />
      </IntlProvider>
    );
    expect(
      screen.getByText(
        /This transaction withdraws this wallet's staking rewards\./
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/<a href/)).not.toBeInTheDocument();
    const link = screen.getByRole('link', { name: 'Governance Center' });
    expect(link).toHaveAttribute('href', '#/governance/delegate');
  });
});
