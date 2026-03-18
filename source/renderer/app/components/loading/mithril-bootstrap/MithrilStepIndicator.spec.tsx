import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import MithrilStepIndicator from './MithrilStepIndicator';

describe('MithrilStepIndicator', () => {
  const renderComponent = (
    status: import('../../../../../common/types/mithril-bootstrap.types').MithrilBootstrapStatus = 'finalizing'
  ) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilStepIndicator status={status} />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the visible Mithril flow as preparing, downloading, and finalizing', () => {
    renderComponent();

    expect(screen.getByText(/preparing/i)).toBeInTheDocument();
    expect(screen.getByText(/downloading/i)).toBeInTheDocument();
    expect(screen.getByText(/finalizing/i)).toBeInTheDocument();
    expect(screen.queryByText(/installing/i)).not.toBeInTheDocument();
  });

  it('maps unpacking status into the visible finalizing step', () => {
    renderComponent('unpacking');

    expect(screen.queryByText(/92.5%/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/installing/i)).not.toBeInTheDocument();
    expect(screen.getByText(/finalizing/i)).toBeInTheDocument();
  });
});
