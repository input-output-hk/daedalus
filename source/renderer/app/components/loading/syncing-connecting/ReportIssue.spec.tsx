import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../../i18n/locales/en-US.json';
import ReportIssue from './ReportIssue';

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <ReportIssue
        onIssueClick={jest.fn()}
        onOpenExternalLink={jest.fn()}
        onDownloadLogs={jest.fn()}
        disableDownloadLogs={false}
        {...overrides}
      />
    </IntlProvider>
  );

describe('ReportIssue', () => {
  afterEach(cleanup);

  it('offers Retry and Download logs when given a retry handler', () => {
    const onRetry = jest.fn();
    renderComponent({ onRetry });
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(onRetry).toHaveBeenCalledTimes(1);
    expect(screen.getByText('Download logs')).toBeInTheDocument();
  });

  it('shows no Retry button without a retry handler', () => {
    renderComponent();
    expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
  });
});
