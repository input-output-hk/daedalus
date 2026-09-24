import React from 'react';
import { IntlProvider } from 'react-intl';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import AnalyticsConsentForm from './AnalyticsConsentForm';
import { analyticsConsent } from '../../../ipc/ariadneAnalytics';

jest.mock('../../../ipc/ariadneAnalytics', () => ({
  analyticsConsent: jest.fn(),
}));
jest.mock('react-polymorph/lib/components/Button', () => ({
  Button: ({ label, disabled, onClick }) => (
    <button type="button" disabled={disabled} onClick={onClick}>
      {label}
    </button>
  ),
}));
jest.mock('./CollectedDataOverview', () => ({
  CollectedDataOverview: () => <p>Data inventory</p>,
}));

test('consent is an explicit Ariadne decision; disabled configuration cannot be accepted; save failures are visible', async () => {
  const submit = jest.fn();
  (analyticsConsent as jest.Mock).mockResolvedValue({ enabled: false });
  const { unmount } = render(
    <IntlProvider locale="en">
      <AnalyticsConsentForm
        loading={false}
        onSubmit={submit}
        onExternalLinkClick={jest.fn()}
      />
    </IntlProvider>
  );
  const allow = screen.getByRole('button', { name: /Allow Ariadne analytics/ });
  await waitFor(() =>
    expect(screen.getByText(/Collection is disabled/)).toBeTruthy()
  );
  expect(allow.hasAttribute('disabled')).toBe(true);
  fireEvent.click(allow);
  expect(submit).not.toHaveBeenCalled();
  expect(screen.getByText(/provisional notice v2/)).toBeTruthy();
  expect(
    screen.getByText(/does not delete events already received/)
  ).toBeTruthy();
  unmount();
  (analyticsConsent as jest.Mock).mockResolvedValue({ enabled: true });
  render(
    <IntlProvider locale="en">
      <AnalyticsConsentForm
        loading={false}
        saveFailed
        onSubmit={submit}
        onExternalLinkClick={jest.fn()}
      />
    </IntlProvider>
  );
  const accepted = screen.getByRole('button', {
    name: /Allow Ariadne analytics/,
  });
  await waitFor(() => expect(accepted.hasAttribute('disabled')).toBe(false));
  fireEvent.click(accepted);
  expect(submit).toHaveBeenLastCalledWith(true);
  fireEvent.click(screen.getByRole('button', { name: /Keep off/ }));
  expect(submit).toHaveBeenLastCalledWith(false);
  expect(screen.getByRole('alert').textContent).toContain('could not be saved');
});
