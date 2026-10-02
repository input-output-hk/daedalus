import React from 'react';
import { observable, runInAction } from 'mobx';
import { act, render, screen } from '@testing-library/react';
import { AnalyticsConsentPage } from './AnalyticsConsentPage';
import { useStores } from '../../hooks/useStores';

jest.mock('../../hooks/useStores', () => ({ useStores: jest.fn() }));
jest.mock('../../hooks/useActions', () => ({ useActions: () => ({}) }));
jest.mock(
  '../../components/layout/TopBar',
  () =>
    function () {
      return null;
    }
);
jest.mock(
  '../../components/layout/TopBarLayout',
  () =>
    function ({ children }) {
      return <div>{children}</div>;
    }
);
jest.mock(
  '../../components/profile/analytics/AnalyticsConsentForm',
  () =>
    function ({ available, loading, saveFailed }) {
      return (
        <div data-testid="consent-state">
          {JSON.stringify({ available, loading, saveFailed })}
        </div>
      );
    }
);

test('the consent container reacts to the shared store without remounting', () => {
  const consent = observable({
    view: { enabled: false },
    saving: false,
    saveFailed: false,
  });
  (useStores as jest.Mock).mockReturnValue({
    networkStatus: {},
    analyticsConsent: consent,
  });
  render(<AnalyticsConsentPage />);
  expect(screen.getByTestId('consent-state').textContent).toBe(
    JSON.stringify({ available: false, loading: false, saveFailed: false })
  );
  act(() =>
    runInAction(() => {
      consent.view.enabled = true;
      consent.saving = true;
      consent.saveFailed = true;
    })
  );
  expect(screen.getByTestId('consent-state').textContent).toBe(
    JSON.stringify({ available: true, loading: true, saveFailed: true })
  );
});
