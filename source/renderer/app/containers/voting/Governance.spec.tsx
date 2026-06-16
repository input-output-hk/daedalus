import React from 'react';
import { Provider } from 'mobx-react';
import { Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { ROUTES } from '../../routes-config';
import Governance from './Governance';

// Keep the harness light: render children straight through and expose the
// nav items as plain buttons that forward onNavItemClick.
jest.mock('../MainLayout', () => {
  return function MainLayoutMock(props: { children?: React.ReactNode }) {
    return <div>{props.children}</div>;
  };
});

jest.mock('../../components/navigation/Navigation', () => {
  return function NavigationMock(props: {
    items: Array<{ id: string; label: string }>;
    onNavItemClick: (id: string) => void;
  }) {
    return (
      <div>
        {props.items.map((item) => (
          <button
            key={item.id}
            type="button"
            onClick={() => props.onNavItemClick(item.id)}
          >
            {item.label}
          </button>
        ))}
      </div>
    );
  };
});

const renderGovernance = (initialPath: string) => {
  const history = createMemoryHistory({ initialEntries: [initialPath] });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = {
    app: {
      currentRoute: initialPath,
    },
  };
  render(
    <Provider stores={stores as any} actions={{} as any}>
      <Router history={history}>
        <IntlProvider locale="en-US" messages={translations}>
          <Governance />
        </IntlProvider>
      </Router>
    </Provider>
  );
  return { history, pushSpy };
};

describe('Governance container — duplicate hash-history push guard', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  it('does NOT push when the current pathname already equals the target route', () => {
    const { pushSpy } = renderGovernance(ROUTES.GOVERNANCE.DREPS);

    // Sub-tab click for the route we are already on.
    screen.getByText('!!!Directory').click();

    expect(pushSpy).not.toHaveBeenCalled();
  });

  it('pushes once when the current pathname differs from the target route (positive control)', () => {
    // Start on the governance root so the Directory sub-tab click is a real
    // navigation. This proves the click wiring fires and the guard is not
    // passing vacuously.
    const { pushSpy } = renderGovernance(ROUTES.GOVERNANCE.ROOT);

    screen.getByText('!!!Directory').click();

    expect(pushSpy).toHaveBeenCalledTimes(1);
    expect(pushSpy).toHaveBeenCalledWith(ROUTES.GOVERNANCE.DREPS);
  });
});
