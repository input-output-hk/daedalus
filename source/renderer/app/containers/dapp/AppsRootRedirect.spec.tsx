import React from 'react';
import { Provider } from 'mobx-react';
import { Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { render } from '@testing-library/react';
import { ROUTES } from '../../routes-config';
import AppsRootRedirect from './AppsRootRedirect';

describe('AppsRootRedirect', () => {
  it('opens Apps for an active restoring Shelley wallet', () => {
    const wallet = { id: 'wallet-a', isLegacy: false, isRestoring: true };
    const history = createMemoryHistory({ initialEntries: [ROUTES.APPS.ROOT] });

    render(
      <Provider
        stores={{
          wallets: {
            active: wallet,
            activeDappWallet: null,
            eligibleDappWallets: [],
            allWallets: [wallet],
          },
        }}
      >
        <Router history={history}>
          <AppsRootRedirect />
        </Router>
      </Provider>
    );

    expect(history.location.pathname).toBe('/apps/wallet-a');
  });
});
