import React from 'react';
import { Provider } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import type { Network } from '../../../../common/types/environment.types';
import translations from '../../i18n/locales/en-US.json';
import DappCatalogPage from './DappCatalogPage';

jest.mock(
  '../../components/dapp/DappCatalog',
  () =>
    function DappCatalog(props: {
      entries: readonly { id: string; name: string; description: string }[];
      beforeEntries: React.ReactNode;
      onLaunch: (id: string) => void;
    }) {
      return (
        <>
          {props.beforeEntries}
          {props.entries.map((entry) => (
            <button
              key={entry.id}
              type="button"
              onClick={() => props.onLaunch(entry.id)}
            >
              {entry.name}:{entry.description}
            </button>
          ))}
        </>
      );
    }
);

describe('DappCatalogPage', () => {
  const originalNetwork = global.environment.network;
  const originalIsFlight = global.isFlight;

  afterEach(() => {
    global.environment.network = originalNetwork;
    global.isFlight = originalIsFlight;
  });

  const renderPage = (network: Network) => {
    global.environment.network = network;
    global.isFlight = false;
    const launch = jest.fn();
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <Provider
          stores={{
            dapp: {
              catalogAvailable: true,
              ready: true,
              guestOpen: false,
              isLaunching: false,
              launch,
              close: jest.fn(),
            },
            collateral: {
              refresh: jest.fn(),
              snapshot: undefined,
              isLoading: false,
              actionFailed: false,
              prepare: jest.fn(),
              cancelPreparation: jest.fn(),
              clear: jest.fn(),
              repair: jest.fn(),
            },
            wallets: { activeDappWallet: { id: 'wallet-a' } },
            networkStatus: { isConnected: true, isSynced: true },
          }}
        >
          <DappCatalogPage />
        </Provider>
      </IntlProvider>
    );
    return launch;
  };

  it('shows Mainnet entries and launches unfrack.it by opaque ID', () => {
    const launch = renderPage('mainnet');
    expect(screen.getByText(/Liqwid Finance/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /unfrack\.it/ }));
    expect(launch).toHaveBeenCalledWith('unfrack-it', 'unfrack.it');
  });

  it('shows only unfrack.it on Preprod', () => {
    renderPage('preprod');
    expect(screen.queryByText(/Liqwid Finance/)).not.toBeInTheDocument();
    expect(screen.getByText(/unfrack\.it/)).toBeInTheDocument();
  });
});
