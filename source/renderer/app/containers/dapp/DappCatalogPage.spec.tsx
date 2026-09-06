import React from 'react';
import { Provider } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { fireEvent, render, screen } from '@testing-library/react';
import DappCatalogPage from './DappCatalogPage';

jest.mock('../../../../common/config/dappCatalog', () => ({
  dappCatalogPresentation: [
    {
      id: 'catalog-id',
      nameMessageId: 'catalog.name',
      descriptionMessageId: 'catalog.description',
      iconAsset: 'cardano',
    },
  ],
}));

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
          <button
            type="button"
            onClick={() => props.onLaunch(props.entries[0].id)}
          >
            {props.entries[0].name}:{props.entries[0].description}
          </button>
        </>
      );
    }
);

describe('DappCatalogPage', () => {
  it('localizes presentation entries and launches with an opaque ID', () => {
    const launch = jest.fn();
    const refresh = jest.fn();
    render(
      <IntlProvider
        locale="en"
        messages={{
          'catalog.name': 'Catalog name',
          'catalog.description': 'Catalog description',
        }}
      >
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
              refresh,
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

    fireEvent.click(screen.getByRole('button', { name: /Catalog name/ }));
    expect(launch).toHaveBeenCalledWith('catalog-id', 'Catalog name');
    expect(refresh).toHaveBeenCalledTimes(1);
  });
});
