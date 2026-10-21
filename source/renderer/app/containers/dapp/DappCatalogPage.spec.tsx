import React from 'react';
import { Provider } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { observable, runInAction } from 'mobx';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import type { Network } from '../../../../common/types/environment.types';
import translations from '../../i18n/locales/en-US.json';
import DappCatalogPage from './DappCatalogPage';
import CollateralStore from '../../stores/CollateralStore';
import { dappCollateralChannel } from '../../ipc/collateral';
import type { CollateralSnapshot } from '../../../../common/types/collateral.types';

jest.mock('../../ipc/collateral', () => ({
  dappCollateralChannel: { request: jest.fn() },
}));

jest.mock(
  '../../components/dapp/DappCatalog',
  () =>
    function DappCatalog(props: {
      entries: readonly { id: string; name: string; description: string }[];
      beforeEntries: React.ReactNode;
      launchFailed: boolean;
      onLaunch: (id: string) => void;
    }) {
      return (
        <>
          {props.beforeEntries}
          {props.launchFailed && <p role="alert">Catalog launch failed</p>}
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
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  const originalNetwork = global.environment.network;
  const originalIsFlight = global.isFlight;

  afterEach(() => {
    global.environment.network = originalNetwork;
    global.isFlight = originalIsFlight;
  });

  const renderPage = (
    network: Network,
    catalogLaunchFailed = false,
    overrides: Record<string, unknown> = {}
  ) => {
    global.environment.network = network;
    global.isFlight = false;
    const launch = jest.fn();
    const view = render(
      <IntlProvider locale="en-US" messages={translations}>
        <Provider
          stores={{
            dapp: {
              catalogAvailable: true,
              ready: true,
              guestOpen: false,
              launchingCatalogId: null,
              catalogLaunchFailed,
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
            ...overrides,
          }}
        >
          <DappCatalogPage />
        </Provider>
      </IntlProvider>
    );
    return { launch, ...view };
  };

  it('shows Mainnet entries and launches unfrack.it by opaque ID', () => {
    const { launch } = renderPage('mainnet');
    expect(screen.getByText(/Liqwid Finance/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: /unfrack\.it/ }));
    expect(launch).toHaveBeenCalledWith('unfrack-it', 'unfrack.it');
  });

  it('shows only unfrack.it on Preprod', () => {
    renderPage('preprod');
    expect(screen.queryByText(/Liqwid Finance/)).not.toBeInTheDocument();
    expect(screen.getByText(/unfrack\.it/)).toBeInTheDocument();
  });

  it('passes the catalog launch failure state to the catalog', () => {
    renderPage('mainnet', true);

    expect(screen.getByRole('alert')).toHaveTextContent(
      'Catalog launch failed'
    );
  });

  describe('collateral preparation confirmation', () => {
    const request = dappCollateralChannel.request as jest.MockedFunction<
      typeof dappCollateralChannel.request
    >;
    const preparation = {
      operationId: '11111111-1111-4111-8111-111111111111',
      walletId: 'ab'.repeat(20),
      networkGenesis: 'cd'.repeat(32),
    };
    const pending: CollateralSnapshot = {
      corrupt: false,
      preparation,
      preference: {
        schemaVersion: 1,
        walletId: preparation.walletId,
        networkGenesis: preparation.networkGenesis,
        targetLovelace: '5000000',
        preferredInputs: [],
        generation: 0,
        state: 'preparing',
      },
    };
    const confirmed: CollateralSnapshot = {
      corrupt: false,
      preference: {
        ...pending.preference,
        state: 'ready',
        generation: 1,
        preferredInputs: [{ transactionId: '12'.repeat(32), index: 0 }],
      },
    };
    const advance = async (milliseconds: number) => {
      await act(async () => {
        jest.advanceTimersByTime(milliseconds);
      });
    };
    const mountPreparing = async () => {
      const collateral = new CollateralStore(
        null as never,
        null as never,
        null as never
      );
      collateral.preparation = preparation;
      const networkStatus = observable({ isConnected: true, isSynced: true });
      const view = renderPage('preprod', false, {
        collateral,
        networkStatus,
        wallets: { activeDappWallet: { id: preparation.walletId } },
      });
      await act(() => Promise.resolve());
      return { ...view, collateral, networkStatus };
    };

    beforeEach(() => {
      jest.useFakeTimers();
      request.mockReset();
      request.mockResolvedValue(pending);
    });

    afterEach(() => {
      cleanup();
      jest.useRealTimers();
    });

    it('adopts confirmed collateral and clears the preparation notice without navigating away', async () => {
      const { collateral } = await mountPreparing();
      expect(screen.getByRole('status')).toBeInTheDocument();
      expect(collateral.state).toBe('preparing');

      request.mockResolvedValue(confirmed);
      await advance(5_000);

      expect(collateral.state).toBe('ready');
      expect(collateral.snapshot?.preference.preferredInputs).toEqual(
        confirmed.preference.preferredInputs
      );
      expect(collateral.preparation).toBeUndefined();
      expect(screen.queryByRole('status')).not.toBeInTheDocument();
      request.mockClear();
      await advance(15_000);
      expect(request).not.toHaveBeenCalled();
    });

    it('allows a slow confirmation lookup to finish without superseding it', async () => {
      const { collateral } = await mountPreparing();
      let resolve!: (snapshot: CollateralSnapshot) => void;
      request.mockReturnValue(
        new Promise((complete) => {
          resolve = complete;
        })
      );
      request.mockClear();
      await advance(5_000);
      expect(collateral.isLoading).toBe(true);
      await advance(10_000);
      expect(request).toHaveBeenCalledTimes(1);

      await act(async () => resolve(confirmed));
      expect(collateral.state).toBe('ready');
      expect(screen.queryByRole('status')).not.toBeInTheDocument();
    });

    it.each(['isConnected', 'isSynced'] as const)(
      'pauses when %s is false, resumes on recovery, and stops on unmount',
      async (flag) => {
        const { collateral, networkStatus, unmount } = await mountPreparing();
        act(() => {
          runInAction(() => {
            networkStatus[flag] = false;
          });
        });
        request.mockClear();
        await advance(10_000);
        expect(request).not.toHaveBeenCalled();

        await act(async () => {
          runInAction(() => {
            networkStatus[flag] = true;
          });
        });
        expect(collateral.state).toBe('preparing');
        expect(request).toHaveBeenCalledTimes(1);
        request.mockClear();
        await advance(5_000);
        expect(request).toHaveBeenCalledTimes(1);
        request.mockResolvedValue(confirmed);
        unmount();
        request.mockClear();
        await advance(10_000);
        expect(request).not.toHaveBeenCalled();
        expect(collateral.state).toBe('preparing');
      }
    );
  });
});
