import React from 'react';
import BigNumber from 'bignumber.js';
import { Provider } from 'mobx-react';
import { Router } from 'react-router-dom';
import { createMemoryHistory } from 'history';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';
import { ROUTES } from '../../routes-config';
import GovernanceStore from '../../stores/GovernanceStore';
import { normalizeDRepIdentity } from '../../utils/governance/normalizeDRepIdentity';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../features/discreet-mode';
import GovernanceWalletsPage from './GovernanceWalletsPage';

jest.mock('../../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const DREP_ID = 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const drep = normalizeDRepIdentity(DREP_ID)!;
const voting = { kind: 'drep' as const, drep, source: 'onchain' as const };

const wallet = (ledgerVote: { status: string; voting?: unknown }) => ({
  id: 'wallet-1',
  name: 'Savings',
  isLegacy: false,
  votingTarget: voting,
  ledgerVote,
  heldReward: new BigNumber(0),
});

const flush = () => act(async () => {});

const renderPage = async (
  ledgerVote: { status: string; voting?: unknown },
  getDRep: jest.Mock
) => {
  const api = {
    ada: { getDRep },
    localStorage: {
      getDRepFavorites: jest.fn().mockResolvedValue([]),
      setDRepFavorites: jest.fn().mockResolvedValue(undefined),
    },
  };
  const governance = new GovernanceStore(api as any, {} as any, {} as any);
  const wallets = { allWallets: [wallet(ledgerVote)] };
  governance.configure({
    networkStatus: { localTip: { epoch: 500 }, isNodeInSync: true },
    router: { location: { pathname: '/governance' } },
    wallets,
  } as any);
  const history = createMemoryHistory({
    initialEntries: [ROUTES.GOVERNANCE.DASHBOARD],
  });
  const stores = {
    governance,
    wallets,
    app: { openExternalLink: jest.fn() },
  } as any;
  const view = render(
    <Provider stores={stores}>
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <BrowserLocalStorageBridge>
            <DiscreetModeFeatureProvider>
              <Router history={history}>
                <GovernanceWalletsPage />
              </Router>
            </DiscreetModeFeatureProvider>
          </BrowserLocalStorageBridge>
        </IntlProvider>
      </ThemeProvider>
    </Provider>
  );
  await flush();
  return { governance, history, getDRep, ...view };
};

const notFound = () =>
  jest.fn().mockRejectedValue({ code: 'not_found', message: 'gone' });

describe('GovernanceWalletsPage', () => {
  afterEach(cleanup);

  it('reads the retirement from the ledger field once the registry answers 404', async () => {
    await renderPage({ status: 'not_delegated' }, notFound());

    expect(screen.getByText('DRep retired')).toBeInTheDocument();
    expect(screen.queryByText('Loading DRep…')).not.toBeInTheDocument();
  });

  it('says the DRep is not in the registry where the wallet reports no ledger field', async () => {
    await renderPage({ status: 'unknown' }, notFound());

    expect(screen.getByText('Not in the DRep registry')).toBeInTheDocument();
    expect(screen.queryByText('Loading DRep…')).not.toBeInTheDocument();
  });

  it('says the DRep data is unavailable after a failed lookup', async () => {
    await renderPage(
      { status: 'delegated', voting } as any,
      jest.fn().mockRejectedValue(new Error('network'))
    );

    expect(screen.getByText('DRep data unavailable')).toBeInTheDocument();
    expect(screen.queryByText('Loading DRep…')).not.toBeInTheDocument();
  });

  it('adds no lookup when the page updates', async () => {
    const { getDRep, governance } = await renderPage(
      { status: 'not_delegated' },
      notFound()
    );
    for (let pass = 0; pass < 10; pass += 1) {
      await act(async () => {
        governance.setDelegationNavState({ selectedWalletId: `w${pass}` });
      });
    }
    expect(getDRep).toHaveBeenCalledTimes(1);
  });

  it('opens the delegation form with the wallet selected', async () => {
    const { governance, history } = await renderPage(
      { status: 'not_delegated' },
      notFound()
    );

    fireEvent.click(screen.getAllByRole('button', { name: 'Delegate' })[0]);

    expect(governance.delegationNavState).toMatchObject({
      selectedWalletId: 'wallet-1',
    });
    expect(history.location.pathname).toBe(ROUTES.GOVERNANCE.DELEGATE);
  });
});
