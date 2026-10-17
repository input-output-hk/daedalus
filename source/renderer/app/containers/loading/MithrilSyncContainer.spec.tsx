import React from 'react';
import { IntlProvider } from 'react-intl';
import { Provider } from 'mobx-react';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';

import globalMessages from '../../i18n/global-messages';
import messages from '../../components/loading/mithril/MithrilBootstrap.messages';
import chainDamageMessages from '../../components/loading/chain-damage/ChainDamage.messages';
import MithrilSyncContainer from './MithrilSyncContainer';

const makeBackend = (overrides = {}) => ({
  loadingPhase: 'mithril-syncing',
  mithrilPhase: 'error',
  mithrilProgress: null,
  lastError: 'could not delete the files',
  lastErrorCode: 'WIPE_FAILED',
  startMithril: jest.fn(),
  retryMithril: jest.fn(),
  startNode: jest.fn(),
  showBootstrapDecision: jest.fn(),
  rebuildChain: jest.fn(),
  cancelRebuildConfirmation: jest.fn(),
  _rebuildConfirmationRequested: false,
  hasChain: true,
  lastErrorRebuild: false,
  rebuildPending: false,
  cancelMithril: jest.fn(),
  ...overrides,
});

const renderContainer = (
  backend: ReturnType<typeof makeBackend>,
  intlMessages: Record<string, string> = {}
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={intlMessages}>
        <Provider
          stores={{ backend, app: { openExternalLink: jest.fn() } }}
          actions={{}}
        >
          <MithrilSyncContainer />
        </Provider>
      </IntlProvider>
    </ThemeProvider>
  );

describe('MithrilSyncContainer error view', () => {
  afterEach(cleanup);

  it('shows the message for the code the watchdog reported', () => {
    renderContainer(makeBackend(), {
      [globalMessages.view.id]: 'View',
      [messages.errorWipeFailedHint.id]: 'Close other programs and retry.',
    });
    expect(
      screen.getByText('Close other programs and retry.')
    ).toBeInTheDocument();
    fireEvent.click(screen.getByText('View'));
    expect(screen.getByText('WIPE_FAILED')).toBeInTheDocument();
  });

  it('labels the buttons from the locale and retries or starts the node from them', () => {
    const backend = makeBackend();
    renderContainer(backend, {
      [messages.errorRetry.id]: 'Translated retry',
      [messages.errorSkip.id]: 'Translated skip',
    });
    fireEvent.click(screen.getByRole('button', { name: 'Translated retry' }));
    expect(backend.retryMithril).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole('button', { name: 'Translated skip' }));
    expect(backend.startNode).toHaveBeenCalledTimes(1);
  });

  it('falls back to the generic code when the watchdog reported none', () => {
    renderContainer(makeBackend({ lastErrorCode: null }), {
      [globalMessages.view.id]: 'View',
    });
    fireEvent.click(screen.getByText('View'));
    expect(screen.getByText('mithril_error')).toBeInTheDocument();
  });
});

describe('MithrilSyncContainer after a rebuild removed the chain data', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorChooseSync.id]: 'Choose how to sync',
    [messages.errorChainRemovedHint.id]: 'The old blockchain data was removed.',
    [messages.decisionChainRemovedNotice.id]: 'Old data removed, sync again.',
  };
  const removed = () =>
    makeBackend({
      hasChain: false,
      lastErrorRebuild: true,
      lastErrorCode: 'DOWNLOAD_FAILED',
    });

  it('says the old data was removed and offers the sync choice instead of skipping', () => {
    const backend = removed();
    renderContainer(backend, copy);
    expect(
      screen.getByText('The old blockchain data was removed.')
    ).toBeInTheDocument();
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Choose how to sync' }));
    expect(backend.showBootstrapDecision).toHaveBeenCalledTimes(1);
    expect(backend.startNode).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(backend.retryMithril).toHaveBeenCalledTimes(1);
  });

  it.each([
    ['the rebuild did not delete anything', { hasChain: true }],
    ['the error did not come from a rebuild', { lastErrorRebuild: false }],
  ])('keeps the skip action when %s', (_name, overrides) => {
    const backend = { ...removed(), ...overrides };
    renderContainer(backend, copy);
    expect(
      screen.queryByText('The old blockchain data was removed.')
    ).toBeNull();
    fireEvent.click(
      screen.getByRole('button', { name: 'Skip to blockchain sync' })
    );
    expect(backend.startNode).toHaveBeenCalledTimes(1);
  });

  it('notes the removal on the decision view after a restart mid-rebuild', () => {
    renderContainer(
      makeBackend({
        loadingPhase: 'bootstrap-decision',
        mithrilPhase: null,
        hasChain: false,
        rebuildPending: true,
      }),
      copy
    );
    expect(
      screen.getByText('Old data removed, sync again.')
    ).toBeInTheDocument();
  });

  it('shows no note on the decision view of a first run', () => {
    renderContainer(
      makeBackend({
        loadingPhase: 'bootstrap-decision',
        mithrilPhase: null,
        hasChain: false,
      }),
      copy
    );
    expect(screen.queryByText('Old data removed, sync again.')).toBeNull();
  });
});

describe('MithrilSyncContainer after a partial sync failed to install', () => {
  afterEach(cleanup);

  const confirmation = () =>
    makeBackend({
      lastErrorCode: 'INSTALL_FAILED',
      _rebuildConfirmationRequested: true,
    });

  it('shows the rebuild confirmation in place of the error once Retry asked for it', () => {
    renderContainer(confirmation(), {
      [chainDamageMessages.confirmTitle.id]: 'Rebuild blockchain data?',
      [messages.errorRetry.id]: 'Retry',
    });
    expect(
      screen.getByRole('heading', { name: 'Rebuild blockchain data?' })
    ).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
  });

  it('rebuilds only from the confirmation and returns to the error from Back', () => {
    const backend = confirmation();
    renderContainer(backend, {
      [chainDamageMessages.confirmBack.id]: 'Back',
      [chainDamageMessages.confirmRebuild.id]: 'Rebuild',
    });
    fireEvent.click(screen.getByRole('button', { name: 'Back' }));
    expect(backend.cancelRebuildConfirmation).toHaveBeenCalledTimes(1);
    expect(backend.rebuildChain).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Rebuild' }));
    expect(backend.rebuildChain).toHaveBeenCalledTimes(1);
  });

  it('shows the error view while no confirmation was asked for', () => {
    renderContainer(makeBackend({ lastErrorCode: 'INSTALL_FAILED' }), {
      [messages.errorRetry.id]: 'Retry',
    });
    expect(screen.getByRole('button', { name: 'Retry' })).toBeInTheDocument();
  });
});
