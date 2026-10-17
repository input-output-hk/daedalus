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
  installPending: false,
  installRetryRequired: false,
  canChooseAnotherLocation: false,
  chooseAnotherLocation: jest.fn(),
  pendingChainLocation: null,
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
      rebuildPending: true,
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
    [
      'the rebuild did not delete anything',
      { hasChain: true, rebuildPending: false },
    ],
    [
      'the rebuild failed in a newly chosen location',
      { hasChain: false, rebuildPending: false },
    ],
    [
      'the error did not come from a rebuild',
      { lastErrorRebuild: false, rebuildPending: false },
    ],
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

describe('MithrilSyncContainer after an install did not finish', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorChooseSync.id]: 'Choose how to sync',
    [messages.errorChainRemovedHint.id]: 'Removed; retry or choose.',
    [messages.errorChainRemovedRetryHint.id]: 'Removed; retry.',
    [messages.errorInstallUnfinishedHint.id]: 'Unfinished; retry or choose.',
    [messages.errorInstallUnfinishedRetryHint.id]: 'Unfinished; retry.',
    [messages.decisionChainRemovedNotice.id]: 'Old data removed, sync again.',
    [messages.decisionInstallUnfinishedNotice.id]:
      'Install unfinished, sync again.',
  };

  it('says the install did not finish, not that data was removed, on the decision view after a restart', () => {
    renderContainer(
      makeBackend({
        loadingPhase: 'bootstrap-decision',
        mithrilPhase: null,
        hasChain: false,
        installPending: true,
      }),
      copy
    );
    expect(
      screen.getByText('Install unfinished, sync again.')
    ).toBeInTheDocument();
    expect(screen.queryByText('Old data removed, sync again.')).toBeNull();
  });

  it('says the install did not finish and offers the sync choice instead of skipping after a later error', () => {
    const backend = makeBackend({
      hasChain: false,
      installPending: true,
      lastErrorRebuild: true,
      lastErrorCode: 'PROBE_FAILED',
    });
    renderContainer(backend, copy);
    expect(
      screen.getByText('Unfinished; retry or choose.')
    ).toBeInTheDocument();
    expect(screen.queryByText('Removed; retry or choose.')).toBeNull();
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Choose how to sync' }));
    expect(backend.showBootstrapDecision).toHaveBeenCalledTimes(1);
  });

  it('says the install did not finish and offers only Retry after an install that was not a rebuild failed', () => {
    renderContainer(
      makeBackend({
        lastErrorCode: 'INSTALL_FAILED',
        installRetryRequired: true,
        installPending: true,
      }),
      copy
    );
    expect(screen.getByText('Unfinished; retry.')).toBeInTheDocument();
    expect(screen.queryByText('Removed; retry.')).toBeNull();
    expect(screen.getAllByRole('button')).toHaveLength(1);
  });
});

describe('MithrilSyncContainer after an install failed', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorChooseSync.id]: 'Choose how to sync',
    [messages.errorChainRemovedHint.id]: 'Removed; retry or choose.',
    [messages.errorChainRemovedRetryHint.id]: 'Removed; retry.',
  };
  const installFailed = (overrides = {}) =>
    makeBackend({
      lastErrorCode: 'INSTALL_FAILED',
      installRetryRequired: true,
      ...overrides,
    });

  it('offers only Retry after a partial sync failed to install', () => {
    const backend = installFailed();
    renderContainer(backend, copy);
    expect(screen.getAllByRole('button')).toHaveLength(1);
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    expect(screen.queryByText('Removed; retry.')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(backend.retryMithril).toHaveBeenCalledTimes(1);
    expect(backend.startNode).not.toHaveBeenCalled();
  });

  it('says the old data was removed and offers only Retry after the install of a rebuild failed', () => {
    renderContainer(
      installFailed({ lastErrorRebuild: true, rebuildPending: true }),
      copy
    );
    expect(screen.getByText('Removed; retry.')).toBeInTheDocument();
    expect(screen.queryByText('Removed; retry or choose.')).toBeNull();
    expect(screen.queryByText('Choose how to sync')).toBeNull();
    expect(screen.getAllByRole('button')).toHaveLength(1);
  });

  it('offers only Retry for a later error while the watchdog still waits for an install', () => {
    renderContainer(
      installFailed({ lastErrorCode: 'PROBE_FAILED', lastErrorRebuild: true }),
      copy
    );
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    expect(screen.getAllByRole('button')).toHaveLength(1);
  });
});

describe('MithrilSyncContainer after a partial sync failed to install', () => {
  afterEach(cleanup);

  const confirmation = () =>
    makeBackend({
      lastErrorCode: 'INSTALL_FAILED',
      installRetryRequired: true,
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
    renderContainer(
      makeBackend({
        lastErrorCode: 'INSTALL_FAILED',
        installRetryRequired: true,
      }),
      { [messages.errorRetry.id]: 'Retry' }
    );
    expect(screen.getByRole('button', { name: 'Retry' })).toBeInTheDocument();
  });
});

describe('MithrilSyncContainer location choice', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorChooseLocation.id]: 'Choose another location',
    [chainDamageMessages.confirmBody.id]: 'Delete and download.',
    [chainDamageMessages.confirmBodyInLocation.id]:
      'Keep it in {location}, delete what is there and download.',
  };

  it('offers another location beside Retry when the error allows it', () => {
    const backend = makeBackend({
      lastErrorCode: 'INSUFFICIENT_SPACE',
      lastErrorRebuild: true,
      canChooseAnotherLocation: true,
    });
    renderContainer(backend, copy);
    fireEvent.click(
      screen.getByRole('button', { name: 'Choose another location' })
    );
    expect(backend.chooseAnotherLocation).toHaveBeenCalledTimes(1);
    expect(screen.getByRole('button', { name: 'Retry' })).toBeInTheDocument();
  });

  it('offers no location choice for other errors', () => {
    renderContainer(makeBackend(), copy);
    expect(screen.queryByText('Choose another location')).toBeNull();
  });

  it('offers the location choice but no skip while the watchdog waits for an install', () => {
    renderContainer(
      makeBackend({
        lastErrorCode: 'INSUFFICIENT_SPACE',
        lastErrorRebuild: true,
        installRetryRequired: true,
        canChooseAnotherLocation: true,
      }),
      copy
    );
    expect(
      screen.getByRole('button', { name: 'Choose another location' })
    ).toBeInTheDocument();
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
  });

  it('names the picked location in the rebuild confirmation', () => {
    renderContainer(
      makeBackend({
        lastErrorCode: 'INSUFFICIENT_SPACE',
        lastErrorRebuild: true,
        _rebuildConfirmationRequested: true,
        pendingChainLocation: 'D:\\Cardano',
      }),
      copy
    );
    expect(
      screen.getByText(
        'Keep it in D:\\Cardano, delete what is there and download.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('Delete and download.')).toBeNull();
  });
});
