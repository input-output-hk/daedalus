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

// By default, a partial sync that could not reach the Mithril service: the
// watchdog started the node again on the chain it had
const makeBackend = (overrides = {}) => ({
  loadingPhase: 'mithril-syncing',
  mithrilPhase: 'error',
  mithrilProgress: null,
  lastError: 'could not reach the aggregator',
  lastErrorCode: 'PROBE_FAILED',
  startMithril: jest.fn(),
  retryMithril: jest.fn(),
  replayMithrilLedger: jest.fn(),
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
  rebuildStarting: false,
  ...overrides,
});

// The labels of the actions the error view offers, by accessible name. The
// technical details toggle, named View in every render below, is a button in
// the section header and not one of them.
const DETAILS_TOGGLE = 'View';
const offeredActions = () =>
  screen
    .queryAllByRole('button')
    .map((button) => button.textContent)
    .filter((name) => name !== DETAILS_TOGGLE);

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
      <IntlProvider
        locale="en-US"
        messages={{ [globalMessages.view.id]: DETAILS_TOGGLE, ...intlMessages }}
      >
        <Provider
          stores={{ backend, app: { openExternalLink: jest.fn() } }}
          actions={{}}
        >
          <MithrilSyncContainer />
        </Provider>
      </IntlProvider>
    </ThemeProvider>
  );

describe('MithrilSyncContainer ledger replay recovery', () => {
  afterEach(cleanup);

  it.each([false, true])(
    'offers replay after conversion fails (rebuild: %s)',
    (rebuildPending) => {
      const backend = makeBackend({
        lastErrorCode: 'PARTIAL_SYNC_CONVERSION_FAILED',
        lastError: 'converter failed',
        rebuildPending,
        lastErrorRebuild: rebuildPending,
      });
      renderContainer(backend, {
        [messages.errorReplayLedger.id]: 'Keep blocks and replay ledger',
        [messages.errorReplayLedgerHint.id]:
          'Replay uses the downloaded blocks.',
      });
      expect(
        screen.getByText('Replay uses the downloaded blocks.')
      ).toBeInTheDocument();
      fireEvent.click(
        screen.getByRole('button', { name: 'Keep blocks and replay ledger' })
      );
      expect(backend.replayMithrilLedger).toHaveBeenCalledTimes(1);
      expect(backend.retryMithril).not.toHaveBeenCalled();
    }
  );

  it.each([
    'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
    'PARTIAL_SYNC_STAGED_DB_INVALID',
    'INSTALL_FAILED',
  ])('does not offer replay for %s', (lastErrorCode) => {
    renderContainer(makeBackend({ lastErrorCode }), {
      [messages.errorReplayLedger.id]: 'Keep blocks and replay ledger',
    });
    expect(
      screen.queryByRole('button', { name: 'Keep blocks and replay ledger' })
    ).toBeNull();
  });
});

describe('MithrilSyncContainer progress view', () => {
  afterEach(cleanup);

  const waitingLine = 'Downloading will continue when the network returns.';
  const intlMessages = { [messages.progressWaitingForNetwork.id]: waitingLine };

  it('says that the download continues when the network returns while it waits', () => {
    renderContainer(
      makeBackend({
        mithrilPhase: 'downloading',
        lastError: null,
        lastErrorCode: null,
        mithrilWaitingForNetwork: true,
      }),
      intlMessages
    );
    expect(screen.getByText(waitingLine)).toBeInTheDocument();
  });

  it('says nothing about the network while the download goes on', () => {
    renderContainer(
      makeBackend({
        mithrilPhase: 'downloading',
        lastError: null,
        lastErrorCode: null,
        mithrilWaitingForNetwork: false,
      }),
      intlMessages
    );
    expect(screen.queryByText(waitingLine)).not.toBeInTheDocument();
  });
});

describe('MithrilSyncContainer error view', () => {
  afterEach(cleanup);

  it('shows the message for the code the watchdog reported', () => {
    renderContainer(makeBackend(), {
      [globalMessages.view.id]: 'View',
      [messages.errorProbeFailedHint.id]: 'Check your connection and retry.',
    });
    expect(
      screen.getByText('Check your connection and retry.')
    ).toBeInTheDocument();
    fireEvent.click(screen.getByText('View'));
    expect(screen.getByText('PROBE_FAILED')).toBeInTheDocument();
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

describe('MithrilSyncContainer after a rebuild failed or was refused', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorNotNow.id]: 'Not now',
    [messages.errorChooseSync.id]: 'Choose how to sync',
    [messages.errorChooseLocation.id]: 'Choose another location',
    [messages.errorTitle.id]: 'Mithril bootstrap failed',
    [messages.errorRebuildTitle.id]: 'Could not rebuild the blockchain data',
  };

  it.each([
    [
      'had no ancillary verification key',
      'ANCILLARY_VKEY_MISSING',
      ['Retry', 'Not now'],
    ],
    ['could not use the folder', 'INVALID_CHAIN_PATH', ['Retry', 'Not now']],
    [
      'could not reach the Mithril service',
      'PROBE_FAILED',
      ['Retry', 'Not now'],
    ],
    [
      'could not resolve the staging folder',
      'STAGING_FAILED',
      ['Retry', 'Not now'],
    ],
    [
      'found too little free space',
      'INSUFFICIENT_SPACE',
      ['Retry', 'Choose another location', 'Not now'],
    ],
    [
      'found other files in the folder',
      'CHAIN_DIR_NOT_REPLACEABLE',
      ['Retry', 'Choose another location', 'Not now'],
    ],
    ['could not record itself', 'REBUILD_MARKER_FAILED', ['Retry', 'Not now']],
    [
      "found another network's data",
      'CHAIN_DIR_OTHER_NETWORK',
      ['Retry', 'Not now'],
    ],
  ])(
    'offers Not now in place of the skip when a rebuild %s before deleting anything',
    (_name, code, actions) => {
      const backend = makeBackend({
        lastErrorRebuild: true,
        lastErrorCode: code,
        canChooseAnotherLocation: actions.includes('Choose another location'),
      });
      renderContainer(backend, copy);
      expect(offeredActions()).toEqual(actions);
      fireEvent.click(screen.getByRole('button', { name: 'Not now' }));
      expect(backend.startNode).toHaveBeenCalledTimes(1);
    }
  );

  it.each([
    [
      'could not reach the Mithril service',
      { lastErrorRebuild: false, lastErrorCode: 'PROBE_FAILED' },
    ],
    [
      'could not resolve the staging folder',
      { lastErrorRebuild: false, lastErrorCode: 'STAGING_FAILED' },
    ],
    [
      'failed while downloading',
      {
        lastErrorRebuild: false,
        lastErrorCode: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
      },
    ],
  ])(
    'keeps the skip when a partial sync %s and the node runs on the chain again',
    (_name, overrides) => {
      renderContainer(makeBackend(overrides), copy);
      expect(offeredActions()).toEqual(['Retry', 'Skip to blockchain sync']);
    }
  );

  it('offers the sync choice and no Not now when a rebuild is refused where there is no chain', () => {
    renderContainer(
      makeBackend({
        hasChain: false,
        lastErrorRebuild: true,
        lastErrorCode: 'PROBE_FAILED',
      }),
      copy
    );
    expect(offeredActions()).toEqual(['Retry', 'Choose how to sync']);
  });

  it('titles the error as a rebuild that could not finish', () => {
    renderContainer(
      makeBackend({ lastErrorRebuild: true, lastErrorCode: 'PROBE_FAILED' }),
      copy
    );
    expect(
      screen.getByRole('heading', {
        name: 'Could not rebuild the blockchain data',
      })
    ).toBeInTheDocument();
    expect(screen.queryByText('Mithril bootstrap failed')).toBeNull();
  });

  it('keeps the bootstrap title for an error that did not come from a rebuild', () => {
    renderContainer(
      makeBackend({ lastErrorRebuild: false, lastErrorCode: 'PROBE_FAILED' }),
      copy
    );
    expect(
      screen.getByRole('heading', { name: 'Mithril bootstrap failed' })
    ).toBeInTheDocument();
    expect(
      screen.queryByText('Could not rebuild the blockchain data')
    ).toBeNull();
  });
});

describe('MithrilSyncContainer after a rebuild removed the chain data', () => {
  afterEach(cleanup);

  const copy = {
    [messages.errorRetry.id]: 'Retry',
    [messages.errorSkip.id]: 'Skip to blockchain sync',
    [messages.errorNotNow.id]: 'Not now',
    [messages.errorChooseSync.id]: 'Choose how to sync',
    [messages.errorChainRemovedHint.id]: 'The old blockchain data was removed.',
    [messages.decisionChainRemovedNotice.id]: 'Old data removed, sync again.',
  };
  const removed = () =>
    makeBackend({
      hasChain: false,
      lastErrorRebuild: true,
      rebuildPending: true,
      lastErrorCode: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
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
      'Not now',
      'a rebuild was refused before it deleted anything',
      {
        hasChain: true,
        rebuildPending: false,
        lastErrorCode: 'PROBE_FAILED',
      },
    ],
    [
      'Skip to blockchain sync',
      'a partial sync failed and the node runs on the chain again',
      { hasChain: true, lastErrorRebuild: false, rebuildPending: false },
    ],
  ])(
    'notes no removal and starts the node from %s when %s',
    (label, _name, overrides) => {
      const backend = { ...removed(), ...overrides };
      renderContainer(backend, copy);
      expect(
        screen.queryByText('The old blockchain data was removed.')
      ).toBeNull();
      fireEvent.click(screen.getByRole('button', { name: label }));
      expect(backend.startNode).toHaveBeenCalledTimes(1);
    }
  );

  it.each([
    [
      'a rebuild was refused in a newly chosen location',
      { lastErrorRebuild: true, lastErrorCode: 'PROBE_FAILED' },
    ],
    [
      'a first bootstrap was refused',
      { lastErrorRebuild: false, lastErrorCode: 'PROBE_FAILED' },
    ],
    [
      'a first bootstrap failed to download',
      {
        lastErrorRebuild: false,
        lastErrorCode: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
      },
    ],
  ])(
    'offers the sync choice and no skip when %s and there is no chain',
    (_name, overrides) => {
      const backend = makeBackend({
        hasChain: false,
        rebuildPending: false,
        ...overrides,
      });
      renderContainer(backend, copy);
      expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
      expect(
        screen.queryByText('The old blockchain data was removed.')
      ).toBeNull();
      fireEvent.click(
        screen.getByRole('button', { name: 'Choose how to sync' })
      );
      expect(backend.showBootstrapDecision).toHaveBeenCalledTimes(1);
      expect(backend.startNode).not.toHaveBeenCalled();
    }
  );

  describe('when the rebuild could not delete the old data', () => {
    const wipeFailed = (overrides = {}) =>
      makeBackend({
        hasChain: false,
        lastErrorRebuild: true,
        rebuildPending: true,
        lastErrorCode: 'WIPE_FAILED',
        ...overrides,
      });
    const wipeCopy = {
      ...copy,
      [messages.errorWipeFailedHint.id]: 'Could not delete; close programs.',
    };

    it('says the data could not be deleted, not that it was removed, and offers Retry and the sync choice', () => {
      renderContainer(wipeFailed(), wipeCopy);
      expect(
        screen.getByText('Could not delete; close programs.')
      ).toBeInTheDocument();
      expect(
        screen.queryByText('The old blockchain data was removed.')
      ).toBeNull();
      expect(offeredActions()).toEqual(['Retry', 'Choose how to sync']);
    });

    it('says nothing about removed data on the decision view that follows', () => {
      renderContainer(
        wipeFailed({ loadingPhase: 'bootstrap-decision' }),
        wipeCopy
      );
      expect(screen.queryByText('Old data removed, sync again.')).toBeNull();
    });
  });

  it('notes the removal on the decision view after a restart mid-rebuild', () => {
    renderContainer(
      makeBackend({
        loadingPhase: 'bootstrap-decision',
        mithrilPhase: null,
        lastError: null,
        lastErrorCode: null,
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
        lastError: null,
        lastErrorCode: null,
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
        lastError: null,
        lastErrorCode: null,
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
    expect(offeredActions()).toEqual(['Retry']);
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
  // Main marks a failed install that was not a rebuild as unfinished
  const installFailed = (overrides = {}) =>
    makeBackend({
      lastErrorCode: 'INSTALL_FAILED',
      installRetryRequired: true,
      installPending: true,
      ...overrides,
    });

  it('offers only Retry after a partial sync failed to install', () => {
    const backend = installFailed();
    renderContainer(backend, copy);
    expect(offeredActions()).toEqual(['Retry']);
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    expect(screen.queryByText('Removed; retry.')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(backend.retryMithril).toHaveBeenCalledTimes(1);
    expect(backend.startNode).not.toHaveBeenCalled();
  });

  it('says the old data was removed and offers only Retry after the install of a rebuild failed', () => {
    renderContainer(
      installFailed({
        lastErrorRebuild: true,
        rebuildPending: true,
        installPending: false,
      }),
      copy
    );
    expect(screen.getByText('Removed; retry.')).toBeInTheDocument();
    expect(screen.queryByText('Removed; retry or choose.')).toBeNull();
    expect(screen.queryByText('Choose how to sync')).toBeNull();
    expect(offeredActions()).toEqual(['Retry']);
  });

  it('offers only Retry for a later error while the watchdog still waits for an install', () => {
    renderContainer(
      installFailed({ lastErrorCode: 'PROBE_FAILED', lastErrorRebuild: true }),
      copy
    );
    expect(screen.queryByText('Skip to blockchain sync')).toBeNull();
    expect(offeredActions()).toEqual(['Retry']);
  });
});

describe('MithrilSyncContainer after a partial sync failed to install', () => {
  afterEach(cleanup);

  const confirmation = () =>
    makeBackend({
      lastErrorCode: 'INSTALL_FAILED',
      installRetryRequired: true,
      installPending: true,
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

describe('MithrilSyncContainer while a confirmed rebuild starts', () => {
  afterEach(cleanup);

  it('shows the rebuild preparing with no Cancel in place of the error it replaces', () => {
    renderContainer(
      makeBackend({
        lastErrorCode: 'INSTALL_FAILED',
        installRetryRequired: true,
        rebuildStarting: true,
      }),
      {
        [messages.title.id]: 'Fast sync with Mithril',
        [messages.cancel.id]: 'Cancel',
        [messages.errorRetry.id]: 'Retry',
        [messages.errorTitle.id]: 'Mithril bootstrap failed',
      }
    );
    expect(
      screen.getByRole('heading', { name: 'Fast sync with Mithril' })
    ).toBeInTheDocument();
    expect(screen.queryByText('Mithril bootstrap failed')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Cancel' })).toBeNull();
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
