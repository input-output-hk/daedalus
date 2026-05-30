import React from 'react';
import { createMemoryHistory } from 'history';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import App from './App';

const overlayMock = jest.fn(
  ({
    status,
    onCancel,
    onRetry,
    onRestartNormally,
    onWipeAndFullSync,
    onDismissCompleted,
  }) => (
    <div data-testid="mithril-partial-sync-overlay">
      <span>{status}</span>
      <button onClick={onCancel}>cancel</button>
      <button onClick={onRetry}>retry</button>
      <button onClick={onRestartNormally}>restart</button>
      <button onClick={onWipeAndFullSync}>wipe</button>
      <button onClick={onDismissCompleted}>dismiss</button>
    </div>
  )
);

jest.mock('./Routes', () => ({
  Routes() {
    return <div data-testid="routes" />;
  },
}));

jest.mock(
  './ThemeManager',
  () =>
    function ThemeManager() {
      return <div data-testid="theme-manager" />;
    }
);

jest.mock('./i18n/translations', () => ({
  __esModule: true,
  default: { 'en-US': {} },
}));

jest.mock('./components/analytics', () => ({
  AnalyticsProvider({ children }: { children: React.ReactNode }) {
    return <>{children}</>;
  },
}));

jest.mock('./containers/MenuUpdater', () => ({
  MenuUpdater() {
    return null;
  },
}));

jest.mock(
  './containers/static/AboutDialog',
  () =>
    function AboutDialog() {
      return null;
    }
);
jest.mock(
  './containers/status/DaedalusDiagnosticsDialog',
  () =>
    function DaedalusDiagnosticsDialog() {
      return null;
    }
);
jest.mock(
  './containers/notifications/NotificationsContainer',
  () =>
    function NotificationsContainer() {
      return null;
    }
);
jest.mock(
  './containers/news/NewsOverlayContainer',
  () =>
    function NewsOverlayContainer() {
      return null;
    }
);
jest.mock(
  './containers/news/NewsFeedContainer',
  () =>
    function NewsFeedContainer() {
      return null;
    }
);
jest.mock(
  './containers/knownIssues/ToggleRTSFlagsDialogContainer',
  () =>
    function ToggleRTSFlagsDialogContainer() {
      return null;
    }
);
jest.mock(
  './containers/knownIssues/RTSFlagsRecommendationOverlayContainer',
  () =>
    function RTSFlagsRecommendationOverlayContainer() {
      return null;
    }
);

jest.mock(
  './components/loading/mithril-bootstrap/MithrilPartialSyncOverlay',
  () => ({
    __esModule: true,
    default: function MithrilPartialSyncOverlay(props) {
      return overlayMock(props);
    },
  })
);

jest.mock('react-polymorph/lib/components/ThemeProvider', () => ({
  ThemeProvider({ children }: { children: React.ReactNode }) {
    return <>{children}</>;
  },
}));

jest.mock('react-polymorph/lib/skins/simple', () => ({
  SimpleSkins: {},
}));

jest.mock('react-polymorph/lib/themes/simple', () => ({
  SimpleDefaults: {},
}));

jest.mock('./themes/daedalus/cardano.ts', () => ({
  __esModule: true,
  default: {},
}));

describe('App', () => {
  const makeStores = (overrides = {}) => ({
    app: {
      isActiveDialog: jest.fn(() => false),
      isSetupPage: false,
      openExternalLink: jest.fn(),
    },
    mithrilPartialSync: {
      shouldShowOverlay: false,
      status: 'idle',
      progressItems: [],
      filesDownloaded: undefined,
      filesTotal: undefined,
      elapsedSeconds: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      error: null,
      canRetry: false,
      canRestartNormally: false,
      canWipeAndFullSync: false,
      cancelPartialSync: jest.fn(),
      startPartialSync: jest.fn(),
      restartNormally: jest.fn(),
      wipeAndFullSync: jest.fn(),
      dismissCompletedOverlay: jest.fn(),
    },
    networkStatus: {
      isNodeStopping: false,
      isNodeStopped: false,
    },
    profile: {
      currentLocale: 'en-US',
      currentTheme: 'cardano',
    },
    ...overrides,
  });

  const makeActions = () => ({
    app: {
      initAppEnvironment: { trigger: jest.fn() },
    },
  });

  afterEach(() => {
    cleanup();
    overlayMock.mockClear();
  });

  it('does not mount the partial sync overlay when the store hides it', () => {
    render(
      <App
        stores={makeStores() as any}
        actions={makeActions() as any}
        history={createMemoryHistory()}
      />
    );

    expect(
      screen.queryByTestId('mithril-partial-sync-overlay')
    ).not.toBeInTheDocument();
  });

  it('mounts the partial sync overlay and forwards all recovery callbacks', () => {
    const stores = makeStores({
      mithrilPartialSync: {
        shouldShowOverlay: true,
        status: 'failed',
        progressItems: [],
        filesDownloaded: 3,
        filesTotal: 9,
        elapsedSeconds: 65,
        ancillaryBytesDownloaded: 1,
        ancillaryBytesTotal: 2,
        error: null,
        canRetry: true,
        canRestartNormally: true,
        canWipeAndFullSync: true,
        cancelPartialSync: jest.fn(),
        startPartialSync: jest.fn(),
        restartNormally: jest.fn(),
        wipeAndFullSync: jest.fn(),
        dismissCompletedOverlay: jest.fn(),
      },
    });
    const actions = makeActions();

    render(
      <App
        stores={stores as any}
        actions={actions as any}
        history={createMemoryHistory()}
      />
    );

    expect(actions.app.initAppEnvironment.trigger).toHaveBeenCalledTimes(1);
    expect(
      screen.getByTestId('mithril-partial-sync-overlay')
    ).toBeInTheDocument();
    expect(overlayMock).toHaveBeenCalledTimes(1);
    expect(overlayMock.mock.calls[0][0]).toEqual(
      expect.objectContaining({
        status: 'failed',
        onCancel: stores.mithrilPartialSync.cancelPartialSync,
        onRetry: stores.mithrilPartialSync.startPartialSync,
        onRestartNormally: stores.mithrilPartialSync.restartNormally,
        onWipeAndFullSync: stores.mithrilPartialSync.wipeAndFullSync,
        onDismissCompleted: stores.mithrilPartialSync.dismissCompletedOverlay,
        onOpenExternalLink: stores.app.openExternalLink,
      })
    );

    fireEvent.click(screen.getByRole('button', { name: 'cancel' }));
    fireEvent.click(screen.getByRole('button', { name: 'retry' }));
    fireEvent.click(screen.getByRole('button', { name: 'restart' }));
    fireEvent.click(screen.getByRole('button', { name: 'wipe' }));
    fireEvent.click(screen.getByRole('button', { name: 'dismiss' }));

    expect(stores.mithrilPartialSync.cancelPartialSync).toHaveBeenCalledTimes(
      1
    );
    expect(stores.mithrilPartialSync.startPartialSync).toHaveBeenCalledTimes(1);
    expect(stores.mithrilPartialSync.restartNormally).toHaveBeenCalledTimes(1);
    expect(stores.mithrilPartialSync.wipeAndFullSync).toHaveBeenCalledTimes(1);
    expect(
      stores.mithrilPartialSync.dismissCompletedOverlay
    ).toHaveBeenCalledTimes(1);
  });
});
