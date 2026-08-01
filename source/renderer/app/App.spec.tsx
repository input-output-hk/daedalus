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
    <div data-testid="mithril-sync-overlay">
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

jest.mock('./components/loading/mithril-bootstrap/MithrilSyncOverlay', () => ({
  __esModule: true,
  default: function MithrilSyncOverlay(props) {
    return overlayMock(props);
  },
}));

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
    mithrilSync: {
      shouldShowOverlay: false,
      flowType: null,
      status: 'idle',
      progressItems: [],
      startedAt: undefined,
      filesDownloaded: undefined,
      filesTotal: undefined,
      snapshotBytesDownloaded: undefined,
      snapshotBytesTotal: undefined,
      ancillaryBytesDownloaded: undefined,
      ancillaryBytesTotal: undefined,
      ancillaryProgress: undefined,
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
      screen.queryByTestId('mithril-sync-overlay')
    ).not.toBeInTheDocument();
  });

  it('mounts the sync overlay and forwards all recovery callbacks', () => {
    const stores = makeStores({
      mithrilSync: {
        shouldShowOverlay: true,
        flowType: 'partial-sync',
        status: 'failed',
        progressItems: [],
        startedAt: undefined,
        filesDownloaded: 3,
        filesTotal: 9,
        snapshotBytesDownloaded: undefined,
        snapshotBytesTotal: undefined,
        ancillaryBytesDownloaded: 1,
        ancillaryBytesTotal: 2,
        ancillaryProgress: undefined,
        error: null,
        canRetry: true,
        canRestartNormally: true,
        canWipeAndFullSync: true,
        cancelPartialSync: jest.fn(),
        startPartialSync: jest.fn().mockResolvedValue(undefined),
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
    expect(screen.getByTestId('mithril-sync-overlay')).toBeInTheDocument();
    expect(overlayMock).toHaveBeenCalledTimes(1);
    expect(overlayMock.mock.calls[0][0]).toEqual(
      expect.objectContaining({
        status: 'failed',
        onCancel: stores.mithrilSync.cancelPartialSync,
        onRetry: expect.any(Function),
        onRestartNormally: stores.mithrilSync.restartNormally,
        onWipeAndFullSync: stores.mithrilSync.wipeAndFullSync,
        onDismissCompleted: stores.mithrilSync.dismissCompletedOverlay,
        onOpenExternalLink: stores.app.openExternalLink,
      })
    );

    fireEvent.click(screen.getByRole('button', { name: 'cancel' }));
    fireEvent.click(screen.getByRole('button', { name: 'retry' }));
    fireEvent.click(screen.getByRole('button', { name: 'restart' }));
    fireEvent.click(screen.getByRole('button', { name: 'wipe' }));
    fireEvent.click(screen.getByRole('button', { name: 'dismiss' }));

    expect(stores.mithrilSync.cancelPartialSync).toHaveBeenCalledTimes(1);
    expect(stores.mithrilSync.startPartialSync).toHaveBeenCalledTimes(1);
    expect(stores.mithrilSync.restartNormally).toHaveBeenCalledTimes(1);
    expect(stores.mithrilSync.wipeAndFullSync).toHaveBeenCalledTimes(1);
    expect(stores.mithrilSync.dismissCompletedOverlay).toHaveBeenCalledTimes(1);
  });
});
