import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import MithrilPartialSyncOverlay, {
  isMithrilPartialSyncOverlayStatus,
} from './MithrilPartialSyncOverlay';

describe('MithrilPartialSyncOverlay', () => {
  const renderComponent = (overrides = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilPartialSyncOverlay
          status="downloading"
          progressItems={[]}
          filesDownloaded={3}
          filesTotal={9}
          elapsedSeconds={65}
          ancillaryBytesDownloaded={1}
          ancillaryBytesTotal={2}
          error={null}
          canRetry={false}
          canRestartNormally={false}
          canWipeAndFullSync={false}
          onCancel={jest.fn()}
          onRetry={jest.fn()}
          onRestartNormally={jest.fn()}
          onWipeAndFullSync={jest.fn()}
          onDismissCompleted={jest.fn()}
          onOpenExternalLink={jest.fn()}
          {...overrides}
        />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the progress overlay with partial sync copy', () => {
    renderComponent();

    expect(
      screen.getByRole('heading', { name: /mithril partial sync/i })
    ).toBeInTheDocument();
    expect(screen.getByText('1:05')).toBeInTheDocument();
  });

  it('renders recovery actions only from allowed backend actions', () => {
    const onRetry = jest.fn();
    const onRestartNormally = jest.fn();

    renderComponent({
      status: 'failed',
      canRetry: true,
      canRestartNormally: true,
      canWipeAndFullSync: false,
      error: null,
      onRetry,
      onRestartNormally,
    });

    fireEvent.click(
      screen.getByRole('button', { name: /retry mithril partial sync/i })
    );
    fireEvent.click(screen.getByRole('button', { name: /restart normally/i }));

    expect(onRetry).toHaveBeenCalledTimes(1);
    expect(onRestartNormally).toHaveBeenCalledTimes(1);
    expect(
      screen.queryByRole('button', {
        name: /wipe chain data and do full mithril sync/i,
      })
    ).not.toBeInTheDocument();
  });

  it('shows a completed dismiss action', () => {
    const onDismissCompleted = jest.fn();
    renderComponent({ status: 'completed', onDismissCompleted });

    fireEvent.click(
      screen.getByRole('button', { name: /continue to daedalus/i })
    );

    expect(onDismissCompleted).toHaveBeenCalledTimes(1);
  });

  it('does not reuse the bootstrap byte-progress bar for partial sync file counts', () => {
    renderComponent({
      progressItems: [
        {
          id: 'step-3',
          label: 'Downloading snapshot data',
          state: 'active',
        },
      ],
      filesDownloaded: 3,
      filesTotal: 9,
    });

    expect(
      screen.queryByText(/snapshot files: .*fast sync:/i)
    ).not.toBeInTheDocument();
  });
});

describe('isMithrilPartialSyncOverlayStatus', () => {
  it('excludes the optimistic stopping-node seed', () => {
    expect(isMithrilPartialSyncOverlayStatus('stopping-node')).toBe(false);
    expect(isMithrilPartialSyncOverlayStatus('preparing')).toBe(true);
    expect(isMithrilPartialSyncOverlayStatus('failed')).toBe(true);
  });
});

describe('Mithril partial sync locale strings', () => {
  it('ships polished runtime strings without placeholder markers', () => {
    const keys = Object.keys(translations).filter((key) =>
      key.startsWith('loading.mithrilPartialSync.')
    );

    keys.forEach((key) => {
      expect(translations[key]).not.toMatch(/^!!!/);
      expect(jaTranslations[key]).not.toMatch(/^!!!/);
    });
  });
});
