import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';

import MithrilErrorView from './MithrilErrorView';

const renderView = (
  error: {
    code?: string;
    stage?: string;
    message: string;
  },
  extraProps: {
    rebuild?: boolean;
    chainRemoved?: boolean;
    installUnfinished?: boolean;
    retryOnly?: boolean;
  } = {}
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilErrorView error={error as any} actions={[]} {...extraProps} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('MithrilErrorView', () => {
  afterEach(cleanup);

  it('opens and closes the technical details with a button in their header', () => {
    renderView({ code: 'WIPE_FAILED', message: 'detail from the watchdog' });
    const header = screen.getByRole('heading', { name: /Technical details/ });
    expect(header).toHaveTextContent(/^Technical detailsView$/);
    expect(screen.queryByText('detail from the watchdog')).toBeNull();
    fireEvent.click(
      screen.getByRole('button', { name: 'Technical details: View' })
    );
    expect(screen.getByText('detail from the watchdog')).toBeInTheDocument();
    fireEvent.click(
      screen.getByRole('button', { name: 'Technical details: Hide' })
    );
    expect(screen.queryByText('detail from the watchdog')).toBeNull();
  });

  it('says the old blockchain data was removed after a failed rebuild', () => {
    renderView(
      { code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED', message: 'x' },
      { chainRemoved: true, rebuild: true }
    );
    expect(
      screen.getByText(
        'The old blockchain data was removed. Retry downloads it again with Mithril, or choose how to sync.'
      )
    ).toBeInTheDocument();
  });

  it('says the old blockchain data was removed and offers no sync choice when Retry is the only action', () => {
    renderView(
      { code: 'INSTALL_FAILED', message: 'x' },
      { chainRemoved: true, retryOnly: true }
    );
    expect(
      screen.getByText(
        'The old blockchain data was removed. Retry downloads it again with Mithril.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/choose how to sync/)).toBeNull();
  });

  it('says a download did not finish installing, and nothing about removed data, after an unfinished install', () => {
    renderView(
      { code: 'PROBE_FAILED', message: 'x' },
      { installUnfinished: true }
    );
    expect(
      screen.getByText(
        'A download of the blockchain data did not finish installing. Retry downloads it again with Mithril, or choose how to sync.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/was removed/)).toBeNull();
    expect(screen.queryByText(/has not been changed/)).toBeNull();
  });

  it('says a download did not finish installing and offers no sync choice when Retry is the only action', () => {
    renderView(
      { code: 'INSTALL_FAILED', message: 'x' },
      { installUnfinished: true, retryOnly: true }
    );
    expect(
      screen.getByText(
        'A download of the blockchain data did not finish installing. Retry downloads it again with Mithril.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/choose how to sync/)).toBeNull();
  });

  it.each([
    [
      'PROBE_FAILED',
      'Daedalus could not reach the Mithril service. Your blockchain data has not been changed. Check your internet connection and try again.',
    ],
    [
      'INVALID_CHAIN_PATH',
      'Daedalus cannot rebuild the blockchain data in this folder. Choose a different blockchain data location and try again.',
    ],
    [
      'CHAIN_DIR_NOT_REPLACEABLE',
      'The blockchain data folder contains other files, so Daedalus did not delete it. Move those files elsewhere and retry, or choose another location.',
    ],
    [
      'CHAIN_DIR_OTHER_NETWORK',
      'The blockchain data folder holds data for a different Cardano network, so Daedalus did not change it. Choose a different blockchain data location and try again.',
    ],
    [
      'INSUFFICIENT_SPACE',
      'There is not enough free space on the drive that holds the blockchain data. Your blockchain data has not been changed. Free some space on that drive and retry, or choose another location.',
    ],
    [
      'REBUILD_MARKER_FAILED',
      'Daedalus could not record the rebuild in its data folder, so it changed nothing. Check that the data folder is writable and retry.',
    ],
    [
      'MARKER_FAILED',
      'Daedalus could not write to its data folder before installing the downloaded blockchain data, so it did not install it. Check that the data folder is writable and retry.',
    ],
    [
      'WIPE_FAILED',
      'Daedalus could not delete the blockchain data files. Close any other programs that might be using the blockchain folder, then retry.',
    ],
  ])('shows the specific message for %s', (code, hint) => {
    renderView({ code, message: 'detail from the watchdog' });
    expect(screen.getByText(hint)).toBeInTheDocument();
    fireEvent.click(screen.getByText('View'));
    expect(screen.getByText(code)).toBeInTheDocument();
  });

  it.each([
    [
      'PROBE_FAILED',
      'Daedalus could not reach the Mithril service. Check your internet connection and try again.',
    ],
    [
      'INSUFFICIENT_SPACE',
      'There is not enough free space on the drive that holds the blockchain data. Free some space on that drive and retry, or choose another location.',
    ],
    [
      'CHAIN_DIR_NOT_REPLACEABLE',
      'The blockchain data folder contains other files, so Daedalus cannot install the blockchain data there. Move those files elsewhere and retry, or choose another location.',
    ],
    [
      'CHAIN_DIR_OTHER_NETWORK',
      'The blockchain data folder holds data for a different Cardano network. Choose a different blockchain data location and try again.',
    ],
    [
      'REBUILD_MARKER_FAILED',
      'Daedalus could not record the rebuild in its data folder. Check that the data folder is writable and retry.',
    ],
  ])(
    'does not say that nothing changed for %s once the old data was removed',
    (code, hint) => {
      renderView({ code, message: 'x' }, { chainRemoved: true });
      expect(screen.getByText(hint)).toBeInTheDocument();
      expect(
        screen.queryByText(
          /has not been changed|changed nothing|did not delete it|did not change it/
        )
      ).toBeNull();
      expect(
        screen.getByText(/The old blockchain data was removed/)
      ).toBeInTheDocument();
    }
  );

  it.each([
    ['PROBE_FAILED', /could not reach the Mithril service/],
    ['INSUFFICIENT_SPACE', /not enough free space/],
    ['CHAIN_DIR_NOT_REPLACEABLE', /cannot install the blockchain data there/],
    ['CHAIN_DIR_OTHER_NETWORK', /different Cardano network/],
    ['REBUILD_MARKER_FAILED', /could not record the rebuild/],
  ])(
    'does not say that nothing changed for %s in the wait after a failed install',
    (code, hint) => {
      renderView({ code, message: 'x' }, { retryOnly: true });
      expect(screen.getByText(hint)).toBeInTheDocument();
      expect(
        screen.queryByText(
          /has not been changed|changed nothing|did not delete it|did not change it/
        )
      ).toBeNull();
    }
  );

  it('says only that the old data could not be deleted after a rebuild failed to delete it', () => {
    renderView(
      { code: 'WIPE_FAILED', message: 'x' },
      { chainRemoved: true, rebuild: true }
    );
    expect(
      screen.getByText(
        'Daedalus could not delete the blockchain data files. Close any other programs that might be using the blockchain folder, then retry.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/was removed/)).toBeNull();
  });

  it('keeps the message of a code that says nothing about unchanged data once the old data was removed', () => {
    renderView({ code: 'MARKER_FAILED', message: 'x' }, { chainRemoved: true });
    expect(screen.getByText(/so it did not install it/)).toBeInTheDocument();
  });

  it('prefers the message for the code over the one for the stage', () => {
    renderView({
      code: 'PROBE_FAILED',
      stage: 'download',
      message: 'connection refused',
    });
    expect(
      screen.getByText(/could not reach the Mithril service/)
    ).toBeVisible();
    expect(
      screen.queryByText(/Check your network connection and try the download/)
    ).not.toBeInTheDocument();
  });

  it.each([
    ['a refusal', { code: 'PROBE_FAILED', message: 'x' }],
    [
      'a failure in a stage',
      { code: 'mithril_error', stage: 'download', message: 'x' },
    ],
  ])(
    'titles %s of a rebuild as a rebuild that could not finish',
    (_name, error) => {
      renderView(error, { rebuild: true });
      expect(
        screen.getByRole('heading', {
          name: 'Could not rebuild the blockchain data',
        })
      ).toBeInTheDocument();
      expect(screen.queryByText('Mithril bootstrap failed')).toBeNull();
    }
  );

  it('keeps the bootstrap title when the failure was not a rebuild', () => {
    renderView({ code: 'PROBE_FAILED', message: 'x' });
    expect(
      screen.getByRole('heading', { name: 'Mithril bootstrap failed' })
    ).toBeInTheDocument();
    expect(
      screen.queryByText('Could not rebuild the blockchain data')
    ).toBeNull();
  });

  it('shows the message for the stage when the code has none', () => {
    renderView({
      code: 'mithril_error',
      stage: 'download',
      message: 'connection refused',
    });
    expect(
      screen.getByText(
        'Check your network connection and try the download again.'
      )
    ).toBeInTheDocument();
  });
});
