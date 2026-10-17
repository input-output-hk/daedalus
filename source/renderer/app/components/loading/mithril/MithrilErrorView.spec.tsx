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
  extraProps: { chainRemoved?: boolean } = {}
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

  it('says the old blockchain data was removed after a failed rebuild', () => {
    renderView(
      { code: 'DOWNLOAD_FAILED', stage: 'download', message: 'x' },
      { chainRemoved: true }
    );
    expect(
      screen.getByText(
        'The old blockchain data was removed. Retry downloads it again with Mithril, or choose how to sync.'
      )
    ).toBeInTheDocument();
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
      'The blockchain data folder contains other files, so Daedalus did not delete it. Move those files elsewhere and try again.',
    ],
    [
      'CHAIN_DIR_OTHER_NETWORK',
      'The blockchain data folder holds data for a different Cardano network, so Daedalus did not change it. Choose a different blockchain data location and try again.',
    ],
    [
      'INSUFFICIENT_SPACE',
      'There is not enough free space on the drive that holds the blockchain data. Your blockchain data has not been changed. Free some space on that drive and retry.',
    ],
    [
      'MARKER_FAILED',
      'Daedalus could not record the rebuild in its data folder, so it changed nothing. Check that the data folder is writable and retry.',
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
