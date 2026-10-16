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
