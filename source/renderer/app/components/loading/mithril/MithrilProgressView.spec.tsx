import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import MithrilProgressView from './MithrilProgressView';

const waitingLine = 'Downloading will continue when the network returns.';

const renderView = (props = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <MithrilProgressView
        status="downloading"
        filesDownloaded={47}
        filesTotal={100}
        onAction={jest.fn()}
        {...props}
      />
    </IntlProvider>
  );

describe('MithrilProgressView', () => {
  afterEach(cleanup);

  it('says that downloading continues when the network returns while the download waits', () => {
    renderView({ waitingForNetwork: true });
    expect(screen.getByRole('status')).toHaveTextContent(waitingLine);
  });

  it('says nothing about the network while the download goes on', () => {
    renderView({ waitingForNetwork: false });
    expect(screen.queryByText(waitingLine)).not.toBeInTheDocument();
  });

  it('keeps the view and its Cancel while the download waits', () => {
    renderView({ waitingForNetwork: true });
    expect(screen.getByRole('button', { name: 'Cancel' })).toBeInTheDocument();
    expect(
      screen.getByRole('heading', { name: 'Fast sync with Mithril' })
    ).toBeInTheDocument();
  });
});
