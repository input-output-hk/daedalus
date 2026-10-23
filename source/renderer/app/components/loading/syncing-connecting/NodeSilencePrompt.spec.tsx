import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import NodeSilencePrompt from './NodeSilencePrompt';

const renderPrompt = (silentSecs = 300) => {
  const props = {
    onRestart: jest.fn(),
    onKeepWaiting: jest.fn(),
  };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <NodeSilencePrompt silentSecs={silentSecs} {...props} />
    </IntlProvider>
  );
  return props;
};

describe('NodeSilencePrompt', () => {
  afterEach(cleanup);

  it('says for how many minutes the node has reported nothing', () => {
    renderPrompt(300);
    expect(
      screen.getByRole('heading', {
        name: 'Cardano node is not reporting progress',
      })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Cardano node has reported nothing for 5 minutes while starting. It may still be working. If it is stuck, restarting it can help.'
      )
    ).toBeInTheDocument();
  });

  it('counts a silence shorter than a minute as one minute', () => {
    renderPrompt(20);
    expect(
      screen.getByText(/has reported nothing for 1 minute while starting/)
    ).toBeInTheDocument();
  });

  it('restarts the node from Restart Cardano node', () => {
    const { onRestart, onKeepWaiting } = renderPrompt();
    fireEvent.click(
      screen.getByRole('button', { name: 'Restart Cardano node' })
    );
    expect(onRestart).toHaveBeenCalledTimes(1);
    expect(onKeepWaiting).not.toHaveBeenCalled();
  });

  it('keeps waiting from Keep waiting', () => {
    const { onRestart, onKeepWaiting } = renderPrompt();
    fireEvent.click(screen.getByRole('button', { name: 'Keep waiting' }));
    expect(onKeepWaiting).toHaveBeenCalledTimes(1);
    expect(onRestart).not.toHaveBeenCalled();
  });
});
