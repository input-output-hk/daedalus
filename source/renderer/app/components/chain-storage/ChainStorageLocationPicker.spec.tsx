import React from 'react';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import type { ChainStorageValidation } from '../../../../common/types/mithril-bootstrap.types';
import translations from '../../i18n/locales/en-US.json';
import { showOpenDialogChannel } from '../../ipc/show-file-dialog-channels';
import ChainStorageLocationPicker from './ChainStorageLocationPicker';

jest.mock('../../ipc/show-file-dialog-channels', () => ({
  showOpenDialogChannel: {
    send: jest.fn(),
  },
}));

describe('ChainStorageLocationPicker', () => {
  const defaultValidation: ChainStorageValidation = {
    isValid: true,
    path: null,
    resolvedPath: '/tmp/state/chain',
    availableSpaceBytes: 5000,
    requiredSpaceBytes: 1024,
  };
  const customValidation: ChainStorageValidation = {
    isValid: true,
    path: '/mnt/current-chain',
    resolvedPath: '/mnt/current-chain',
    availableSpaceBytes: 4000,
    requiredSpaceBytes: 1024,
  };

  const renderComponent = (overrides = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <ChainStorageLocationPicker
          customChainPath="/mnt/current-chain"
          defaultChainPath="/tmp/state/chain"
          defaultChainStorageValidation={defaultValidation}
          chainStorageValidation={customValidation}
          isChainStorageLoading={false}
          onSetChainStorageDirectory={jest.fn()}
          onResetChainStorageDirectory={jest.fn()}
          onValidateChainStorageDirectory={jest.fn()}
          onConfirmStorageLocation={jest.fn()}
          {...overrides}
        />
      </IntlProvider>
    );

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterEach(cleanup);

  it('shows generic blockchain data copy and the recommended free space', () => {
    renderComponent({
      estimatedRequiredSpaceBytes: 2048,
    });

    expect(
      screen.getByRole('heading', {
        name: /select blockchain data location/i,
      })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/the latest available snapshot is about 2 kB/i)
    ).toBeInTheDocument();
  });

  it('shows a large-space warning when no estimate is available', () => {
    renderComponent({
      estimatedRequiredSpaceBytes: undefined,
    });

    expect(
      screen.getByText(
        /blockchain data can require a large amount of free space/i
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/the latest available snapshot is about/i)
    ).not.toBeInTheDocument();
  });

  it('validates a chosen directory before applying it on continue', async () => {
    const onSetChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
    });
    const onValidateChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
      availableSpaceBytes: 3000,
      requiredSpaceBytes: 1024,
    });
    const onConfirmStorageLocation = jest.fn();

    (showOpenDialogChannel.send as jest.Mock).mockResolvedValue({
      canceled: false,
      filePaths: ['/mnt/new-chain'],
    });

    renderComponent({
      onSetChainStorageDirectory,
      onValidateChainStorageDirectory,
      onConfirmStorageLocation,
    });

    fireEvent.click(screen.getByRole('button', { name: /choose directory/i }));

    await waitFor(() => {
      expect(onValidateChainStorageDirectory).toHaveBeenCalledWith(
        '/mnt/new-chain'
      );
    });

    expect(onSetChainStorageDirectory).not.toHaveBeenCalled();
    expect(screen.getByDisplayValue('/mnt/new-chain')).toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onSetChainStorageDirectory).toHaveBeenCalledWith('/mnt/new-chain');
    });

    expect(onConfirmStorageLocation).toHaveBeenCalled();
  });

  it('defers reset-to-default until continue is pressed', async () => {
    const onResetChainStorageDirectory = jest
      .fn()
      .mockResolvedValue(defaultValidation);
    const onConfirmStorageLocation = jest.fn();

    renderComponent({
      onResetChainStorageDirectory,
      onConfirmStorageLocation,
    });

    fireEvent.click(screen.getByRole('button', { name: /reset to default/i }));

    expect(onResetChainStorageDirectory).not.toHaveBeenCalled();
    expect(screen.getByDisplayValue('/tmp/state/chain')).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /reset to default/i })
    ).not.toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onResetChainStorageDirectory).toHaveBeenCalled();
    });

    expect(onConfirmStorageLocation).toHaveBeenCalled();
  });

  it('does not update local state after unmount while applying a storage change', async () => {
    let resolveStorageChange:
      | ((value: ChainStorageValidation) => void)
      | undefined;
    const onValidateChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
      availableSpaceBytes: 3000,
      requiredSpaceBytes: 1024,
    });
    const onSetChainStorageDirectory = jest.fn(
      () =>
        new Promise<ChainStorageValidation>((resolve) => {
          resolveStorageChange = resolve;
        })
    );
    const consoleErrorSpy = jest
      .spyOn(console, 'error')
      .mockImplementation(() => {});

    const { unmount } = renderComponent({
      onValidateChainStorageDirectory,
      onSetChainStorageDirectory,
    });

    (showOpenDialogChannel.send as jest.Mock).mockResolvedValue({
      canceled: false,
      filePaths: ['/mnt/new-chain'],
    });

    fireEvent.click(screen.getByRole('button', { name: /choose directory/i }));

    await waitFor(() => {
      expect(onValidateChainStorageDirectory).toHaveBeenCalledWith(
        '/mnt/new-chain'
      );
    });

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onSetChainStorageDirectory).toHaveBeenCalledWith('/mnt/new-chain');
    });

    unmount();

    await act(async () => {
      resolveStorageChange?.({
        isValid: true,
        path: '/mnt/current-chain',
        resolvedPath: '/mnt/current-chain',
      });
      await Promise.resolve();
    });

    expect(consoleErrorSpy).not.toHaveBeenCalledWith(
      expect.stringContaining(
        "Can't perform a React state update on an unmounted component"
      )
    );

    consoleErrorSpy.mockRestore();
  });
});
