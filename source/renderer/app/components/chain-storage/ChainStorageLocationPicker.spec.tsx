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
          isRecoveryFallback={false}
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

    const input = screen.getByLabelText(/blockchain data location/i);

    expect(
      screen.getByRole('heading', {
        name: /select blockchain data location/i,
      })
    ).toBeInTheDocument();
    expect(input).toHaveDisplayValue('/mnt/current-chain/chain');
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
    expect(
      screen.getByDisplayValue('/mnt/new-chain/chain')
    ).toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onSetChainStorageDirectory).toHaveBeenCalledWith('/mnt/new-chain');
    });

    expect(onConfirmStorageLocation).toHaveBeenCalled();
  });

  it('applies the canonical parent when a selected alias resolves to chain data', async () => {
    const onSetChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/external',
      resolvedPath: '/mnt/external',
      availableSpaceBytes: 3000,
      requiredSpaceBytes: 1024,
      chainSubdirectoryStatus: 'existing-directory',
    });
    const onValidateChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/external',
      resolvedPath: '/mnt/external',
      availableSpaceBytes: 3000,
      requiredSpaceBytes: 1024,
      chainSubdirectoryStatus: 'existing-directory',
    });

    (showOpenDialogChannel.send as jest.Mock).mockResolvedValue({
      canceled: false,
      filePaths: ['/mnt/link-to-chain'],
    });

    renderComponent({
      onSetChainStorageDirectory,
      onValidateChainStorageDirectory,
    });

    fireEvent.click(screen.getByRole('button', { name: /choose directory/i }));

    await waitFor(() => {
      expect(onValidateChainStorageDirectory).toHaveBeenCalledWith(
        '/mnt/link-to-chain'
      );
    });

    expect(screen.getByDisplayValue('/mnt/external/chain')).toBeInTheDocument();
    expect(
      screen.getByText(
        /existing blockchain data found\. proceeding will reuse this data\./i
      )
    ).toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onSetChainStorageDirectory).toHaveBeenCalledWith('/mnt/external');
    });
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
      | React.Dispatch<ChainStorageValidation>
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
      .mockImplementation(() => undefined);

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

  it('links validation feedback to the input and announces it politely', () => {
    renderComponent({
      chainStorageValidation: {
        isValid: false,
        path: '/mnt/current-chain',
        resolvedPath: '/mnt/current-chain',
        availableSpaceBytes: 10,
        requiredSpaceBytes: 1024,
        reason: 'insufficient-space',
      },
    });

    const input = screen.getByLabelText(/blockchain data location/i);
    const statusRegion = screen.getByRole('status');
    const describedById = input.getAttribute('aria-describedby');

    expect(statusRegion).toHaveAttribute('aria-live', 'polite');
    expect(input).toHaveAttribute('aria-invalid', 'true');
    expect(describedById).toBeTruthy();
    expect(document.getElementById(describedById || '')).toHaveTextContent(
      /does not have enough free space/i
    );
  });

  it('keeps the attempted path visible and continue disabled after an invalid apply remount', () => {
    renderComponent({
      pendingChainPath: '/mnt/invalid-chain',
      chainStorageValidation: {
        isValid: false,
        path: '/mnt/invalid-chain',
        resolvedPath: '/mnt/invalid-chain',
        reason: 'insufficient-space',
        message: 'Selected directory does not have enough free space.',
      },
    });

    expect(
      screen.getByDisplayValue('/mnt/invalid-chain/chain')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /continue/i })).toBeDisabled();
  });

  it('announces apply feedback in a polite status region while updating', async () => {
    let resolveStorageChange:
      | React.Dispatch<ChainStorageValidation>
      | undefined;
    const onResetChainStorageDirectory = jest.fn(
      () =>
        new Promise<ChainStorageValidation>((resolve) => {
          resolveStorageChange = resolve;
        })
    );

    renderComponent({
      onResetChainStorageDirectory,
    });

    fireEvent.click(screen.getByRole('button', { name: /reset to default/i }));

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    expect(screen.getByRole('status')).toHaveTextContent(
      /updating blockchain data location/i
    );

    await act(async () => {
      resolveStorageChange?.({
        isValid: true,
        path: '/mnt/current-chain',
        resolvedPath: '/mnt/current-chain',
      });
      await Promise.resolve();
    });
  });

  it('focuses and announces the recovery fallback notice on initial render', () => {
    renderComponent({
      customChainPath: null,
      chainStorageValidation: defaultValidation,
      isRecoveryFallback: true,
    });

    const recoveryNotice = screen.getByText(
      /we couldn't access your previous storage location/i
    );

    expect(recoveryNotice).toHaveAttribute('role', 'status');
    expect(recoveryNotice).toHaveAttribute('aria-live', 'polite');
    expect(recoveryNotice).toHaveFocus();
  });

  it('clears the recovery notice after choosing a new directory', async () => {
    const onValidateChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: true,
      path: '/mnt/new-chain',
      resolvedPath: '/mnt/new-chain',
      availableSpaceBytes: 3000,
      requiredSpaceBytes: 1024,
    });

    (showOpenDialogChannel.send as jest.Mock).mockResolvedValue({
      canceled: false,
      filePaths: ['/mnt/new-chain'],
    });

    renderComponent({
      customChainPath: null,
      chainStorageValidation: defaultValidation,
      isRecoveryFallback: true,
      onValidateChainStorageDirectory,
    });

    fireEvent.click(screen.getByRole('button', { name: /choose directory/i }));

    await waitFor(() => {
      expect(onValidateChainStorageDirectory).toHaveBeenCalledWith(
        '/mnt/new-chain'
      );
    });

    expect(
      screen.queryByText(/we couldn't access your previous storage location/i)
    ).not.toBeInTheDocument();
  });

  it('clears the recovery notice immediately after continue is clicked', async () => {
    const onConfirmStorageLocation = jest.fn();

    renderComponent({
      customChainPath: null,
      chainStorageValidation: defaultValidation,
      isRecoveryFallback: true,
      onConfirmStorageLocation,
    });

    fireEvent.click(screen.getByRole('button', { name: /continue/i }));

    await waitFor(() => {
      expect(onConfirmStorageLocation).toHaveBeenCalled();
    });

    expect(
      screen.queryByText(/we couldn't access your previous storage location/i)
    ).not.toBeInTheDocument();
  });

  it('shows the data-found notice instead of the existing-directory help text', () => {
    renderComponent({
      chainStorageValidation: {
        isValid: true,
        path: '/mnt/current-chain',
        resolvedPath: '/mnt/current-chain',
        availableSpaceBytes: 4000,
        requiredSpaceBytes: 1024,
        chainSubdirectoryStatus: 'existing-directory',
      },
      isRecoveryFallback: true,
    });

    const input = screen.getByLabelText(/blockchain data location/i);
    const recoveryNotice = screen.getByText(
      /we couldn't access your previous storage location/i
    );
    const dataFoundNotice = screen.getByText(
      /existing blockchain data found\. proceeding will reuse this data\./i
    );
    const describedByIds =
      input.getAttribute('aria-describedby')?.split(' ') ?? [];

    expect(
      screen.queryByText(/will use the existing chain subdirectory/i)
    ).not.toBeInTheDocument();
    expect(dataFoundNotice).toBeInTheDocument();
    expect(describedByIds).toContain(recoveryNotice.getAttribute('id'));
    expect(describedByIds).toContain(dataFoundNotice.getAttribute('id'));
  });

  it('shows no storage status message for an empty directly selected chain directory', () => {
    renderComponent({
      customChainPath: '/mnt/current-parent',
      chainStorageValidation: {
        isValid: true,
        path: '/mnt/current-parent',
        resolvedPath: '/mnt/current-parent',
        availableSpaceBytes: 4000,
        requiredSpaceBytes: 1024,
      },
    });

    expect(
      screen.queryByText(/existing blockchain data found/i)
    ).not.toBeInTheDocument();
    expect(
      screen.queryByText(
        /chain subdirectory inside the selected parent folder/i
      )
    ).not.toBeInTheDocument();
  });

  it('displays the raw rejected path after validation failure instead of managed chain path', async () => {
    const onValidateChainStorageDirectory = jest.fn().mockResolvedValue({
      isValid: false,
      path: '/mnt/bad-chain/chain/db',
      reason: 'is-managed-child',
      message: 'Cannot select a path inside the managed chain directory.',
    });

    (showOpenDialogChannel.send as jest.Mock).mockResolvedValue({
      canceled: false,
      filePaths: ['/mnt/bad-chain/chain/db'],
    });

    renderComponent({
      onValidateChainStorageDirectory,
    });

    fireEvent.click(screen.getByRole('button', { name: /choose directory/i }));

    await waitFor(() => {
      expect(onValidateChainStorageDirectory).toHaveBeenCalledWith(
        '/mnt/bad-chain/chain/db'
      );
    });

    // Should display the raw rejected path, NOT /mnt/bad-chain/chain/db/chain
    expect(
      screen.getByDisplayValue('/mnt/bad-chain/chain/db')
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /continue/i })).toBeDisabled();
  });

  it('displays managed chain path for valid committed custom selections', () => {
    renderComponent({
      customChainPath: '/mnt/my-storage',
      chainStorageValidation: {
        isValid: true,
        path: '/mnt/my-storage',
        resolvedPath: '/mnt/my-storage',
        availableSpaceBytes: 4000,
        requiredSpaceBytes: 1024,
      },
    });

    expect(
      screen.getByDisplayValue('/mnt/my-storage/chain')
    ).toBeInTheDocument();
  });
});
