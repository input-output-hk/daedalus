// @flow
import { dialog } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  SHOW_OPEN_DIALOG_CHANNEL,
  SHOW_SAVE_DIALOG_CHANNEL,
} from '../../common/ipc/api';
import type {
  ShowOpenDialogRendererRequest,
  ShowOpenDialogMainResponse,
  ShowSaveDialogRendererRequest,
  ShowSaveDialogMainResponse,
} from '../../common/ipc/api';

export const showOpenDialogChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ShowOpenDialogRendererRequest,
  ShowOpenDialogMainResponse
> = new MainIpcChannel(SHOW_OPEN_DIALOG_CHANNEL);

export const showSaveDialogChannel: // IpcChannel<Incoming, Outgoing>
MainIpcChannel<
  ShowSaveDialogRendererRequest,
  ShowSaveDialogMainResponse
> = new MainIpcChannel(SHOW_SAVE_DIALOG_CHANNEL);

export const handleFileDialogRequests = () => {
  showOpenDialogChannel.onReceive((request: ShowOpenDialogRendererRequest) =>
    dialog.showOpenDialog(request.browserWindow, request.options)
  );
  showSaveDialogChannel.onReceive((request: ShowSaveDialogRendererRequest) =>
    dialog.showSaveDialog(request.browserWindow, request.options)
  );
};
