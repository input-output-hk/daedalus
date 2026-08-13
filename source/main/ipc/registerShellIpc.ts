import { registerOpenExternalUrlChannel } from './open-external-url';
import { registerOpenLocalDirectoryChannel } from './open-local-directory';

let installed = false;

export const registerShellIpc = (): void => {
  if (installed) return;
  installed = true;
  registerOpenExternalUrlChannel();
  registerOpenLocalDirectoryChannel();
};
