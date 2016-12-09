import type { AppController } from '../app/controllers/AppController';
import type { Api } from '../app/api';
import type environment from '../app/environment';
import type { appState } from '../app/state';

declare let daedalus = {
  controller: AppController,
  api: Api,
  environment: environment,
  state: appState,
  reset: () => any,
  render: () => any
};
