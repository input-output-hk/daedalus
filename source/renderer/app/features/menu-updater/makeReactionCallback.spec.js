// @flow

import { RouterStore } from 'mobx-react-router';
import type { RebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import ProfileStore from '../../stores/ProfileStore';
import UiDialogsStore from '../../stores/UiDialogsStore';
import makeReactionCallback from './makeReactionCallback';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { ROUTES } from '../../routes-config';

const makeProfile = ({
  currentLocaleCallback = () => {},
  areTermsOfUseAccepted = false,
} = {}): ProfileStore =>
  ({
    get currentLocale() {
      currentLocaleCallback();
      return 'en';
    },
    areTermsOfUseAccepted,
  }: any);
const makeRouter = ({ pathname = ROUTES.WALLETS.ROOT } = {}): RouterStore =>
  ({ location: { pathname } }: any);
const makeUiDialogs = ({ activeDialog = false } = {}): UiDialogsStore =>
  ({ activeDialog }: any);
const makeRebuildApplicationMenu = ({
  send = () => {},
} = {}): RebuildApplicationMenu => ({ send }: any);

const defaultArgs = {
  stores: {
    profile: makeProfile(),
    router: makeRouter(),
    uiDialogs: makeUiDialogs(),
  },
  rebuildApplicationMenu: makeRebuildApplicationMenu(),
};

describe('MenuUpdater feature/makeReactionCallback', () => {
  it('sends rebuildApplicationMenu IPC channel message', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send).toHaveBeenCalledTimes(1);
    expect(send).toHaveBeenNthCalledWith(1, {
      isNavigationEnabled: expect.any(Boolean),
      walletSettingsState: expect.stringMatching(
        new RegExp(
          `(${WalletSettingsStateEnum.enabled}|${WalletSettingsStateEnum.disabled}|${WalletSettingsStateEnum.hidden})`
        )
      ),
    });
  });

  it('sends isNavigationEnabled value according to termsOfUseAcceptance', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile: makeProfile({
          areTermsOfUseAccepted: true,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send.mock.calls[0][0]).toHaveProperty('isNavigationEnabled', true);
  });

  it('mentions the profile.currentLocale in order to inform mobx autorun to watch on that property', () => {
    let currentLocaleMentioned = false;
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile: makeProfile({
          currentLocaleCallback: () => {
            currentLocaleMentioned = true;
          },
        }),
      },
    })();

    expect(currentLocaleMentioned).toEqual(true);
  });

  it('sends walletSettingsState hidden when it is not a wallet page', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: '/not-a-wallet-page',
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.hidden
    );
  });

  it('sends walletSettingsState hidden when wallet settings route is already active', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.SETTINGS,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.hidden
    );
  });

  it('sends walletSettingsState enabled when it is one of a wallet pages', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.enabled
    );
  });

  it('sends walletSettingsState disabled when it is one of a wallet pages but a dialog is open', () => {
    const send = jest.fn();
    makeReactionCallback({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
        uiDialogs: makeUiDialogs({ activeDialog: true }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    })();

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.disabled
    );
  });
});
