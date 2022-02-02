import { render } from '@testing-library/react';
import { RouterStore } from 'mobx-react-router';
import React from 'react';
import { WalletSettingsStateEnum } from '../../../../common/ipc/api';
import { REDEEM_ITN_REWARDS_STEPS } from '../../config/stakingConfig';
import type { RebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import AppStore from '../../stores/AppStore';
import ProfileStore from '../../stores/ProfileStore';
import UiDialogsStore from '../../stores/UiDialogsStore';
import useMenuUpdater from './useMenuUpdater';
import { ROUTES } from '../../routes-config';
import type { UseMenuUpdaterArgs } from './types';
import StakingStore from '../../stores/StakingStore';

const makeApp = ({ activeDialog = false } = {}): AppStore =>
  ({
    activeDialog: activeDialog ? 'SOME_DIALOG' : null,
  } as any);

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
  } as any);

const makeRouter = ({ pathname = ROUTES.WALLETS.ROOT } = {}): RouterStore =>
  ({
    location: {
      pathname,
    },
  } as any);

const makeStaking = ({ redeemOpen = false } = {}): StakingStore =>
  ({
    redeemStep: redeemOpen ? REDEEM_ITN_REWARDS_STEPS.CONFIGURATION : null,
  } as any);

const makeUiDialogs = ({ activeDialog = false } = {}): UiDialogsStore =>
  ({
    activeDialog: activeDialog ? () => {} : null,
  } as any);

const makeRebuildApplicationMenu = ({
  send = () => {},
} = {}): RebuildApplicationMenu =>
  ({
    send,
  } as any);

const renderComponent = (args) => {
  const Component = () => {
    useMenuUpdater(args);
    return null;
  };

  render(<Component />);
};

const defaultArgs: UseMenuUpdaterArgs = {
  stores: {
    app: makeApp(),
    profile: makeProfile(),
    router: makeRouter(),
    staking: makeStaking(),
    uiDialogs: makeUiDialogs(),
  },
  rebuildApplicationMenu: makeRebuildApplicationMenu(),
};
// @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'describe'. Do you need to instal... Remove this comment to see the full error message
describe('useMenuUpdater', () => {
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends rebuildApplicationMenu IPC channel message', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send).toHaveBeenCalledTimes(1);
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send).toHaveBeenNthCalledWith(1, {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
      isNavigationEnabled: expect.any(Boolean),
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
      walletSettingsState: expect.any(String),
    });
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends isNavigationEnabled value according to termsOfUseAcceptance', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile: makeProfile({
          areTermsOfUseAccepted: true,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send.mock.calls[0][0]).toHaveProperty('isNavigationEnabled', true);
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('watches on the profile.currentLocale property changes', () => {
    let currentLocaleMentioned = false;
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile: makeProfile({
          currentLocaleCallback: () => {
            currentLocaleMentioned = true;
          },
        }),
      },
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(currentLocaleMentioned).toEqual(true);
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends walletSettingsState hidden when it is not a wallet page', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: '/not-a-wallet-page',
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.hidden
    );
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends walletSettingsState enabled when it is one of a wallet pages', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.enabled
    );
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends walletSettingsState disabled when it is one of a wallet pages but a dialog is open', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
        uiDialogs: makeUiDialogs({
          activeDialog: true,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.disabled
    );
  });
  [
    {
      name: 'app',
      storesOverride: {
        app: makeApp({
          activeDialog: true,
        }),
      },
    },
    {
      name: 'Redeem Rewards',
      storesOverride: {
        staking: makeStaking({
          redeemOpen: true,
        }),
      },
    },
    {
      name: 'other',
      storesOverride: {
        uiDialogs: makeUiDialogs({
          activeDialog: true,
        }),
      },
    },
  ].forEach(({ name, storesOverride }) => {
    // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
    it(`sends walletSettingsState disabled when it is one of a wallet pages but ${name} dialog is open`, () => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
      const send = jest.fn();
      renderComponent({
        ...defaultArgs,
        stores: {
          ...defaultArgs.stores,
          router: makeRouter({
            pathname: ROUTES.WALLETS.ADD,
          }),
          ...storesOverride,
        },
        rebuildApplicationMenu: makeRebuildApplicationMenu({
          send,
        }),
      });
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
      expect(send.mock.calls[0][0]).toHaveProperty(
        'walletSettingsState',
        WalletSettingsStateEnum.disabled
      );
    });
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('sends walletSettingsState disabled when wallet settings route is already active', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'jest'.
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.SETTINGS,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.disabled
    );
  });
});
