// @flow

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

const makeApp = ({ activeDialog = false } = {}): AppStore =>
  ({ activeDialog: activeDialog ? 'SOME_DIALOG' : null }: any);
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
const makeStaking = ({ redeemOpen = false } = {}): RouterStore =>
  ({
    redeemStep: redeemOpen ? REDEEM_ITN_REWARDS_STEPS.CONFIGURATION : null,
  }: any);
const makeUiDialogs = ({ activeDialog = false } = {}): UiDialogsStore =>
  ({
    activeDialog: activeDialog ? () => {} : null,
  }: any);
const makeRebuildApplicationMenu = ({
  send = () => {},
} = {}): RebuildApplicationMenu => ({ send }: any);

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

describe('useMenuUpdater', () => {
  it('sends rebuildApplicationMenu IPC channel message', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send).toHaveBeenCalledTimes(1);
    expect(send).toHaveBeenNthCalledWith(1, {
      isNavigationEnabled: expect.any(Boolean),
      walletSettingsState: expect.any(String),
    });
  });

  it('sends isNavigationEnabled value according to termsOfUseAcceptance', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile: makeProfile({
          areTermsOfUseAccepted: true,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send.mock.calls[0][0]).toHaveProperty('isNavigationEnabled', true);
  });

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

    expect(currentLocaleMentioned).toEqual(true);
  });

  it('sends walletSettingsState hidden when it is not a wallet page', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: '/not-a-wallet-page',
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.hidden
    );
  });

  it('sends walletSettingsState enabled when it is one of a wallet pages', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.enabled
    );
  });

  it('sends walletSettingsState disabled when it is one of a wallet pages but a dialog is open', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.ADD,
        }),
        uiDialogs: makeUiDialogs({ activeDialog: true }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.disabled
    );
  });

  [
    { name: 'app', storesOverride: { app: makeApp({ activeDialog: true }) } },
    {
      name: 'Redeem Rewards',
      storesOverride: { staking: makeStaking({ redeemOpen: true }) },
    },
    {
      name: 'other',
      storesOverride: { uiDialogs: makeUiDialogs({ activeDialog: true }) },
    },
  ].forEach(({ name, storesOverride }) => {
    it(`sends walletSettingsState disabled when it is one of a wallet pages but ${name} dialog is open`, () => {
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
        rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
      });

      expect(send.mock.calls[0][0]).toHaveProperty(
        'walletSettingsState',
        WalletSettingsStateEnum.disabled
      );
    });
  });

  it('sends walletSettingsState disabled when wallet settings route is already active', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: ROUTES.WALLETS.SETTINGS,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({ send }),
    });

    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      WalletSettingsStateEnum.disabled
    );
  });
});
