'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = require('@testing-library/react');
const react_2 = __importDefault(require('react'));
const api_1 = require('../../../../common/ipc/api');
const stakingConfig_1 = require('../../config/stakingConfig');
const useMenuUpdater_1 = __importDefault(require('./useMenuUpdater'));
const routes_config_1 = require('../../routes-config');
const makeApp = ({ activeDialog = false } = {}) => ({
  activeDialog: activeDialog ? 'SOME_DIALOG' : null,
});
const makeProfile = ({
  currentLocale = 'en',
  areTermsOfUseAccepted = false,
} = {}) => ({
  currentLocale,
  areTermsOfUseAccepted,
});
const makeRouter = ({
  pathname = routes_config_1.ROUTES.WALLETS.ROOT,
} = {}) => ({
  location: {
    pathname,
  },
});
const makeStaking = ({ redeemOpen = false } = {}) => ({
  redeemStep: redeemOpen
    ? stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIGURATION
    : null,
});
const makeUiDialogs = ({ activeDialog = false } = {}) => ({
  activeDialog: activeDialog ? () => {} : null,
});
const makeRebuildApplicationMenu = ({ send = () => {} } = {}) => ({
  send,
});
function Component(props) {
  (0, useMenuUpdater_1.default)(props);
  return null;
}
const renderComponent = (props) => {
  const { rerender } = (0, react_1.render)(
    react_2.default.createElement(Component, { ...props })
  );
  return {
    rerender: (nextProps) =>
      rerender(react_2.default.createElement(Component, { ...nextProps })),
  };
};
const defaultArgs = {
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
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
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
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    expect(send.mock.calls[0][0]).toHaveProperty('isNavigationEnabled', true);
  });
  it('watches on the profile.currentLocale property changes', () => {
    const send = jest.fn();
    const makeArgs = (profile) => ({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        profile,
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    const { rerender } = renderComponent(makeArgs(makeProfile()));
    rerender(makeArgs(makeProfile({ currentLocale: 'jp' })));
    expect(send).toHaveBeenCalledTimes(2);
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
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      api_1.WalletSettingsStateEnum.hidden
    );
  });
  it('sends walletSettingsState enabled when it is one of a wallet pages', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: routes_config_1.ROUTES.WALLETS.ADD,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      api_1.WalletSettingsStateEnum.enabled
    );
  });
  it('sends walletSettingsState disabled when it is one of a wallet pages but a dialog is open', () => {
    const send = jest.fn();
    renderComponent({
      ...defaultArgs,
      stores: {
        ...defaultArgs.stores,
        router: makeRouter({
          pathname: routes_config_1.ROUTES.WALLETS.ADD,
        }),
        uiDialogs: makeUiDialogs({
          activeDialog: true,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      api_1.WalletSettingsStateEnum.disabled
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
    it(`sends walletSettingsState disabled when it is one of a wallet pages but ${name} dialog is open`, () => {
      const send = jest.fn();
      renderComponent({
        ...defaultArgs,
        stores: {
          ...defaultArgs.stores,
          router: makeRouter({
            pathname: routes_config_1.ROUTES.WALLETS.ADD,
          }),
          ...storesOverride,
        },
        rebuildApplicationMenu: makeRebuildApplicationMenu({
          send,
        }),
      });
      expect(send.mock.calls[0][0]).toHaveProperty(
        'walletSettingsState',
        api_1.WalletSettingsStateEnum.disabled
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
          pathname: routes_config_1.ROUTES.WALLETS.SETTINGS,
        }),
      },
      rebuildApplicationMenu: makeRebuildApplicationMenu({
        send,
      }),
    });
    expect(send.mock.calls[0][0]).toHaveProperty(
      'walletSettingsState',
      api_1.WalletSettingsStateEnum.disabled
    );
  });
});
//# sourceMappingURL=useMenuUpdater.spec.js.map
