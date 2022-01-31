// @flow
import { action } from 'mobx';
import { get } from 'lodash';
import BigNumber from 'bignumber.js';
import AdaApi from './api';
import { logger } from '../utils/logging';
import ApiError from '../domains/ApiError';

// domains
import Wallet, {
  WalletUnits,
} from '../domains/Wallet';

// Config constants
import { LOVELACES_PER_ADA } from '../config/numbersConfig';

import type { AdaWallet } from './wallets/types'

import {
  createWalletChannel,
} from '../ipc/getLightWalletChannel';


export default (api: AdaApi) => {
  api.lightWalletTest = async () => {
    console.debug('>>> [LightWallets] lightWalletTest')
    return 'lightWalletAPI Test';
  };

  api.getWallets = async () => {
    /* const wallet = await createWalletChannel.request();
    console.debug('[LightWallets] Create Light Wallet - SUCCESS', {
      response: wallet,
      responseJSON: JSON.stringify(wallet),
    }); */

    // Mocked from real data
    const mockedWalletJSON = '{"id":"efa6173d5763692eab8958b26066ec7b2507b6f7","name":"Test Wallet","address_pool_gap":20,"balance":{"available":{"quantity":0,"unit":"lovelace"},"total":{"quantity":0,"unit":"lovelace"},"reward":{"quantity":0,"unit":"lovelace"}},"assets":{"available":[{"policy_id":"65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b","asset_name":"test_asset","quantity":0}],"total":[{"policy_id":"65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b","asset_name":"test_asset","quantity":0}]},"passphrase":{"last_updated_at":1643382680401},"delegation":{"active":{"status":"delegating","target":"1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"}},"state":{"status":"ready"},"isLegacy":false,"discovery":"sequential","isHardwareWallet":false,"isLightWallet":true}';
    const mockedWallet = JSON.parse(mockedWalletJSON);
    console.debug('>>> [LightWallets] getWallets: ', {
      wallets: [mockedWallet],
    })

    const wallet = _createWalletFromServerData(mockedWallet);

    console.debug('>>> Constructed Wallet: ', wallet);

    return [wallet];
  };

  api.getAssets = async () => {
    return [];
  }

  const _createWalletFromServerData = action(
    'AdaApi::_createWalletFromServerData',
    (wallet: AdaWallet) => {
      const {
        id,
        address_pool_gap: addressPoolGap,
        balance,
        name,
        assets,
        passphrase,
        delegation,
        state: syncState,
        isLegacy = false,
        discovery,
        isHardwareWallet = false,
      } = wallet;

      const passphraseLastUpdatedAt = get(passphrase, 'last_updated_at', null);
      const walletTotalAmount =
        balance.total.unit === WalletUnits.LOVELACE
          ? new BigNumber(balance.total.quantity.toString()).dividedBy(
              LOVELACES_PER_ADA
            )
          : new BigNumber(balance.total.quantity.toString());
      const walletAvailableAmount =
        balance.available.unit === WalletUnits.LOVELACE
          ? new BigNumber(balance.available.quantity.toString()).dividedBy(
              LOVELACES_PER_ADA
            )
          : new BigNumber(balance.available.quantity.toString());
      let walletRewardAmount = new BigNumber(0);
      if (!isLegacy) {
        walletRewardAmount =
          balance.reward.unit === WalletUnits.LOVELACE
            ? new BigNumber(balance.reward.quantity.toString()).dividedBy(
                LOVELACES_PER_ADA
              )
            : new BigNumber(balance.reward.quantity.toString());
      }

      // Current (Active)
      const active = get(delegation, 'active', null);
      const target = get(active, 'target', null);
      const status = get(active, 'status', null);
      const delegatedStakePoolId = isLegacy ? null : target;
      const delegationStakePoolStatus = isLegacy ? null : status;

      // Last
      const next = get(delegation, 'next', null);
      const lastPendingStakePool = next ? last(next) : null;
      const lastTarget = get(lastPendingStakePool, 'target', null);
      const lastStatus = get(lastPendingStakePool, 'status', null);
      const lastDelegatedStakePoolId = isLegacy ? null : lastTarget;
      const lastDelegationStakePoolStatus = isLegacy ? null : lastStatus;

      // Mapping asset items from server data
      const walletAssets = {
        available: assets.available.map((item) => {
          const { policy_id: policyId, asset_name: assetName, quantity } = item;
          const uniqueId = `${policyId}${assetName}`;
          return {
            uniqueId,
            policyId,
            assetName,
            quantity: new BigNumber(quantity.toString()),
          };
        }),
        total: assets.total.map((item) => {
          const { policy_id: policyId, asset_name: assetName, quantity } = item;
          const uniqueId = `${policyId}${assetName}`;
          return {
            uniqueId,
            policyId,
            assetName,
            quantity: new BigNumber(quantity.toString()),
          };
        }),
      };

      return new Wallet({
        id,
        addressPoolGap,
        name,
        amount: walletTotalAmount,
        availableAmount: walletAvailableAmount,
        reward: walletRewardAmount,
        assets: walletAssets,
        passwordUpdateDate:
          passphraseLastUpdatedAt && new Date(passphraseLastUpdatedAt),
        hasPassword: isHardwareWallet || passphraseLastUpdatedAt !== null, // For HW set that wallet has password
        syncState,
        isLegacy,
        isHardwareWallet,
        delegatedStakePoolId,
        delegationStakePoolStatus,
        lastDelegatedStakePoolId,
        lastDelegationStakePoolStatus,
        pendingDelegations: next,
        discovery,
      });
    }
  );
};
