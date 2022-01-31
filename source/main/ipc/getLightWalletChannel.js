// @flow
import { BrowserWindow } from 'electron';
import { get} from 'lodash';
import BigNumber from 'bignumber.js';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { logger } from '../utils/logging';
import {
  CREATE_WALLET_CHANNEL,
} from '../../common/ipc/lightWalletApi';

import type {
  createWalletRendererRequest,
  createWalletMainResponse,
} from '../../common/ipc/lightWalletApi';

import type { BIP32Path } from '../../common/types/light-wallets.types';

import { SingleAddressWallet, KeyManagement } from '@cardano-sdk/wallet';
import { blockfrostAssetProvider, blockfrostWalletProvider } from '@cardano-sdk/blockfrost';
import { AssetId, createStubStakePoolSearchProvider } from '@cardano-sdk/util-dev';


const createWalletChannel: MainIpcChannel<
  createWalletRendererRequest,
  createWalletMainResponse
> = new MainIpcChannel(CREATE_WALLET_CHANNEL);

let light_wallet = {}

export const handleLightWalletRequests = async (
  mainWindow: BrowserWindow
) => {
  let deviceConnection = null;
  let observer;

  // TODO: Remove next line
  const networkId = 0;
  const isTestnet = networkId === 0;
  // TODO: CHANGE next line and hide keys
  let projectId = 'BLOCKFROST_API_KEY';

  const testSingleAddressWallet = async () => {
    // Log SDK
    console.debug('>>> SDK::Classes: ', {
      Wallet_Class: SingleAddressWallet,
      KeyManager_Class: KeyManagement
    });
    // End of Log SDK

    let walletProvider;
    let assetProvider;
    let stakePoolSearchProvider;
    try {
      console.debug('>>> SDK::testSingleAddressWallet CALLED');

      // Construct Wallet

      // 1. Construct Key Manager
      console.debug('>>> SDK::Construct Key Manager');
      const walletProps = { name: 'tomotestwallet' }; // type SingleAddressWalletProps
      // const mnemonicWords = KeyManagement.util.generateMnemonicWords();
      const mnemonicWords = [
        'immune',
        'soul',
        'bus',
        'unfair',
        'medal',
        'public',
        'bus',
        'pupil',
        'surprise',
        'wish',
        'when',
        'thunder',
        'fit',
        'youth',
        'unveil',
        'rocket',
        'suit',
        'empower',
        'heavy',
        'night',
        'voyage',
        'glare',
        'trade',
        'pyramid'
      ]; // address is "addr_test1qqnl3q7xlnzgjpyt3zy6arajjz52mkrsn3z3ld9wnckcvsyny332me0mkgnxeq7qlm223n2nj6kprsxzuzsxxj9du00ssrvqw0"
      const password = 'Secret1234';
      const keyManager = KeyManagement.createInMemoryKeyManager({
        mnemonicWords,
        networkId,
        password,
      });
      console.debug('>>> SDK::Construct Key Manager - Done');

      // 2. Construct Providers
      console.debug('>>> SDK::Construct Providers');
      walletProvider = blockfrostWalletProvider({ isTestnet: true, projectId });
      assetProvider = blockfrostAssetProvider({ isTestnet: true, projectId });
      stakePoolSearchProvider = createStubStakePoolSearchProvider();
      console.debug('>>> SDK::Construct Providers - Done');

      // 3. Create / Restore Wallet
      console.debug('>>> SDK:: Restore wallet');
      const singleAddressWallet = new SingleAddressWallet(
        { name: 'Test Wallet' },
        {
          assetProvider,
          keyManager,
          stakePoolSearchProvider,
          walletProvider,
        }
      );
      console.debug('>>> SDK:: Restore wallet - Done');

      // 4. Log some Wallet data
      console.debug('>>> SDK:: singleAddressWallet - Constructed: ', {
        singleAddressWallet_Data: singleAddressWallet,
      });

      console.debug('>>> SDK:: Wallet data', {
        addresses: singleAddressWallet.addresses$.value,
        balance: singleAddressWallet.balance.total$.value, // Needs to be subscribed
      })

      const wallet = createWalletFromSdkData(singleAddressWallet);
      console.debug('>>> SDK:: WALLET: ', {
        wallet,
      });
      return wallet;
    } catch (e) {
      console.debug('>>> SDK:: FAILED: ', e);
      throw e;
    }
  };

  const genRanHex = size => [...Array(size)].map(() => Math.floor(Math.random() * 16).toString(16)).join('');

  const createWalletFromSdkData = (singleAddressWallet) => {
    // Types from api/wallet/types
    // type AdaWallet = {
    //   id: string,
    //   address_pool_gap: number,
    //   balance: {
    //     available: WalletBalance,
    //     total: WalletBalance,
    //     reward: WalletBalance,
    //   },
    //   assets: {
    //     available: ApiTokens,
    //     total: ApiTokens,
    //   },
    //   delegation: {
    //     active: WalletDelegation,
    //     next?: WalletNextDelegation,
    //   },
    //   name: string, -> DONE
    //   passphrase?: {
    //     last_updated_at: string,
    //   },
    //   state: WalletSyncState,
    //   discovery: Discovery,
    //   isLegacy: boolean,
    //   isHardwareWallet?: boolean,
    // };

    // Mocked Data
    const currentTimestamp = new Date().getTime().toString(16);
    const mockedBalance = {
      quantity: 0,
      unit: 'lovelace',
    };
    const mockedAssets = [{
      policy_id: '65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      asset_name: 'test_asset',
      quantity: 0,
      // address: null,
    }];
    const mockedDelegation = {
      active: {
        status: 'delegating',
        target: '1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1'
      },
      // next: {
      //   status: 'not_delegating',
      //   changes_at: {
      //     epoch_number: 14,
      //     epoch_start_time: '2020-01-22T10:06:39.037Z',
      //   },
      // },
    };

    const walletData = {
      id: genRanHex(40),
      name: singleAddressWallet.name,
      address_pool_gap: 20, // TODO - check this (Number of consecutive unused addresses allowed)
      balance: {
        available: mockedBalance,
        total: mockedBalance,
        reward: mockedBalance,
      },
      assets: {
        available: mockedAssets,
        total: mockedAssets,
      },
      passphrase: {
        last_updated_at: currentTimestamp,
      },
      delegation: mockedDelegation,
      state: {
        status: 'ready',
      },
      isLegacy: false,
      discovery: 'sequential', // TODO - check this
      isHardwareWallet: false,
      isLightWallet: true
    };

    console.debug('>>> walletData: ', walletData)
    console.debug('>>> walletData - JSON: ', JSON.stringify(walletData))

    // TODO - remove mocked data
    return walletData;
  };

  // {"id":1643382680401,"name":"Test Wallet","address_pool_gap":20,"balance":{"available":{"quantity":0,"unit":"lovelace"},"total":{"quantity":0,"unit":"lovelace"},"reward":{"quantity":0,"unit":"lovelace"}},"assets":{"available":[{"policy_id":"65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b","asset_name":"test_asset","quantity":0}],"total":[{"policy_id":"65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b","asset_name":"test_asset","quantity":0}]},"passphrase":{"last_updated_at":1643382680401},"delegation":{"active":{"status":"delegating","target":"1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"}},"state":{"status":"ready"},"isLegacy":false,"discovery":"sequential","isHardwareWallet":false,"isLightWallet":true}
  createWalletChannel.onRequest(async () => {
    console.debug('>>> SDK::Create Light Wallet');
    logger.info('>>> SDK::Create Light Wallet');
    try {
      const singleAddressWallet = await testSingleAddressWallet();
      console.debug('>>> SDK::Create Light Wallet SUCCESS', singleAddressWallet);
      logger.info('>>> SDK::Create Light Wallet SUCCESS', singleAddressWallet);
      return singleAddressWallet;
      // return 'success';
    } catch (e) {
      console.debug('>>> SDK::Create Light Wallet FAILED: ', e);
      logger.info('>>> SDK::Create Light Wallet FAILED: ', e);
      throw e;
    }
  });
};
