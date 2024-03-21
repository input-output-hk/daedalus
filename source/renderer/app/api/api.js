'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const lodash_1 = require('lodash');
const mobx_1 = require('mobx');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const moment_1 = __importDefault(require('moment'));
// domains
const Wallet_1 = __importStar(require('../domains/Wallet'));
const WalletTransaction_1 = require('../domains/WalletTransaction');
const WalletAddress_1 = __importDefault(require('../domains/WalletAddress'));
// Addresses requests
const getAddresses_1 = require('./addresses/requests/getAddresses');
const getByronWalletAddresses_1 = require('./addresses/requests/getByronWalletAddresses');
const createByronWalletAddress_1 = require('./addresses/requests/createByronWalletAddress');
const constructAddress_1 = require('./addresses/requests/constructAddress');
const inspectAddress_1 = require('./addresses/requests/inspectAddress');
// Network requests
const getNetworkInfo_1 = require('./network/requests/getNetworkInfo');
const getNetworkClock_1 = require('./network/requests/getNetworkClock');
const getNetworkParameters_1 = require('./network/requests/getNetworkParameters');
// Transactions requests
const getTransactionFee_1 = require('./transactions/requests/getTransactionFee');
const getByronWalletTransactionFee_1 = require('./transactions/requests/getByronWalletTransactionFee');
const getTransaction_1 = require('./transactions/requests/getTransaction');
const getTransactionHistory_1 = require('./transactions/requests/getTransactionHistory');
const getLegacyWalletTransactionHistory_1 = require('./transactions/requests/getLegacyWalletTransactionHistory');
const getWithdrawalHistory_1 = require('./transactions/requests/getWithdrawalHistory');
const createTransaction_1 = require('./transactions/requests/createTransaction');
const createByronWalletTransaction_1 = require('./transactions/requests/createByronWalletTransaction');
const deleteLegacyTransaction_1 = require('./transactions/requests/deleteLegacyTransaction');
const selectCoins_1 = require('./transactions/requests/selectCoins');
const createExternalTransaction_1 = require('./transactions/requests/createExternalTransaction');
const getPublicKey_1 = require('./transactions/requests/getPublicKey');
const getICOPublicKey_1 = require('./transactions/requests/getICOPublicKey');
// Voting requests
const createWalletSignature_1 = require('./voting/requests/createWalletSignature');
const getCatalystFund_1 = require('./voting/requests/getCatalystFund');
// Wallets requests
const updateSpendingPassword_1 = require('./wallets/requests/updateSpendingPassword');
const updateByronSpendingPassword_1 = require('./wallets/requests/updateByronSpendingPassword');
const deleteWallet_1 = require('./wallets/requests/deleteWallet');
const deleteLegacyWallet_1 = require('./wallets/requests/deleteLegacyWallet');
const exportWalletAsJSON_1 = require('./wallets/requests/exportWalletAsJSON');
const importWalletAsJSON_1 = require('./wallets/requests/importWalletAsJSON');
const getWallets_1 = require('./wallets/requests/getWallets');
const getLegacyWallets_1 = require('./wallets/requests/getLegacyWallets');
const importWalletAsKey_1 = require('./wallets/requests/importWalletAsKey');
const createWallet_1 = require('./wallets/requests/createWallet');
const restoreWallet_1 = require('./wallets/requests/restoreWallet');
const restoreLegacyWallet_1 = require('./wallets/requests/restoreLegacyWallet');
const restoreByronWallet_1 = require('./wallets/requests/restoreByronWallet');
const restoreExportedByronWallet_1 = require('./wallets/requests/restoreExportedByronWallet');
const updateWallet_1 = require('./wallets/requests/updateWallet');
const updateByronWallet_1 = require('./wallets/requests/updateByronWallet');
const getWalletUtxos_1 = require('./wallets/requests/getWalletUtxos');
const getByronWalletUtxos_1 = require('./wallets/requests/getByronWalletUtxos');
const getWallet_1 = require('./wallets/requests/getWallet');
const getWalletPublicKey_1 = require('./wallets/requests/getWalletPublicKey');
const getLegacyWallet_1 = require('./wallets/requests/getLegacyWallet');
const transferFundsCalculateFee_1 = require('./wallets/requests/transferFundsCalculateFee');
const transferFunds_1 = require('./wallets/requests/transferFunds');
const createHardwareWallet_1 = require('./wallets/requests/createHardwareWallet');
const getCurrencyList_1 = require('./wallets/requests/getCurrencyList');
const getCurrencyRate_1 = require('./wallets/requests/getCurrencyRate');
// Staking
const StakePool_1 = __importDefault(require('../domains/StakePool'));
// News requests
const getNews_1 = require('./news/requests/getNews');
// Stake Pools request
const getStakePools_1 = require('./staking/requests/getStakePools');
const getDelegationFee_1 = require('./staking/requests/getDelegationFee');
const joinStakePool_1 = require('./staking/requests/joinStakePool');
const quitStakePool_1 = require('./staking/requests/quitStakePool');
const getSmashSettings_1 = require('./staking/requests/getSmashSettings');
const checkSmashServerHealth_1 = require('./staking/requests/checkSmashServerHealth');
const updateSmashSettings_1 = require('./staking/requests/updateSmashSettings');
// Utility functions
const cardano_ipc_1 = require('../ipc/cardano.ipc');
const patchAdaApi_1 = __importDefault(require('./utils/patchAdaApi'));
const utils_1 = require('./utils');
const logging_1 = require('../utils/logging');
const strings_1 = require('../utils/strings');
const mnemonics_1 = require('./utils/mnemonics');
const logging_2 = require('../../../common/utils/logging');
const hardwareWalletUtils_1 = require('../utils/hardwareWalletUtils');
// Config constants
const numbersConfig_1 = require('../config/numbersConfig');
const stakingConfig_1 = require('../config/stakingConfig');
const cryptoConfig_1 = require('../config/cryptoConfig');
const currencyConfig_1 = require('../config/currencyConfig');
const errors_1 = require('./nodes/errors');
const hashing_1 = require('./utils/hashing');
const getNewsHash_1 = require('./news/requests/getNewsHash');
const deleteTransaction_1 = require('./transactions/requests/deleteTransaction');
const walletRestoreConfig_1 = require('../config/walletRestoreConfig');
const ApiError_1 = __importDefault(require('../domains/ApiError'));
const formatters_1 = require('../utils/formatters');
const Asset_1 = __importDefault(require('../domains/Asset'));
const getAssets_1 = require('./assets/requests/getAssets');
const getAccountPublicKey_1 = require('./wallets/requests/getAccountPublicKey');
const apiHelpers_1 = require('./utils/apiHelpers');
const errors_2 = require('./errors');
class AdaApi {
  config;
  // We need to preserve all asset metadata during single runtime in order
  // to avoid losing it in case of Token Metadata Registry server unavailability
  storedAssetMetadata = {};
  constructor(isTest, config) {
    this.setRequestConfig(config);
    if (isTest) (0, patchAdaApi_1.default)(this);
  }
  setRequestConfig(config) {
    this.config = config;
  }
  getWallets = async () => {
    logging_1.logger.debug('AdaApi::getWallets called');
    const { getHardwareWalletsLocalData } = global.daedalus.api.localStorage;
    try {
      const wallets = await (0, getWallets_1.getWallets)(this.config);
      const legacyWallets = await (0, getLegacyWallets_1.getLegacyWallets)(
        this.config
      );
      const hwLocalData = await getHardwareWalletsLocalData();
      logging_1.logger.debug('AdaApi::getWallets success', {
        wallets,
        legacyWallets,
        hwLocalData: (0, logging_2.filterLogData)(hwLocalData),
      });
      (0, lodash_1.map)(legacyWallets, (legacyAdaWallet) => {
        const extraLegacyWalletProps = {
          address_pool_gap: 0,
          // Not needed for legacy wallets
          delegation: {
            active: {
              status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
        wallets.push({ ...legacyAdaWallet, ...extraLegacyWalletProps });
      });
      // @TODO - Remove this once we get hardware wallet flag from WBE
      return await Promise.all(
        wallets.map(async (wallet) => {
          const { id } = wallet;
          const walletData = hwLocalData[id];
          return _createWalletFromServerData({
            ...wallet,
            isHardwareWallet:
              walletData &&
              walletData.device &&
              (0, lodash_1.size)(walletData.device) > 0,
          });
        })
      );
    } catch (error) {
      logging_1.logger.error('AdaApi::getWallets error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getWallet = async (request) => {
    logging_1.logger.debug('AdaApi::getWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const { walletId, isLegacy } = request;
      let wallet;
      if (isLegacy) {
        const legacyWallet = await (0, getLegacyWallet_1.getLegacyWallet)(
          this.config,
          {
            walletId,
          }
        );
        const extraLegacyWalletProps = {
          address_pool_gap: 0,
          // Not needed for legacy wallets
          delegation: {
            active: {
              status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
        wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      } else {
        wallet = await (0, getWallet_1.getWallet)(this.config, {
          walletId,
        });
      }
      logging_1.logger.debug('AdaApi::getWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::getWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getWalletPublicKey = async (request) => {
    logging_1.logger.debug('AdaApi::getWalletPublicKey called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const { walletId, role, index } = request;
      const walletPublicKey = await (0,
      getWalletPublicKey_1.getWalletPublicKey)(this.config, {
        walletId,
        role,
        index,
      });
      logging_1.logger.debug('AdaApi::getWalletPublicKey success', {
        walletPublicKey,
      });
      return walletPublicKey;
    } catch (error) {
      logging_1.logger.error('AdaApi::getWalletPublicKey error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getAccountPublicKey = async (request) => {
    logging_1.logger.debug('AdaApi::getAccountPublicKey called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const { walletId, index, passphrase, extended } = request;
      const accountPublicKey = await (0,
      getAccountPublicKey_1.getAccountPublicKey)(this.config, {
        walletId,
        index,
        passphrase,
        extended,
      });
      logging_1.logger.debug('AdaApi::getAccountPublicKey success', {
        accountPublicKey,
      });
      return accountPublicKey;
    } catch (error) {
      logging_1.logger.error('AdaApi::getAccountPublicKey error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  getAddresses = async (request) => {
    logging_1.logger.debug('AdaApi::getAddresses called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, queryParams, isLegacy } = request;
    try {
      let response = [];
      if (isLegacy) {
        response = await (0, getByronWalletAddresses_1.getByronWalletAddresses)(
          this.config,
          walletId,
          queryParams
        );
      } else {
        response = await (0, getAddresses_1.getAddresses)(
          this.config,
          walletId,
          queryParams
        );
        response.reverse();
      }
      logging_1.logger.debug('AdaApi::getAddresses success', {
        addresses: response,
      });
      return response.map(_createAddressFromServerData);
    } catch (error) {
      logging_1.logger.error('AdaApi::getAddresses error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getTransaction = async (request) => {
    logging_1.logger.debug('AdaApi::getTransaction called', {
      parameters: request,
    });
    const { walletId, transactionId } = request;
    try {
      const response = await (0, getTransaction_1.getTransaction)(
        this.config,
        walletId,
        transactionId
      );
      logging_1.logger.debug('AdaApi::getTransaction success', {
        response,
      });
      return _createTransactionFromServerData(response);
    } catch (error) {
      logging_1.logger.error('AdaApi::getTransaction error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getTransactions = async (request) => {
    logging_1.logger.debug('AdaApi::getTransactions called', {
      parameters: request,
    });
    const { walletId, order, fromDate, toDate, isLegacy } = request;
    const params = Object.assign(
      {},
      {
        order: order || 'descending',
      },
      !!fromDate && {
        start: `${moment_1.default
          .utc(fromDate)
          .format('YYYY-MM-DDTHH:mm:ss')}Z`,
      },
      !!toDate && {
        end: `${moment_1.default.utc(toDate).format('YYYY-MM-DDTHH:mm:ss')}Z`,
      }
    );
    try {
      let response;
      if (isLegacy) {
        response = await (0,
        getLegacyWalletTransactionHistory_1.getLegacyWalletTransactionHistory)(
          this.config,
          walletId,
          params
        );
      } else {
        response = await (0, getTransactionHistory_1.getTransactionHistory)(
          this.config,
          walletId,
          params
        );
      }
      logging_1.logger.debug('AdaApi::getTransactions success', {
        transactions: response,
      });
      const transactions = response.map((tx) =>
        _createTransactionFromServerData(tx)
      );
      return Promise.resolve({
        transactions,
        total: response.length,
      });
    } catch (error) {
      logging_1.logger.error('AdaApi::getTransactions error', {
        error,
      });
      throw new ApiError_1.default(error);
    } // @API TODO - Filter / Search fine tuning "pending" for V2
    // const requestStats = Object.assign({}, request, {
    //   cachedTransactions: request.cachedTransactions.length,
    // });
    //  logger.debug('AdaApi::searchHistory called', { parameters: requestStats });
    // const requestTimestamp = moment();
    // const params = {
    //   wallet_id: walletId,
    //   page: skip === 0 ? 1 : skip + 1,
    //   per_page: perPage,
    //   sort_by: 'DES[created_at]',
    //   created_at: `LTE[${moment.utc().format('YYYY-MM-DDTHH:mm:ss')}]`,
    //   // ^^ By setting created_at filter to current time we make sure
    //   // all subsequent multi-pages requests load the same set of transactions
    // };
    //
    //
    // const {
    //   walletId,
    //   skip,
    //   limit,
    //   isFirstLoad, // during first load we fetch all wallet's transactions
    //   isRestoreActive, // during restoration we fetch only missing transactions
    //   isRestoreCompleted, // once restoration is done we fetch potentially missing transactions
    //   cachedTransactions,
    // } , unionBy= request;
    //
    //
    // let perPage = limit;
    // const shouldLoadAll = limit === null;
    // if (shouldLoadAll || limit > MAX_TRANSACTIONS_PER_PAGE) {
    //   perPage = MAX_TRANSACTIONS_PER_PAGE;
    // }
    //
    // const params = {
    //   wallet_id: walletId,
    //   page: skip === 0 ? 1 : skip + 1,
    //   per_page: perPage,
    //   sort_by: 'DES[created_at]',
    //   created_at: `LTE[${moment.utc().format('YYYY-MM-DDTHH:mm:ss')}]`,
    //   // ^^ By setting created_at filter to current time we make sure
    //   // all subsequent multi-pages requests load the same set of transactions
    // };
    //
    // const shouldLoadOnlyFresh =
    //   !isFirstLoad && !isRestoreActive && !isRestoreCompleted;
    // if (shouldLoadOnlyFresh) {
    //   const tenMinutesAgo = moment
    //     .utc(Date.now() - TX_AGE_POLLING_THRESHOLD)
    //     .format('YYYY-MM-DDTHH:mm:ss');
    //   // Since we load all transactions in a first load, later on we only care about fresh ones
    //   Object.assign(params, { created_at: `GTE[${tenMinutesAgo}]` });
    // }
    //
    // const pagesToBeLoaded = Math.ceil(limit / params.per_page);
    //
    // try {
    //   // Load first page of transactions
    //   const response: Transactions = await getTransactionHistory(
    //     this.config,
    //     params
    //   );
    //   const { meta, data: txHistory } = response;
    //   const { totalPages, totalEntries: totalTransactions } = meta.pagination;
    //
    //   let transactions = txHistory.map(tx =>
    //     _createTransactionFromServerData(tx)
    //   );
    //
    //   // Load additional pages of transactions
    //   const hasMultiplePages =
    //     totalPages > 1 && (shouldLoadAll || limit > perPage);
    //   if (hasMultiplePages) {
    //     let page = 2;
    //     const hasNextPage = () => {
    //       const hasMorePages = page < totalPages + 1;
    //       if ((isRestoreActive || isRestoreCompleted) && hasMorePages) {
    //         const loadedTransactions = unionBy(
    //           transactions,
    //           cachedTransactions,
    //           'id'
    //         );
    //         const hasMoreTransactions =
    //           totalTransactions - loadedTransactions.length > 0;
    //         return hasMoreTransactions;
    //       }
    //       return hasMorePages;
    //     };
    //     const shouldLoadNextPage = () =>
    //       shouldLoadAll || page <= pagesToBeLoaded;
    //
    //     if (isRestoreActive || isRestoreCompleted) {
    //       const latestLoadedTransactionDate = transactions[0].date;
    //       const latestLoadedTransactionDateString = moment
    //         .utc(latestLoadedTransactionDate)
    //         .format('YYYY-MM-DDTHH:mm:ss');
    //       // During restoration we need to fetch only transactions older than the latest loaded one
    //       // as this ensures that both totalPages and totalEntries remain unchanged throughout
    //       // subsequent page loads (as in the meantime new transactions can be discovered)
    //       Object.assign(params, {
    //         created_at: `LTE[${latestLoadedTransactionDateString}]`,
    //       });
    //     }
    //
    //     for (page; hasNextPage() && shouldLoadNextPage(); page++) {
    //       const { data: pageHistory } = await getTransactionHistory(
    //         this.config,
    //         Object.assign(params, { page })
    //       );
    //       transactions.push(
    //         ...pageHistory.map(tx => _createTransactionFromServerData(tx))
    //       );
    //     }
    //   }
    //
    //   // Merge newly loaded and previously loaded transactions
    //   // - unionBy also serves the purpose of removing transaction duplicates
    //   //   which may occur as a side-effect of transaction request pagination
    //   //   as multi-page requests are not executed at the exact same time!
    //   transactions = unionBy(transactions, cachedTransactions, 'id');
    //
    //   // Enforce the limit in case we are not loading all transactions
    //   if (!shouldLoadAll) transactions.splice(limit);
    //
    //   const total = transactions.length;
    //
    //   const responseStats = {
    //     apiRequested: limit || 'all',
    //     apiFiltered: shouldLoadOnlyFresh ? 'fresh' : '',
    //     apiReturned: totalTransactions,
    //     apiPagesTotal: totalPages,
    //     apiPagesRequested: params.page,
    //     daedalusCached: cachedTransactions.length,
    //     daedalusLoaded: total - cachedTransactions.length,
    //     daedalusTotal: total,
    //     requestDurationInMs: moment
    //       .duration(moment().diff(requestTimestamp))
    //       .as('milliseconds'),
    //   };
    //   logger.debug(
    //     `AdaApi::searchHistory success: ${total} transactions loaded`,
    //     { responseStats }
    //   );
    //   return new Promise(resolve => resolve({ transactions, total }));
    // } catch (error) {
    //   logger.error('AdaApi::searchHistory error', { error });
    //   throw new GenericApiError(error);
    // }
  };
  getAssets = async (request) => {
    logging_1.logger.debug('AdaApi::getAssets called', {
      parameters: request,
    });
    const { walletId } = request;
    try {
      const response = await (0, getAssets_1.getAssets)(this.config, {
        walletId,
      });
      logging_1.logger.debug('AdaApi::getAssets success', {
        assets: response,
      });
      const assetsLocalData = await global.daedalus.api.localStorage.getAssetsLocalData();
      logging_1.logger.debug('AdaApi::getAssetsLocalData success', {
        assetsLocalData,
      });
      const assets = response.map((asset) =>
        _createAssetFromServerData(
          asset,
          assetsLocalData[asset.policy_id + asset.asset_name] || {},
          this.storedAssetMetadata
        )
      );
      return new Promise((resolve) =>
        resolve({
          assets,
          total: response.length,
        })
      );
    } catch (error) {
      logging_1.logger.error('AdaApi::getAssets error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getWithdrawals = async (request) => {
    logging_1.logger.debug('AdaApi::getWithdrawals called', {
      parameters: request,
    });
    const { walletId } = request;
    try {
      const response = await (0, getWithdrawalHistory_1.getWithdrawalHistory)(
        this.config,
        walletId
      );
      logging_1.logger.debug('AdaApi::getWithdrawals success', {
        transactions: response,
      });
      let withdrawals = new bignumber_js_1.default(0);
      const outgoingTransactions = response.filter(
        (tx) => tx.direction === 'outgoing' && tx.status === 'in_ledger'
      );
      outgoingTransactions.forEach((tx) => {
        tx.withdrawals.forEach((w) => {
          const withdrawal = new bignumber_js_1.default(
            w.amount.quantity.toString()
          ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
          withdrawals = withdrawals.plus(withdrawal);
        });
      });
      return {
        withdrawals,
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getWithdrawals error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  createWallet = async (request) => {
    logging_1.logger.debug('AdaApi::createWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { name, mnemonic, spendingPassword } = request;
    try {
      const walletInitData = {
        name,
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
        mnemonic_sentence: (0, lodash_1.split)(mnemonic, ' '),
        passphrase: spendingPassword,
      };
      const wallet = await (0, createWallet_1.createWallet)(this.config, {
        walletInitData,
      });
      logging_1.logger.debug('AdaApi::createWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::createWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  createLegacyWallet = async (request) => {
    logging_1.logger.debug('AdaApi::createLegacyWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { name, mnemonic, spendingPassword } = request;
    try {
      const walletInitData = {
        name,
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
        mnemonic_sentence: (0, lodash_1.split)(mnemonic, ' '),
        passphrase: spendingPassword,
      };
      const legacyWallet = await (0, restoreByronWallet_1.restoreByronWallet)(
        this.config,
        {
          walletInitData,
        },
        'random'
      );
      // Generate address for the newly created Byron wallet
      const { id: walletId } = legacyWallet;
      const address = await (0,
      createByronWalletAddress_1.createByronWalletAddress)(this.config, {
        passphrase: spendingPassword,
        walletId,
      });
      logging_1.logger.debug('AdaApi::createByronWalletAddress success', {
        address,
      });
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
        assets: {
          available: [],
          total: [],
        },
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::createLegacyWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::createLegacyWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  deleteWallet = async (request) => {
    logging_1.logger.debug('AdaApi::deleteWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const { walletId, isLegacy } = request;
      let response;
      if (isLegacy) {
        response = await (0, deleteLegacyWallet_1.deleteLegacyWallet)(
          this.config,
          {
            walletId,
          }
        );
      } else {
        response = await (0, deleteWallet_1.deleteWallet)(this.config, {
          walletId,
        });
      }
      logging_1.logger.debug('AdaApi::deleteWallet success', {
        response,
      });
      return true;
    } catch (error) {
      logging_1.logger.error('AdaApi::deleteWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  createTransaction = async (request) => {
    logging_1.logger.debug('AdaApi::createTransaction called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const {
      walletId,
      address,
      amount,
      passphrase,
      isLegacy,
      assets,
      withdrawal = WalletTransaction_1.TransactionWithdrawal,
      hasAssetsRemainingAfterTransaction,
    } = request;
    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: Wallet_1.WalletUnits.LOVELACE,
            },
            assets,
          },
        ],
        passphrase,
      };
      let response;
      if (isLegacy) {
        response = await (0,
        createByronWalletTransaction_1.createByronWalletTransaction)(
          this.config,
          {
            walletId,
            data,
          }
        );
      } else {
        response = await (0, createTransaction_1.createTransaction)(
          this.config,
          {
            walletId,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ withdrawal: TransactionWithdrawalType; pay... Remove this comment to see the full error message
            data: { ...data, withdrawal },
          }
        );
      }
      logging_1.logger.debug('AdaApi::createTransaction success', {
        transaction: response,
      });
      return _createTransactionFromServerData(response);
    } catch (error) {
      logging_1.logger.error('AdaApi::createTransaction error', {
        error,
      });
      const apiError = new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .set('transactionIsTooBig', true, {
          linkLabel: 'tooBigTransactionErrorLinkLabel',
          linkURL: 'tooBigTransactionErrorLinkURL',
        })
        .where('code', 'transaction_is_too_big');
      const { requiresAdaToRemainToSupportNativeTokens, adaToProceed } = (0,
      apiHelpers_1.doesWalletRequireAdaToRemainToSupportTokens)(
        error,
        hasAssetsRemainingAfterTransaction
      );
      if (requiresAdaToRemainToSupportNativeTokens) {
        apiError.set('cannotLeaveWalletEmpty', true, {
          adaAmount: adaToProceed,
        });
      }
      throw apiError.result();
    }
  };
  // For testing purpose ONLY
  createExpiredTransaction = async (request) => {
    if (global.environment.isDev) {
      logging_1.logger.debug('AdaApi::createTransaction called', {
        parameters: (0, logging_2.filterLogData)(request),
      });
      const {
        walletId,
        address,
        amount,
        passphrase,
        isLegacy,
        withdrawal = WalletTransaction_1.TransactionWithdrawal,
        ttl,
      } = request;
      try {
        const data = {
          payments: [
            {
              address,
              amount: {
                quantity: amount,
                unit: Wallet_1.WalletUnits.LOVELACE,
              },
            },
          ],
          passphrase,
          time_to_live: {
            quantity: ttl,
            unit: 'second',
          },
        };
        let response;
        if (isLegacy) {
          response = await (0,
          createByronWalletTransaction_1.createByronWalletTransaction)(
            this.config,
            {
              walletId,
              data,
            }
          );
        } else {
          response = await (0, createTransaction_1.createTransaction)(
            this.config,
            {
              walletId,
              // @ts-ignore ts-migrate(2322) FIXME: Type '{ withdrawal: any; payments: { address: any;... Remove this comment to see the full error message
              data: { ...data, withdrawal },
            }
          );
        }
        logging_1.logger.debug('AdaApi::createTransaction success', {
          transaction: response,
        });
        return _createTransactionFromServerData(response);
      } catch (error) {
        logging_1.logger.error('AdaApi::createTransaction error', {
          error,
        });
        throw new ApiError_1.default(error)
          .set('wrongEncryptionPassphrase')
          .where('code', 'bad_request')
          .inc('message', 'passphrase is too short')
          .set('transactionIsTooBig', true, {
            linkLabel: 'tooBigTransactionErrorLinkLabel',
            linkURL: 'tooBigTransactionErrorLinkURL',
          })
          .where('code', 'transaction_is_too_big')
          .result();
      }
    }
    return null;
  };
  calculateTransactionFee = async (request) => {
    logging_1.logger.debug('AdaApi::calculateTransactionFee called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const {
      walletId,
      address,
      amount,
      assets,
      walletBalance,
      availableBalance,
      rewardsBalance,
      isLegacy,
      withdrawal = WalletTransaction_1.TransactionWithdrawal,
    } = request;
    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: Wallet_1.WalletUnits.LOVELACE,
            },
            assets,
          },
        ],
      };
      let response;
      if (isLegacy) {
        response = await (0,
        getByronWalletTransactionFee_1.getByronWalletTransactionFee)(
          this.config,
          {
            walletId,
            data,
          }
        );
      } else {
        response = await (0, getTransactionFee_1.getTransactionFee)(
          this.config,
          {
            walletId,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ withdrawal: TransactionWithdrawalType; pay... Remove this comment to see the full error message
            data: { ...data, withdrawal },
          }
        );
      }
      const formattedTxAmount = new bignumber_js_1.default(
        amount.toString()
      ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
      const { fee, minimumAda } = _createTransactionFeeFromServerData(response);
      const amountWithFee = formattedTxAmount.plus(fee);
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'Array'.
      const isRewardsRedemptionRequest = Array.isArray(withdrawal);
      if (!isRewardsRedemptionRequest && amountWithFee.gt(walletBalance)) {
        // Amount + fees exceeds walletBalance:
        // = show "Not enough Ada for fees. Try sending a smaller amount."
        throw new ApiError_1.default().result('cannotCoverFee');
      }
      logging_1.logger.debug('AdaApi::calculateTransactionFee success', {
        transactionFee: response,
      });
      return {
        fee,
        minimumAda,
      };
    } catch (error) {
      (0, errors_2.handleNotEnoughMoneyError)(error, {
        walletBalance,
        availableBalance,
        rewardsBalance,
      });
    }
  };
  selectCoins = async (request) => {
    logging_1.logger.debug('AdaApi::selectCoins called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const {
      walletId,
      payments,
      delegation,
      walletBalance,
      availableBalance,
      rewardsBalance,
      metadata,
    } = request;
    try {
      let data;
      if (delegation) {
        data = {
          delegation_action: {
            action: delegation.delegationAction,
            pool: delegation.poolId,
          },
        };
      } else if (payments) {
        data = {
          payments: [
            {
              address: payments.address,
              amount: {
                quantity: payments.amount,
                unit: Wallet_1.WalletUnits.LOVELACE,
              },
              assets: payments.assets,
            },
          ],
          withdrawal: WalletTransaction_1.TransactionWithdrawal,
          metadata: metadata || null,
        };
      } else {
        throw new Error('Missing parameters!');
      }
      const response = await (0, selectCoins_1.selectCoins)(this.config, {
        walletId,
        data,
      });
      // @TODO - handle CHANGE parameter on smarter way and change corresponding downstream logic
      const outputs = (0, lodash_1.concat)(response.outputs, response.change);
      // Calculate fee from inputs and outputs
      const inputsData = [];
      const outputsData = [];
      const certificatesData = [];
      let totalInputs = new bignumber_js_1.default(0);
      let totalOutputs = new bignumber_js_1.default(0);
      (0, lodash_1.map)(response.inputs, (input) => {
        const inputAmount = new bignumber_js_1.default(
          input.amount.quantity.toString()
        );
        // @ts-ignore ts-migrate(2339) FIXME: Property 'assets' does not exist on type 'unknown'... Remove this comment to see the full error message
        const inputAssets = (0, lodash_1.map)(input.assets, (asset) => ({
          policyId: asset.policy_id,
          assetName: asset.asset_name,
          quantity: asset.quantity,
        }));
        totalInputs = totalInputs.plus(inputAmount);
        const inputData = {
          address: input.address,
          amount: input.amount,
          id: input.id,
          index: input.index,
          derivationPath: input.derivation_path,
          assets: inputAssets,
        };
        // @ts-ignore ts-migrate(2339) FIXME: Property 'push' does not exist on type '{}'.
        inputsData.push(inputData);
      });
      (0, lodash_1.map)(outputs, (output) => {
        const outputAmount = new bignumber_js_1.default(
          output.amount.quantity.toString()
        );
        // @ts-ignore ts-migrate(2339) FIXME: Property 'assets' does not exist on type 'unknown'... Remove this comment to see the full error message
        const outputAssets = (0, lodash_1.map)(output.assets, (asset) => ({
          policyId: asset.policy_id,
          assetName: asset.asset_name,
          quantity: asset.quantity,
        }));
        totalOutputs = totalOutputs.plus(outputAmount);
        const outputData = {
          address: output.address,
          amount: output.amount,
          derivationPath: output.derivation_path || null,
          assets: outputAssets,
        };
        // @ts-ignore ts-migrate(2339) FIXME: Property 'push' does not exist on type '{}'.
        outputsData.push(outputData);
      });
      if (response.certificates) {
        (0, lodash_1.map)(response.certificates, (certificate) => {
          const certificateData = {
            certificateType: certificate.certificate_type,
            rewardAccountPath: certificate.reward_account_path,
            pool: certificate.pool || null,
          };
          // @ts-ignore ts-migrate(2339) FIXME: Property 'push' does not exist on type '{}'.
          certificatesData.push(certificateData);
        });
      }
      const withdrawalsData = (0, lodash_1.map)(
        response.withdrawals,
        (withdrawal) => ({
          stakeAddress: withdrawal.stake_address,
          derivationPath: withdrawal.derivation_path,
          amount: withdrawal.amount,
        })
      );
      const depositsArray = (0, lodash_1.map)(
        response.deposits_taken,
        (deposit) => deposit.quantity.toString()
      );
      const deposits = depositsArray.length
        ? bignumber_js_1.default.sum.apply(null, depositsArray)
        : new bignumber_js_1.default(0);
      // @TODO - Use API response
      // https://bump.sh/doc/cardano-wallet-diff/changes/c11ebb1b-39c1-40b6-96b9-610705c62cb8#operation-selectcoins-200-deposits_returned
      const depositsReclaimed =
        delegation &&
        delegation.delegationAction === stakingConfig_1.DELEGATION_ACTIONS.QUIT
          ? new bignumber_js_1.default(
              stakingConfig_1.DELEGATION_DEPOSIT
            ).multipliedBy(numbersConfig_1.LOVELACES_PER_ADA)
          : new bignumber_js_1.default(0);
      const withdrawalsArray = (0, lodash_1.map)(
        response.withdrawals,
        (withdrawal) => withdrawal.amount.quantity.toString()
      );
      const withdrawals = withdrawalsArray.length
        ? bignumber_js_1.default.sum.apply(null, withdrawalsArray)
        : new bignumber_js_1.default(0);
      if (withdrawals) {
        totalOutputs = totalOutputs.minus(withdrawals);
      }
      const fee =
        delegation &&
        delegation.delegationAction === stakingConfig_1.DELEGATION_ACTIONS.QUIT
          ? totalInputs.minus(totalOutputs).plus(depositsReclaimed)
          : totalInputs.minus(totalOutputs).minus(deposits);
      const extendedResponse = {
        inputs: inputsData,
        outputs: outputsData,
        certificates: certificatesData,
        withdrawals: withdrawals.gt(0) ? withdrawalsData : [],
        fee: fee.dividedBy(numbersConfig_1.LOVELACES_PER_ADA),
        deposits: deposits.dividedBy(numbersConfig_1.LOVELACES_PER_ADA),
        depositsReclaimed: depositsReclaimed.dividedBy(
          numbersConfig_1.LOVELACES_PER_ADA
        ),
        metadata: response.metadata || null,
      };
      logging_1.logger.debug('AdaApi::selectCoins success', {
        extendedResponse,
      });
      return extendedResponse;
    } catch (error) {
      logging_1.logger.error('AdaApi::selectCoins error', {
        error,
      });
      (0, errors_2.handleNotEnoughMoneyError)(error, {
        walletBalance,
        availableBalance,
        rewardsBalance,
      });
    }
  };
  createExternalTransaction = async (request) => {
    const { signedTransactionBlob } = request;
    try {
      const response = await (0,
      createExternalTransaction_1.createExternalTransaction)(this.config, {
        signedTransactionBlob,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::createExternalTransaction error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  inspectAddress = async (request) => {
    logging_1.logger.debug('AdaApi::inspectAddress called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { addressId } = request;
    try {
      const response = await (0, inspectAddress_1.inspectAddress)(this.config, {
        addressId,
      });
      logging_1.logger.debug('AdaApi::inspectAddress success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::inspectAddress error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getPublicKey = async (request) => {
    logging_1.logger.debug('AdaApi::getPublicKey called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, role, index } = request;
    try {
      const response = await (0, getPublicKey_1.getPublicKey)(this.config, {
        walletId,
        role,
        index,
      });
      logging_1.logger.debug('AdaApi::getPublicKey success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getPublicKey error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getICOPublicKey = async (request) => {
    logging_1.logger.debug('AdaApi::getICOPublicKey called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const response = await (0, getICOPublicKey_1.getICOPublicKey)(
        this.config,
        request
      );
      logging_1.logger.debug('AdaApi::getICOPublicKey success', {
        icoPublicKey: response,
      });
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Transaction' is not assignable to type 'stri... Remove this comment to see the full error message
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getICOPublicKey error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  constructAddress = async (request) => {
    const { data } = request;
    try {
      const response = await (0, constructAddress_1.constructAddress)(
        this.config,
        {
          data,
        }
      );
      logging_1.logger.debug('AdaApi::constructAddress success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::constructAddress error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  createAddress = async (request) => {
    logging_1.logger.debug('AdaApi::createAddress called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { addressIndex, walletId, passphrase: passwordString } = request;
    const passphrase = passwordString || '';
    try {
      const address = await (0,
      createByronWalletAddress_1.createByronWalletAddress)(this.config, {
        passphrase,
        walletId,
        addressIndex,
      });
      logging_1.logger.debug('AdaApi::createAddress success', {
        address,
      });
      return _createAddressFromServerData(address);
    } catch (error) {
      logging_1.logger.error('AdaApi::createAddress error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  deleteTransaction = async (request) => {
    logging_1.logger.debug('AdaApi::deleteTransaction called', {
      parameters: request,
    });
    const { walletId, transactionId, isLegacy } = request;
    try {
      let response;
      if (isLegacy) {
        response = await (0, deleteLegacyTransaction_1.deleteLegacyTransaction)(
          this.config,
          {
            walletId,
            transactionId,
          }
        );
      } else {
        response = await (0, deleteTransaction_1.deleteTransaction)(
          this.config,
          {
            walletId,
            transactionId,
          }
        );
      }
      logging_1.logger.debug('AdaApi::deleteTransaction success', response);
    } catch (error) {
      logging_1.logger.error('AdaApi::deleteTransaction error', {
        error,
      }); // In this particular call we don't need to handle the error in the UI
      // The only reason transaction canceling would fail is if the transaction
      // is no longer pending - in which case there is nothing we can do.
    }
  };
  isValidCertificateMnemonic = (mnemonic) =>
    mnemonic.split(' ').length ===
    cryptoConfig_1.ADA_CERTIFICATE_MNEMONIC_LENGTH;
  getWalletRecoveryPhrase() {
    logging_1.logger.debug('AdaApi::getWalletRecoveryPhrase called');
    try {
      const response = new Promise((resolve) =>
        resolve(
          (0, mnemonics_1.generateAccountMnemonics)(
            cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT
          )
        )
      );
      logging_1.logger.debug('AdaApi::getWalletRecoveryPhrase success');
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getWalletRecoveryPhrase error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  }
  getWalletCertificateAdditionalMnemonics() {
    logging_1.logger.debug(
      'AdaApi::getWalletCertificateAdditionalMnemonics called'
    );
    try {
      const response = new Promise((resolve) =>
        resolve((0, mnemonics_1.generateAdditionalMnemonics)())
      );
      logging_1.logger.debug(
        'AdaApi::getWalletCertificateAdditionalMnemonics success'
      );
      return response;
    } catch (error) {
      logging_1.logger.error(
        'AdaApi::getWalletCertificateAdditionalMnemonics error',
        {
          error,
        }
      );
      throw new ApiError_1.default(error);
    }
  }
  getWalletCertificateRecoveryPhrase(request) {
    logging_1.logger.debug('AdaApi::getWalletCertificateRecoveryPhrase called');
    const { passphrase, input: scrambledInput } = request;
    try {
      const response = new Promise((resolve) =>
        resolve(
          (0, mnemonics_1.scrambleMnemonics)({
            passphrase,
            scrambledInput,
          })
        )
      );
      logging_1.logger.debug(
        'AdaApi::getWalletCertificateRecoveryPhrase success'
      );
      return response;
    } catch (error) {
      logging_1.logger.error(
        'AdaApi::getWalletCertificateRecoveryPhrase error',
        {
          error,
        }
      );
      throw new ApiError_1.default(error);
    }
  }
  getWalletRecoveryPhraseFromCertificate(request) {
    logging_1.logger.debug(
      'AdaApi::getWalletRecoveryPhraseFromCertificate called'
    );
    const { passphrase, scrambledInput } = request;
    try {
      const response = (0, mnemonics_1.unscrambleMnemonics)({
        passphrase,
        scrambledInput,
      });
      logging_1.logger.debug(
        'AdaApi::getWalletRecoveryPhraseFromCertificate success'
      );
      return Promise.resolve(response);
    } catch (error) {
      logging_1.logger.error(
        'AdaApi::getWalletRecoveryPhraseFromCertificate error',
        {
          error,
        }
      );
      const errorRejection = new ApiError_1.default(error)
        .set('invalidMnemonic', true)
        .result();
      return Promise.reject(errorRejection);
    }
  }
  restoreWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    try {
      const wallet = await (0, restoreWallet_1.restoreWallet)(this.config, {
        walletInitData,
      });
      logging_1.logger.debug('AdaApi::restoreWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  createHardwareWallet = async (request) => {
    logging_1.logger.debug('AdaApi::createHardwareWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletName, accountPublicKey } = request;
    const walletInitData = {
      name: walletName,
      account_public_key: accountPublicKey,
    };
    try {
      const hardwareWallet = await (0,
      createHardwareWallet_1.createHardwareWallet)(this.config, {
        walletInitData,
      });
      const wallet = { ...hardwareWallet, isHardwareWallet: true };
      logging_1.logger.debug('AdaApi::createHardwareWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::createHardwareWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getCurrencyList = async () => {
    try {
      const apiResponse = await (0, getCurrencyList_1.getCurrencyList)();
      const response = currencyConfig_1.currencyConfig.responses.list(
        apiResponse
      );
      logging_1.logger.debug('AdaApi::getCurrencyList success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getCurrencyList error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getCurrencyRate = async (currency) => {
    try {
      const apiResponse = await (0, getCurrencyRate_1.getCurrencyRate)(
        currency
      );
      const response = currencyConfig_1.currencyConfig.responses.rate(
        apiResponse
      );
      logging_1.logger.debug('AdaApi::getCurrencyRate success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getCurrencyRate error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  restoreLegacyWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreLegacyWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      style: 'random',
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    try {
      const legacyWallet = await (0, restoreLegacyWallet_1.restoreLegacyWallet)(
        this.config,
        {
          walletInitData,
        }
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreLegacyWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; dele... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreLegacyWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  restoreByronRandomWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreByronRandomWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = walletRestoreConfig_1.WALLET_BYRON_KINDS.RANDOM;
    try {
      const legacyWallet = await (0, restoreByronWallet_1.restoreByronWallet)(
        this.config,
        {
          walletInitData,
        },
        type
      );
      // Generate address for the newly restored Byron wallet
      const { id: walletId } = legacyWallet;
      const address = await (0,
      createByronWalletAddress_1.createByronWalletAddress)(this.config, {
        passphrase: spendingPassword,
        walletId,
      });
      logging_1.logger.debug('AdaApi::createAddress (Byron) success', {
        address,
      });
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreByronRandomWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; dele... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreByronRandomWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  restoreByronIcarusWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreByronIcarusWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = walletRestoreConfig_1.WALLET_BYRON_KINDS.ICARUS;
    try {
      const legacyWallet = await (0, restoreByronWallet_1.restoreByronWallet)(
        this.config,
        {
          walletInitData,
        },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreByronIcarusWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; dele... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreByronIcarusWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  restoreByronTrezorWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreByronTrezorWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = walletRestoreConfig_1.WALLET_BYRON_KINDS.TREZOR;
    try {
      const legacyWallet = await (0, restoreByronWallet_1.restoreByronWallet)(
        this.config,
        {
          walletInitData,
        },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreByronTrezorWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; dele... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreByronTrezorWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  restoreByronLedgerWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreByronLedgerWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { recoveryPhrase, walletName, spendingPassword } = request;
    const walletInitData = {
      name: walletName,
      mnemonic_sentence: recoveryPhrase,
      passphrase: spendingPassword,
    };
    const type = walletRestoreConfig_1.WALLET_BYRON_KINDS.LEDGER;
    try {
      const legacyWallet = await (0, restoreByronWallet_1.restoreByronWallet)(
        this.config,
        {
          walletInitData,
        },
        type
      );
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreByronLedgerWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; delegation... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreByronLedgerWallet error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('forbiddenMnemonic')
        .where('message', 'JSONValidationFailed')
        .inc(
          'diagnostic.validationError',
          'Forbidden Mnemonic: an example Mnemonic has been submitted'
        )
        .set('forbiddenMnemonic')
        .where('code', 'invalid_restoration_parameters')
        .result();
    }
  };
  restoreExportedByronWallet = async (request) => {
    logging_1.logger.debug('AdaApi::restoreExportedByronWallet called', {
      name: request.name,
    });
    try {
      const legacyWallet = await (0,
      restoreExportedByronWallet_1.restoreExportedByronWallet)(this.config, {
        walletInitData: request,
      });
      const extraLegacyWalletProps = {
        address_pool_gap: 0,
        // Not needed for legacy wallets
        delegation: {
          active: {
            status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
          },
        },
        isLegacy: true,
      };
      const wallet = { ...legacyWallet, ...extraLegacyWalletProps };
      logging_1.logger.debug('AdaApi::restoreExportedByronWallet success', {
        wallet,
      });
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ address_pool_gap: number; dele... Remove this comment to see the full error message
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::restoreExportedByronWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  importWalletFromKey = async (request) => {
    logging_1.logger.debug('AdaApi::importWalletFromKey called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { filePath, spendingPassword } = request;
    try {
      const importedWallet = await (0, importWalletAsKey_1.importWalletAsKey)(
        this.config,
        {
          filePath,
          spendingPassword: spendingPassword || '',
        }
      );
      logging_1.logger.debug('AdaApi::importWalletFromKey success', {
        importedWallet,
      });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::importWalletFromKey error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('walletAlreadyImported', true)
        .where('code', 'wallet_already_exists')
        .result('walletFileImportError');
    }
  };
  importWalletFromFile = async (request) => {
    logging_1.logger.debug('AdaApi::importWalletFromFile called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { filePath, spendingPassword } = request;
    const isKeyFile = filePath.split('.').pop().toLowerCase() === 'key';
    try {
      const importedWallet = isKeyFile
        ? await (0, importWalletAsKey_1.importWalletAsKey)(this.config, {
            filePath,
            spendingPassword,
          })
        : await (0, importWalletAsJSON_1.importWalletAsJSON)(
            this.config,
            filePath
          );
      logging_1.logger.debug('AdaApi::importWalletFromFile success', {
        importedWallet,
      });
      return _createWalletFromServerData(importedWallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::importWalletFromFile error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('walletAlreadyImported', true)
        .where('code', 'wallet_already_exists')
        .result('walletFileImportError');
    }
  };
  updateWallet = async (request) => {
    logging_1.logger.debug('AdaApi::updateWallet called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, name, isLegacy } = request;
    try {
      let wallet;
      if (isLegacy) {
        const response = await (0, updateByronWallet_1.updateByronWallet)(
          this.config,
          {
            walletId,
            name,
          }
        );
        wallet = {
          ...response,
          address_pool_gap: 0,
          // Not needed for legacy wallets
          delegation: {
            active: {
              status: Wallet_1.WalletDelegationStatuses.NOT_DELEGATING,
            },
          },
          isLegacy: true,
        };
      } else {
        wallet = await (0, updateWallet_1.updateWallet)(this.config, {
          walletId,
          name,
        });
      }
      logging_1.logger.debug('AdaApi::updateWallet success', {
        wallet,
      });
      return _createWalletFromServerData(wallet);
    } catch (error) {
      logging_1.logger.error('AdaApi::updateWallet error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  updateSpendingPassword = async (request) => {
    logging_1.logger.debug('AdaApi::updateSpendingPassword called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, oldPassword, newPassword, isLegacy } = request;
    try {
      if (isLegacy) {
        await (0, updateByronSpendingPassword_1.updateByronSpendingPassword)(
          this.config,
          {
            walletId,
            oldPassword,
            newPassword,
          }
        );
        if (!oldPassword) {
          // Generate address for the Byron wallet for which password was set for the 1st time
          const address = await (0,
          createByronWalletAddress_1.createByronWalletAddress)(this.config, {
            passphrase: newPassword,
            walletId,
          });
          logging_1.logger.debug('AdaApi::createAddress (Byron) success', {
            address,
          });
        }
      } else {
        await (0, updateSpendingPassword_1.updateSpendingPassword)(
          this.config,
          {
            walletId,
            oldPassword,
            newPassword,
          }
        );
      }
      logging_1.logger.debug('AdaApi::updateSpendingPassword success');
      return true;
    } catch (error) {
      logging_1.logger.error('AdaApi::updateSpendingPassword error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  quitStakePool = async (request) => {
    logging_1.logger.debug('AdaApi::quitStakePool called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, passphrase } = request;
    try {
      const result = await (0, quitStakePool_1.quitStakePool)(this.config, {
        walletId,
        passphrase,
      });
      logging_1.logger.debug('AdaApi::quitStakePool success', {
        result,
      });
      return result;
    } catch (error) {
      logging_1.logger.error('AdaApi::quitStakePool error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  getSmashSettings = async () => {
    logging_1.logger.debug('AdaApi::getSmashSettings called');
    try {
      const { pool_metadata_source: poolMetadataSource } = await (0,
      getSmashSettings_1.getSmashSettings)(this.config);
      logging_1.logger.debug('AdaApi::getSmashSettings success', {
        poolMetadataSource,
      });
      return poolMetadataSource;
    } catch (error) {
      logging_1.logger.error('AdaApi::getSmashSettings error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  checkSmashServerIsValid = async (url) => {
    logging_1.logger.debug('AdaApi::checkSmashServerIsValid called', {
      parameters: {
        url,
      },
    });
    try {
      if (url === stakingConfig_1.SMASH_SERVERS_LIST.direct.url) {
        return true;
      }
      const { health } = await (0,
      checkSmashServerHealth_1.checkSmashServerHealth)(this.config, url);
      const isValid =
        health === stakingConfig_1.SMASH_SERVER_STATUSES.AVAILABLE;
      logging_1.logger.debug('AdaApi::checkSmashServerIsValid success', {
        isValid,
      });
      return isValid;
    } catch (error) {
      logging_1.logger.error('AdaApi::checkSmashServerIsValid error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  updateSmashSettings = async (poolMetadataSource) => {
    logging_1.logger.debug('AdaApi::updateSmashSettings called', {
      parameters: {
        poolMetadataSource,
      },
    });
    try {
      const isSmashServerValid = await this.checkSmashServerIsValid(
        poolMetadataSource
      );
      if (!isSmashServerValid) {
        const error = {
          code: 'invalid_smash_server',
        };
        throw new ApiError_1.default(error);
      }
      await (0, updateSmashSettings_1.updateSmashSettings)(
        this.config,
        poolMetadataSource
      );
      logging_1.logger.debug('AdaApi::updateSmashSettings success', {
        poolMetadataSource,
      });
    } catch (error) {
      const id = (0, lodash_1.get)(error, 'id');
      const message = (0, lodash_1.get)(error, 'values.message');
      if (
        id === 'api.errors.GenericApiError' &&
        message ===
          'Error parsing query parameter url failed: URI must not contain a path/query/fragment.'
      ) {
        throw new ApiError_1.default({
          code: 'invalid_smash_server',
        });
      }
      logging_1.logger.error('AdaApi::updateSmashSettings error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getRedeemItnRewardsFee = async (request) => {
    const { address, wallet, recoveryPhrase: withdrawal } = request;
    const {
      id: walletId,
      amount: walletBalance,
      availableAmount,
      reward: rewardsBalance,
    } = wallet;
    const minRewardsReceiverBalance = new bignumber_js_1.default(
      stakingConfig_1.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    // Amount is set to either wallet's balance in case balance is less than 3 ADA or 1 ADA in order to avoid min UTXO affecting transaction fees calculation
    const amount = walletBalance.isLessThan(
      minRewardsReceiverBalance.times(
        stakingConfig_1.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE * 3
      )
    )
      ? (0, formatters_1.formattedAmountToLovelace)(walletBalance.toString())
      : stakingConfig_1.REWARDS_REDEMPTION_FEE_CALCULATION_AMOUNT;
    const payload = {
      address,
      walletId,
      walletBalance,
      availableBalance: availableAmount.plus(rewardsBalance),
      rewardsBalance,
      amount,
      withdrawal,
      isLegacy: false,
    };
    try {
      const { fee } = await this.calculateTransactionFee(payload);
      logging_1.logger.debug('AdaApi::getRedeemItnRewardsFee success', {
        fee,
      });
      return fee;
    } catch (error) {
      logging_1.logger.error('AdaApi::getRedeemItnRewardsFee error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  requestRedeemItnRewards = async (request) => {
    const {
      address,
      walletId,
      spendingPassword: passphrase,
      recoveryPhrase: withdrawal,
    } = request;
    const amount = stakingConfig_1.REWARDS_REDEMPTION_FEE_CALCULATION_AMOUNT;
    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: Wallet_1.WalletUnits.LOVELACE,
            },
          },
        ],
        passphrase,
        withdrawal,
      };
      const transaction = await (0, createTransaction_1.createTransaction)(
        this.config,
        {
          walletId,
          data,
        }
      );
      const response = _createRedeemItnRewardsFromServerData(transaction);
      logging_1.logger.debug('AdaApi::requestRedeemItnRewards success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::requestRedeemItnRewards error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  exportWalletToFile = async (request) => {
    const { walletId, filePath } = request;
    logging_1.logger.debug('AdaApi::exportWalletToFile called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const response = await (0, exportWalletAsJSON_1.exportWalletAsJSON)(
        this.config,
        {
          walletId,
          filePath,
        }
      );
      logging_1.logger.debug('AdaApi::exportWalletToFile success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::exportWalletToFile error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getWalletUtxos = async (request) => {
    const { walletId, isLegacy } = request;
    logging_1.logger.debug('AdaApi::getWalletUtxos called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      let response;
      if (isLegacy) {
        response = await (0, getByronWalletUtxos_1.getByronWalletUtxos)(
          this.config,
          {
            walletId,
          }
        );
      } else {
        response = await (0, getWalletUtxos_1.getWalletUtxos)(this.config, {
          walletId,
        });
      }
      logging_1.logger.debug('AdaApi::getWalletUtxos success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::getWalletUtxos error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  transferFundsCalculateFee = async (request) => {
    const { sourceWalletId } = request;
    logging_1.logger.debug('AdaApi::transferFundsCalculateFee called', {
      parameters: {
        sourceWalletId,
      },
    });
    try {
      const response = await (0,
      transferFundsCalculateFee_1.transferFundsCalculateFee)(this.config, {
        sourceWalletId,
      });
      logging_1.logger.debug('AdaApi::transferFundsCalculateFee success', {
        response,
      });
      return _createMigrationFeeFromServerData(response);
    } catch (error) {
      logging_1.logger.error('AdaApi::transferFundsCalculateFee error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  transferFunds = async (request) => {
    const { sourceWalletId, targetWalletAddresses, passphrase } = request;
    logging_1.logger.debug('AdaApi::transferFunds called', {
      parameters: {
        sourceWalletId,
        targetWalletAddresses,
      },
    });
    if (!targetWalletAddresses) {
      throw new ApiError_1.default({
        code: 'no_such_wallet',
        message: 'Target wallet does not exist',
      }).result();
    }
    try {
      const response = await (0, transferFunds_1.transferFunds)(this.config, {
        sourceWalletId,
        targetWalletAddresses,
        passphrase,
      });
      logging_1.logger.debug('AdaApi::transferFunds success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::transferFunds error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  getStakePools = async (stake = 0) => {
    logging_1.logger.debug('AdaApi::getStakePools called', {
      parameters: {
        stake,
      },
    });
    try {
      const response = await (0, getStakePools_1.getStakePools)(
        this.config,
        stake
      );
      const stakePools = response
        .filter(
          (stakePool) =>
            !!stakePool?.metadata &&
            !stakePool?.flags?.includes('delisted') &&
            stakePool?.margin?.quantity < 100
        )
        .map(_createStakePoolFromServerData);
      logging_1.logger.debug('AdaApi::getStakePools success', {
        stakePoolsTotal: response.length,
        stakePoolsWithMetadata: stakePools.length,
        unfilteredStakePools: response,
      });
      return stakePools;
    } catch (error) {
      logging_1.logger.error('AdaApi::getStakePools error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  testReset = async () => {
    logging_1.logger.debug('AdaApi::testReset called');
    try {
      const wallets = await this.getWallets();
      await Promise.all(
        wallets.map((wallet) =>
          this.deleteWallet({
            walletId: wallet.id,
            isLegacy: wallet.isLegacy,
            isHardwareWallet: wallet.isHardwareWallet,
          })
        )
      );
      logging_1.logger.debug('AdaApi::testReset success');
    } catch (error) {
      logging_1.logger.error('AdaApi::testReset error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getNetworkInfo = async () => {
    logging_1.logger.debug('AdaApi::getNetworkInfo called');
    try {
      const networkInfo = await (0, getNetworkInfo_1.getNetworkInfo)(
        this.config
      );
      logging_1.logger.debug('AdaApi::getNetworkInfo success', {
        networkInfo,
      });
      const {
        sync_progress: syncProgressRaw,
        node_tip: nodeTip,
        network_tip: networkTip,
        next_epoch: nextEpoch,
      } = networkInfo;
      const syncProgress =
        (0, lodash_1.get)(syncProgressRaw, 'status') === 'ready'
          ? 100
          : (0, lodash_1.get)(syncProgressRaw, 'progress.quantity', 0);
      const nextEpochNumber = (0, lodash_1.get)(
        nextEpoch,
        'epoch_number',
        null
      );
      const nextEpochStartTime = (0, lodash_1.get)(
        nextEpoch,
        'epoch_start_time',
        ''
      );
      // extract relevant data before sending to NetworkStatusStore
      return {
        syncProgress,
        localTip: {
          epoch: (0, lodash_1.get)(nodeTip, 'epoch_number', 0),
          slot: (0, lodash_1.get)(nodeTip, 'slot_number', 0),
          absoluteSlotNumber: (0, lodash_1.get)(
            nodeTip,
            'absolute_slot_number',
            0
          ),
        },
        networkTip: networkTip
          ? {
              epoch: (0, lodash_1.get)(networkTip, 'epoch_number', 0),
              slot: (0, lodash_1.get)(networkTip, 'slot_number', 0),
              absoluteSlotNumber: (0, lodash_1.get)(
                networkTip,
                'absolute_slot_number',
                0
              ),
            }
          : null,
        nextEpoch: nextEpoch
          ? {
              // N+1 epoch
              epochNumber: nextEpochNumber,
              epochStart: nextEpochStartTime,
            }
          : null,
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getNetworkInfo error', {
        error,
      });
      // Special Error case
      if (
        error.code === errors_1.TlsCertificateNotValidError.API_ERROR ||
        error.code === 'EPROTO'
      ) {
        throw new errors_1.TlsCertificateNotValidError();
      }
      throw new ApiError_1.default(error);
    }
  };
  // @ts-ignore ts-migrate(2583) FIXME: Cannot find name 'Promise'. Do you need to change ... Remove this comment to see the full error message
  getNetworkClock = async (isForceCheck) => {
    logging_1.logger.debug('AdaApi::getNetworkClock called', {
      isForceCheck,
    });
    try {
      const networkClock = await (0, getNetworkClock_1.getNetworkClock)(
        this.config,
        isForceCheck
      );
      logging_1.logger.debug('AdaApi::getNetworkClock success', {
        networkClock,
        isForceCheck,
      });
      return {
        status: networkClock.status,
        offset: (0, lodash_1.get)(networkClock, 'offset.quantity', null),
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getNetworkClock error', {
        error,
        isForceCheck,
      });
      throw new ApiError_1.default(error);
    }
  };
  // @ts-ignore ts-migrate(2583) FIXME: Cannot find name 'Promise'. Do you need to change ... Remove this comment to see the full error message
  getNetworkParameters = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.debug('AdaApi::getNetworkParameters called');
    try {
      const networkParameters = await (0,
      getNetworkParameters_1.getNetworkParameters)(this.config);
      logging_1.logger.debug('AdaApi::getNetworkParameters success', {
        networkParameters,
      });
      const {
        genesis_block_hash: genesisBlockHash,
        blockchain_start_time,
        // eslint-disable-line
        slot_length: slotLength,
        epoch_length: epochLength,
        security_parameter: securityParameter,
        active_slot_coefficient: activeSlotCoefficient,
        decentralization_level: decentralizationLevel,
        desired_pool_number: desiredPoolNumber,
        minimum_utxo_value: minimumUtxoValue,
        eras,
      } = networkParameters;
      const blockchainStartTime = (0, moment_1.default)(
        blockchain_start_time
      ).valueOf();
      return {
        genesisBlockHash,
        blockchainStartTime,
        slotLength,
        epochLength,
        securityParameter,
        activeSlotCoefficient,
        decentralizationLevel,
        desiredPoolNumber,
        minimumUtxoValue,
        eras,
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getNetworkParameters error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  getNews = async () => {
    logging_1.logger.debug('AdaApi::getNews called');
    // Fetch news json
    let rawNews;
    let news;
    try {
      rawNews = await (0, getNews_1.getNews)();
      news = JSON.parse(rawNews);
    } catch (error) {
      logging_1.logger.error('AdaApi::getNews error', {
        error,
      });
      throw new Error('Unable to fetch news');
    }
    // Fetch news verification hash
    let newsHash;
    let expectedNewsHash;
    try {
      newsHash = await (0, hashing_1.getSHA256HexForString)(rawNews);
      expectedNewsHash = await (0, getNewsHash_1.getNewsHash)(news.updatedAt);
    } catch (error) {
      logging_1.logger.error('AdaApi::getNews (hash) error', {
        error,
      });
      throw new Error('Unable to fetch news hash');
    }
    if (newsHash !== expectedNewsHash) {
      throw new Error('Newsfeed could not be verified');
    }
    logging_1.logger.debug('AdaApi::getNews success', {
      updatedAt: news.updatedAt,
      items: news.items.length,
    });
    return news;
  };
  calculateDelegationFee = async (request) => {
    logging_1.logger.debug('AdaApi::calculateDelegationFee called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    try {
      const response = await (0, getDelegationFee_1.getDelegationFee)(
        this.config,
        {
          walletId: request.walletId,
        }
      );
      logging_1.logger.debug('AdaApi::calculateDelegationFee success', {
        response,
      });
      return _createDelegationFeeFromServerData(response);
    } catch (error) {
      logging_1.logger.error('AdaApi::calculateDelegationFee error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  joinStakePool = async (request) => {
    logging_1.logger.debug('AdaApi::joinStakePool called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const { walletId, stakePoolId, passphrase } = request;
    try {
      const response = await (0, joinStakePool_1.joinStakePool)(this.config, {
        walletId,
        stakePoolId,
        passphrase,
      });
      logging_1.logger.debug('AdaApi::joinStakePool success', {
        stakePool: response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::joinStakePool error', {
        error,
      });
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .result();
    }
  };
  createWalletSignature = async (request) => {
    logging_1.logger.debug('AdaApi::createWalletSignature called', {
      parameters: (0, logging_2.filterLogData)(request),
    });
    const {
      walletId,
      role,
      index,
      passphrase,
      votingKey,
      stakeKey,
      addressHex,
      absoluteSlotNumber,
    } = request;
    try {
      const data = {
        passphrase,
        metadata: {
          [61284]: {
            map: [
              {
                k: {
                  int: 1,
                },
                v: {
                  bytes: votingKey,
                },
              },
              {
                k: {
                  int: 2,
                },
                v: {
                  bytes: stakeKey,
                },
              },
              {
                k: {
                  int: 3,
                },
                v: {
                  bytes: addressHex,
                },
              },
              {
                k: {
                  int: 4,
                },
                v: {
                  int: absoluteSlotNumber,
                },
              },
            ],
          },
        },
      };
      const response = await (0, createWalletSignature_1.createWalletSignature)(
        this.config,
        {
          walletId,
          role,
          index,
          data,
        }
      );
      logging_1.logger.debug('AdaApi::createWalletSignature success', {
        response,
      });
      return response;
    } catch (error) {
      logging_1.logger.error('AdaApi::createWalletSignature error', {
        error,
      });
      throw new ApiError_1.default(error);
    }
  };
  createVotingRegistrationTransaction = async (request) => {
    logging_1.logger.debug(
      'AdaApi::createVotingRegistrationTransaction called',
      {
        parameters: (0, logging_2.filterLogData)(request),
      }
    );
    const {
      walletId,
      address,
      addressHex,
      amount,
      passphrase,
      votingKey,
      stakeKey,
      signature,
      absoluteSlotNumber,
    } = request;
    try {
      const data = {
        payments: [
          {
            address,
            amount: {
              quantity: amount,
              unit: Wallet_1.WalletUnits.LOVELACE,
            },
          },
        ],
        passphrase,
        metadata: {
          [61284]: {
            map: [
              {
                k: {
                  int: 1,
                },
                v: {
                  bytes: votingKey,
                },
              },
              {
                k: {
                  int: 2,
                },
                v: {
                  bytes: stakeKey,
                },
              },
              {
                k: {
                  int: 3,
                },
                v: {
                  bytes: addressHex,
                },
              },
              {
                k: {
                  int: 4,
                },
                v: {
                  int: absoluteSlotNumber,
                },
              },
            ],
          },
          [61285]: {
            map: [
              {
                k: {
                  int: 1,
                },
                v: {
                  bytes: signature,
                },
              },
            ],
          },
        },
      };
      const response = await (0, createTransaction_1.createTransaction)(
        this.config,
        {
          walletId,
          data: { ...data },
        }
      );
      logging_1.logger.debug(
        'AdaApi::createVotingRegistrationTransaction success',
        {
          transaction: response,
        }
      );
      return _createTransactionFromServerData(response);
    } catch (error) {
      logging_1.logger.error(
        'AdaApi::createVotingRegistrationTransaction error',
        {
          error,
        }
      );
      throw new ApiError_1.default(error)
        .set('wrongEncryptionPassphrase')
        .where('code', 'bad_request')
        .inc('message', 'passphrase is too short')
        .set('transactionIsTooBig', true, {
          linkLabel: 'tooBigTransactionErrorLinkLabel',
          linkURL: 'tooBigTransactionErrorLinkURL',
        })
        .where('code', 'transaction_is_too_big')
        .result();
    }
  };
  setCardanoNodeFault = async (fault) => {
    await cardano_ipc_1.cardanoFaultInjectionChannel.send(fault);
  };
  // No implementation here but can be overwritten
  setLocalTimeDifference;
  setSyncProgress;
  setFaultyNodeSettingsApi;
  resetTestOverrides;
  // Newsfeed testing utility
  setTestingNewsFeed;
  setTestingStakePools;
  setTestingWallets;
  setTestingWallet;
  // Stake pools testing utility
  setFakeStakePoolsJsonForTesting;
  setStakePoolsFetchingFailed;
  getCatalystFund = async () => {
    logging_1.logger.debug('AdaApi::getCatalystFund called', {});
    try {
      const catalystFund = await (0, getCatalystFund_1.getCatalystFund)();
      logging_1.logger.debug('AdaApi::getCatalystFund success', {
        catalystFund,
      });
      const fundNumber =
        Number(catalystFund.fund_name?.match(/\d+/)?.[0]) ||
        catalystFund.id + 1;
      return {
        current: {
          number: fundNumber,
          startTime: new Date(catalystFund.fund_start_time),
          endTime: new Date(catalystFund.fund_end_time),
          resultsTime: new Date(
            catalystFund.chain_vote_plans?.[0]?.chain_committee_end_time
          ),
          registrationSnapshotTime: new Date(
            catalystFund.registration_snapshot_time
          ),
        },
        next: {
          number: fundNumber + 1,
          startTime: new Date(catalystFund.next_fund_start_time),
          registrationSnapshotTime: new Date(
            catalystFund.next_registration_snapshot_time
          ),
        },
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getCatalystFund error', {
        error,
      });
    }
  };
} // ========== TRANSFORM SERVER DATA INTO FRONTEND MODELS =========
exports.default = AdaApi;
const _createWalletFromServerData = (0, mobx_1.action)(
  'AdaApi::_createWalletFromServerData',
  (wallet) => {
    const {
      id: rawWalletId,
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
    const id = isLegacy
      ? (0, utils_1.getLegacyWalletId)(rawWalletId)
      : rawWalletId;
    const passphraseLastUpdatedAt = (0, lodash_1.get)(
      passphrase,
      'last_updated_at',
      null
    );
    const walletTotalAmount =
      balance.total.unit === Wallet_1.WalletUnits.LOVELACE
        ? new bignumber_js_1.default(
            balance.total.quantity.toString()
          ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA)
        : new bignumber_js_1.default(balance.total.quantity.toString());
    const walletAvailableAmount =
      balance.available.unit === Wallet_1.WalletUnits.LOVELACE
        ? new bignumber_js_1.default(
            balance.available.quantity.toString()
          ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA)
        : new bignumber_js_1.default(balance.available.quantity.toString());
    let walletRewardAmount = new bignumber_js_1.default(0);
    if (!isLegacy) {
      walletRewardAmount =
        balance.reward.unit === Wallet_1.WalletUnits.LOVELACE
          ? new bignumber_js_1.default(
              balance.reward.quantity.toString()
            ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA)
          : new bignumber_js_1.default(balance.reward.quantity.toString());
    }
    // Current (Active)
    const active = (0, lodash_1.get)(delegation, 'active', null);
    const target = (0, lodash_1.get)(active, 'target', null);
    const status = (0, lodash_1.get)(active, 'status', null);
    const delegatedStakePoolId = isLegacy ? null : target;
    const delegationStakePoolStatus = isLegacy ? null : status;
    // Last
    const next = (0, lodash_1.get)(delegation, 'next', null);
    const lastPendingStakePool = next ? (0, lodash_1.last)(next) : null;
    const lastTarget = (0, lodash_1.get)(lastPendingStakePool, 'target', null);
    const lastStatus = (0, lodash_1.get)(lastPendingStakePool, 'status', null);
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
          assetNameASCII: (0, strings_1.hexToString)(assetName),
          quantity: new bignumber_js_1.default(quantity.toString()),
        };
      }),
      total: assets.total.map((item) => {
        const { policy_id: policyId, asset_name: assetName, quantity } = item;
        const uniqueId = `${policyId}${assetName}`;
        return {
          uniqueId,
          policyId,
          assetName,
          assetNameASCII: (0, strings_1.hexToString)(assetName),
          quantity: new bignumber_js_1.default(quantity.toString()),
        };
      }),
    };
    return new Wallet_1.default({
      id,
      addressPoolGap,
      name,
      amount: walletTotalAmount,
      availableAmount: walletAvailableAmount,
      reward: walletRewardAmount,
      assets: walletAssets,
      passwordUpdateDate:
        passphraseLastUpdatedAt && new Date(passphraseLastUpdatedAt),
      hasPassword: isHardwareWallet || passphraseLastUpdatedAt !== null,
      // For HW set that wallet has password
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
const _createAddressFromServerData = (0, mobx_1.action)(
  'AdaApi::_createAddressFromServerData',
  (address) => {
    const { id, state, derivation_path: derivationPath } = address;
    return new WalletAddress_1.default({
      id,
      used: state === 'used',
      spendingPath: (0, hardwareWalletUtils_1.derivationPathToAddressPath)(
        derivationPath
      ), // E.g. "1852'/1815'/0'/0/19",
    });
  }
);
const _conditionToTxState = (condition) => {
  switch (condition) {
    case 'pending':
      return WalletTransaction_1.TransactionStates.PENDING;
    case 'expired':
      return WalletTransaction_1.TransactionStates.FAILED;
    default:
      return WalletTransaction_1.TransactionStates.OK;
  }
};
const _createTransactionFromServerData = (0, mobx_1.action)(
  'AdaApi::_createTransactionFromServerData',
  (data) => {
    const {
      id,
      amount,
      fee,
      deposit_taken: deposit,
      inserted_at: insertedAt,
      pending_since: pendingSince,
      depth,
      direction,
      inputs,
      outputs,
      withdrawals,
      status,
      metadata,
    } = data;
    const state = _conditionToTxState(status);
    const stateInfo =
      state === WalletTransaction_1.TransactionStates.PENDING
        ? pendingSince
        : insertedAt;
    const date = (0, lodash_1.get)(stateInfo, 'time');
    const slotNumber = (0, lodash_1.get)(
      stateInfo,
      ['block', 'slot_number'],
      null
    );
    const epochNumber = (0, lodash_1.get)(
      stateInfo,
      ['block', 'epoch_number'],
      null
    );
    const confirmations = (0, lodash_1.get)(depth, 'quantity', 0);
    // Mapping asset items from server data
    const outputAssets = (0, lodash_1.flatten)(
      outputs.map(({ assets, address }) =>
        assets ? assets.map((asset) => ({ ...asset, address })) : []
      )
    );
    const transactionAssets = (0, lodash_1.map)(
      outputAssets,
      ({ policy_id: policyId, asset_name: assetName, quantity, address }) => ({
        policyId,
        assetName,
        quantity: new bignumber_js_1.default(quantity.toString()),
        address,
      })
    );
    return new WalletTransaction_1.WalletTransaction({
      id,
      confirmations,
      slotNumber,
      epochNumber,
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type:
        direction === 'outgoing'
          ? WalletTransaction_1.TransactionTypes.EXPEND
          : WalletTransaction_1.TransactionTypes.INCOME,
      amount: new bignumber_js_1.default(
        direction === 'outgoing'
          ? `-${amount.quantity.toString()}`
          : amount.quantity.toString()
      ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA),
      fee: new bignumber_js_1.default(fee.quantity.toString()).dividedBy(
        numbersConfig_1.LOVELACES_PER_ADA
      ),
      deposit: new bignumber_js_1.default(
        deposit.quantity.toString()
      ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA),
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ policyId: string; assetName: string; quant... Remove this comment to see the full error message
      assets: transactionAssets,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Date' is not assignable to param... Remove this comment to see the full error message
      date: (0, utils_1.utcStringToDate)(date),
      description: '',
      addresses: {
        from: inputs.map(({ address }) => address || null),
        to: outputs.map(({ address }) => address),
        withdrawals: withdrawals.map(({ stake_address: address }) => address),
      },
      state,
      metadata,
    });
  }
);
const _createAssetFromServerData = (0, mobx_1.action)(
  'AdaApi::_createAssetFromServerData',
  (data, localData, storedAssetMetadata) => {
    const {
      policy_id: policyId,
      asset_name: assetName,
      fingerprint,
      metadata,
    } = data;
    const uniqueId = `${policyId}${assetName}`;
    const storedMetadata = storedAssetMetadata[uniqueId];
    const { decimals } = localData;
    const { decimals: recommendedDecimals = null } =
      metadata || storedMetadata || {};
    if (metadata) {
      storedAssetMetadata[uniqueId] = metadata;
    }
    return new Asset_1.default({
      policyId,
      assetName,
      fingerprint,
      metadata: metadata || storedMetadata,
      decimals,
      recommendedDecimals,
      uniqueId,
    });
  }
);
const _createTransactionFeeFromServerData = (0, mobx_1.action)(
  'AdaApi::_createTransactionFeeFromServerData',
  (data) => {
    const feeAmount = (0, lodash_1.get)(data, ['estimated_max', 'quantity'], 0);
    const minimumAdaAmount = (0, lodash_1.get)(
      data,
      'minimum_coins.[0].quantity',
      0
    );
    const fee = new bignumber_js_1.default(feeAmount.toString()).dividedBy(
      numbersConfig_1.LOVELACES_PER_ADA
    );
    const minimumAda = new bignumber_js_1.default(
      minimumAdaAmount.toString()
    ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
    return {
      fee,
      minimumAda,
    };
  }
);
const _createMigrationFeeFromServerData = (0, mobx_1.action)(
  'AdaApi::_createMigrationFeeFromServerData',
  (data) => {
    const { quantity: feeAmount = 0 } = data.migration_cost;
    const fee = new bignumber_js_1.default(feeAmount.toString()).dividedBy(
      numbersConfig_1.LOVELACES_PER_ADA
    );
    const { quantity: leftoversAmount = 0 } = data.leftovers;
    const leftovers = new bignumber_js_1.default(
      leftoversAmount.toString()
    ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
    return {
      fee,
      leftovers,
    };
  }
);
const _createDelegationFeeFromServerData = (0, mobx_1.action)(
  'AdaApi::_createDelegationFeeFromServerData',
  (data) => {
    const fee = new bignumber_js_1.default(
      (0, lodash_1.get)(data, ['estimated_max', 'quantity'], 0).toString()
    ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
    const deposits = new bignumber_js_1.default(
      (0, lodash_1.get)(data, ['deposit', 'quantity'], 0).toString()
    ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA);
    // @TODO Use api response data when api is ready
    const depositsReclaimed = new bignumber_js_1.default(0);
    return {
      fee,
      deposits,
      depositsReclaimed,
    };
  }
);
const _createStakePoolFromServerData = (0, mobx_1.action)(
  'AdaApi::_createStakePoolFromServerData',
  (stakePool, index) => {
    const {
      id,
      metrics,
      cost,
      margin: profitMargin,
      metadata,
      pledge,
      retirement,
    } = stakePool;
    const {
      relative_stake: relativeStake,
      produced_blocks: producedBlocks,
      non_myopic_member_rewards: nonMyopicMemberRewards,
      saturation,
    } = metrics;
    // eslint-disable-line
    const { name, description = '', ticker, homepage } = metadata;
    const relativeStakePercentage = (0, lodash_1.get)(
      relativeStake,
      'quantity',
      0
    );
    const producedBlocksCount = (0, lodash_1.get)(
      producedBlocks,
      'quantity',
      0
    );
    const nonMyopicMemberRewardsQuantity = (0, lodash_1.get)(
      nonMyopicMemberRewards,
      'quantity',
      0
    );
    const costQuantity = (0, lodash_1.get)(cost, 'quantity', 0).toString();
    const pledgeQuantity = (0, lodash_1.get)(pledge, 'quantity', 0).toString();
    const profitMarginPercentage = (0, lodash_1.get)(
      profitMargin,
      'quantity',
      0
    );
    const retiringAt = (0, lodash_1.get)(retirement, 'epoch_start_time', null);
    return new StakePool_1.default({
      id,
      // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
      relativeStake: relativeStakePercentage,
      producedBlocks: producedBlocksCount,
      potentialRewards: new bignumber_js_1.default(
        nonMyopicMemberRewardsQuantity.toString()
      ).dividedBy(numbersConfig_1.LOVELACES_PER_ADA),
      nonMyopicMemberRewards: nonMyopicMemberRewardsQuantity,
      ticker,
      homepage,
      cost: new bignumber_js_1.default(costQuantity.toString()).dividedBy(
        numbersConfig_1.LOVELACES_PER_ADA
      ),
      description,
      isCharity: false,
      name,
      pledge: new bignumber_js_1.default(pledgeQuantity.toString()).dividedBy(
        numbersConfig_1.LOVELACES_PER_ADA
      ),
      profitMargin: profitMarginPercentage,
      ranking: index + 1,
      retiring: retiringAt ? new Date(retiringAt) : null,
      saturation: saturation * 100,
    });
  }
);
const _createRedeemItnRewardsFromServerData = (0, mobx_1.action)(
  'AdaApi::_createRedeemItnRewardsFromServerData',
  (transaction) => {
    const { quantity, unit } = (0, lodash_1.get)(
      transaction,
      'withdrawals[0].amount'
    );
    return unit === Wallet_1.WalletUnits.LOVELACE
      ? new bignumber_js_1.default(quantity.toString()).dividedBy(
          numbersConfig_1.LOVELACES_PER_ADA
        )
      : new bignumber_js_1.default(quantity.toString());
  }
);
//# sourceMappingURL=api.js.map
