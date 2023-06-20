import { action, computed, observable, runInAction } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import {
  TransactionStates,
  WalletTransaction,
} from '../domains/WalletTransaction';
import { formattedArrayBufferToHexString } from '../utils/formatters';
import walletUtils from '../utils/walletUtils';
import {
  VOTING_PHASE_CHECK_INTERVAL,
  VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
  VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL,
} from '../config/votingConfig';
import {
  votingPDFGenerator,
  VotingPDFGeneratorResult,
} from '../utils/votingPDFGenerator';
import { i18nContext } from '../utils/i18nContext';
import type { PathRoleIdentityType } from '../utils/hardwareWalletUtils';
import type {
  GetTransactionRequest,
  VotingMetadataType,
} from '../api/transactions/types';
import type { CatalystFund } from '../api/voting/types';
import { EventCategories } from '../analytics';

export type VotingRegistrationKeyType = {
  bytes: (...args: Array<any>) => any;
  public: (...args: Array<any>) => any;
};
export type VotingDataType = {
  addressHex: string;
  votingKey: string;
  stakeKey: string;
  role: PathRoleIdentityType;
  index: string;
  metadata: VotingMetadataType;
  absoluteSlotNumber: number;
};

export enum FundPhase {
  SNAPSHOT = 'snapshot',
  VOTING = 'voting',
  TALLYING = 'tallying',
  RESULTS = 'results',
}
export default class VotingStore extends Store {
  @observable
  registrationStep = 1;
  @observable
  selectedWalletId: string | null | undefined = null;
  @observable
  transactionId: string | null | undefined = null;
  @observable
  transactionConfirmations = 0;
  @observable
  isTransactionPending = false;
  @observable
  isTransactionConfirmed = false;
  @observable
  votingRegistrationKey: VotingRegistrationKeyType | null | undefined = null;
  @observable
  qrCode: string | null | undefined = null;
  @observable
  isConfirmationDialogOpen = false;
  @observable
  fundPhase?: FundPhase;
  @observable
  catalystFund: CatalystFund;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  transactionPollingInterval: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  fundPhaseInterval: IntervalID | null | undefined = null;

  setup() {
    const { voting: votingActions } = this.actions;
    votingActions.selectWallet.listen(this._setSelectedWalletId);
    votingActions.sendTransaction.listen(this._sendTransaction);
    votingActions.generateQrCode.listen(this._generateQrCode);
    votingActions.saveAsPDF.listen(this._saveAsPDF);
    votingActions.nextRegistrationStep.listen(this._nextRegistrationStep);
    votingActions.previousRegistrationStep.listen(
      this._previousRegistrationStep
    );
    votingActions.resetRegistration.listen(this._resetRegistration);
    votingActions.showConfirmationDialog.listen(this._showConfirmationDialog);
    votingActions.closeConfirmationDialog.listen(this._closeConfirmationDialog);

    this._setupFund();
  }

  @action
  _setupFund = async () => {
    await this.getCatalystFundRequest.execute().promise;
    this._initializeFundPhaseInterval();

    runInAction('Initialize fund', () => {
      this.catalystFund = this.getCatalystFundRequest.result;
      this._checkFundPhase(new Date());
    });
  };

  // REQUESTS
  @observable
  getWalletPublicKeyRequest: Request<string> = new Request(
    this.api.ada.getWalletPublicKey
  );
  @observable
  createVotingRegistrationTransactionRequest: Request<
    WalletTransaction
  > = new Request(this.api.ada.createVotingRegistrationTransaction);
  @observable
  signMetadataRequest: Request<Buffer> = new Request(
    this.api.ada.createWalletSignature
  );
  @observable
  getTransactionRequest: Request<GetTransactionRequest> = new Request(
    this.api.ada.getTransaction
  );
  @observable
  getCatalystFundRequest: Request<CatalystFund> = new Request(
    this.api.ada.getCatalystFund
  );

  // ACTIONS
  @action
  _showConfirmationDialog = () => {
    this.isConfirmationDialogOpen = true;
  };
  @action
  _closeConfirmationDialog = () => {
    this.isConfirmationDialogOpen = false;
  };
  @action
  _setSelectedWalletId = (walletId: string) => {
    this.selectedWalletId = walletId;
  };
  @action
  _nextRegistrationStep = () => {
    this.registrationStep++;
  };
  @action
  _previousRegistrationStep = () => {
    this.registrationStep--;
  };
  @action
  _resetRegistration = () => {
    this.isConfirmationDialogOpen = false;
    this.registrationStep = 1;
    this.selectedWalletId = null;
    this.transactionId = null;
    this.transactionConfirmations = 0;
    this.isTransactionPending = false;
    this.isTransactionConfirmed = false;
    this.votingRegistrationKey = null;
    this.qrCode = null;
    this.getWalletPublicKeyRequest.reset();
    this.createVotingRegistrationTransactionRequest.reset();
    this.signMetadataRequest.reset();

    if (this.transactionPollingInterval) {
      clearInterval(this.transactionPollingInterval);
    }

    if (this.fundPhaseInterval) {
      clearInterval(this.fundPhaseInterval);
    }
  };
  @action
  _startTransactionPolling = () => {
    if (this.transactionPollingInterval)
      clearInterval(this.transactionPollingInterval);
    this.transactionPollingInterval = setInterval(() => {
      this._checkVotingRegistrationTransaction();
    }, VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL);
  };
  @action
  _initializeFundPhaseInterval = () => {
    if (this.fundPhaseInterval) {
      clearInterval(this.fundPhaseInterval);
    }

    this.fundPhaseInterval = setInterval(() => {
      this._checkFundPhase(new Date());
    }, VOTING_PHASE_CHECK_INTERVAL);
  };
  @action
  _setVotingRegistrationKey = (value: VotingRegistrationKeyType) => {
    this.votingRegistrationKey = value;
  };
  @action
  _setTransactionId = (transactionId: string) => {
    this.transactionId = transactionId;
  };
  @action
  _setTransactionConfirmations = (confirmations: number) => {
    this.transactionConfirmations = confirmations;
  };
  @action
  _setIsTransactionPending = (value: boolean) => {
    this.isTransactionPending = value;
  };
  @action
  _setIsTransactionConfirmed = (value: boolean) => {
    this.isTransactionConfirmed = value;
  };
  @action
  _setQrCode = (value: string | null | undefined) => {
    this.qrCode = value;
  };
  prepareVotingData = async ({ walletId }: { walletId: string }) => {
    try {
      const [address] = await this.stores.addresses.getAddressesByWalletId(
        walletId
      );
      const addressHex = await this._getHexFromBech32(address.id);
      await this._generateVotingRegistrationKey();
      if (!this.votingRegistrationKey)
        throw new Error('Failed to generate voting registration key.');
      const votingKey = formattedArrayBufferToHexString(
        this.votingRegistrationKey.public().bytes()
      );
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const stakeKeyBech32 = await this.getWalletPublicKeyRequest.execute({
        walletId,
        role: 'mutable_account',
        index: '0',
      });
      const stakeKey = await this._getHexFromBech32(stakeKeyBech32);
      const { absoluteSlotNumber } = this.stores.networkStatus;
      const metadata = {
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
                bytes:
                  '00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000',
              },
            },
          ],
        },
      };
      const votingData = {
        address,
        addressHex,
        votingKey,
        stakeKey,
        role: 'mutable_account',
        index: '0',
        metadata,
        nonce: absoluteSlotNumber,
      };
      return votingData;
    } catch (e) {
      throw e;
    }
  };
  _sendTransaction = async ({
    amount,
    passphrase,
  }: {
    amount: number;
    passphrase: string | null | undefined;
  }) => {
    const walletId = this.selectedWalletId;
    if (!walletId)
      throw new Error(
        'Selected wallet required before send voting registration.'
      );
    const [address] = await this.stores.addresses.getAddressesByWalletId(
      walletId
    );
    const selectedWallet = this.stores.wallets.getWalletById(walletId);
    const isHardwareWallet = get(selectedWallet, 'isHardwareWallet', false);
    const { absoluteSlotNumber } = this.stores.networkStatus;

    // Reset voting registration transaction state
    this._setIsTransactionPending(true);

    this._setIsTransactionConfirmed(false);

    // Reset voting registration requests
    this.getWalletPublicKeyRequest.reset();
    this.createVotingRegistrationTransactionRequest.reset();
    this.signMetadataRequest.reset();
    let transaction;

    try {
      if (isHardwareWallet) {
        transaction = await this.stores.hardwareWallets._sendMoney({
          isVotingRegistrationTransaction: true,
          selectedWalletId: walletId,
        });
      } else {
        const votingData = await this.prepareVotingData({
          walletId,
        });
        const { addressHex, votingKey, stakeKey, role, index } = votingData;
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        const signature = await this.signMetadataRequest.execute({
          addressHex,
          walletId,
          passphrase,
          votingKey,
          stakeKey,
          role,
          index,
          absoluteSlotNumber,
        });
        transaction =
          // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
          await this.createVotingRegistrationTransactionRequest.execute({
            address: address.id,
            addressHex,
            amount,
            passphrase,
            walletId,
            votingKey,
            stakeKey,
            signature: signature.toString('hex'),
            absoluteSlotNumber,
          });
      }

      this._setTransactionId(transaction.id);

      if (!isHardwareWallet) {
        this._startTransactionPolling();

        this._nextRegistrationStep();
      }
    } catch (error) {
      if (error.code === 'wrong_encryption_passphrase') {
        // In case of a invalid spending password we stay on the same screen
        this._setIsTransactionPending(false);
      } else {
        // For any other error code we proceed to the next screen
        this._nextRegistrationStep();
      }

      throw error;
    }
  };
  _generateQrCode = async (pinCode: number) => {
    const { symmetric_encrypt: symmetricEncrypt } = await walletUtils;
    const password = new Uint8Array(4);
    pinCode
      .toString()
      .split('')
      .forEach((value: string, index: number) => {
        password[index] = parseInt(value, 10);
      });
    if (!this.votingRegistrationKey)
      throw new Error(
        'Failed to generate QR code due to missing voting registration key.'
      );
    const encrypt = symmetricEncrypt(
      password,
      this.votingRegistrationKey.bytes()
    );

    this._setQrCode(formattedArrayBufferToHexString(encrypt));

    this._nextRegistrationStep();
    this.analytics.sendEvent(EventCategories.VOTING, 'Registered for voting');
  };
  _saveAsPDF = async () => {
    const { qrCode, selectedWalletId } = this;
    if (!qrCode || !selectedWalletId) return;
    const selectedWallet = this.stores.wallets.getWalletById(selectedWalletId);
    if (!selectedWallet) return;
    const { name: walletName } = selectedWallet;
    const { desktopDirectoryPath } = this.stores.profile;
    const {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    } = this.stores.profile;
    const nextVotingFundNumber = this.catalystFund?.next?.number;
    const { network, isMainnet } = this.environment;
    const intl = i18nContext(currentLocale);

    const result = await votingPDFGenerator({
      nextVotingFundNumber,
      qrCode,
      walletName,
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
      desktopDirectoryPath,
      network,
      isMainnet,
      intl,
    });

    if (result === VotingPDFGeneratorResult.FileSaved) {
      this.actions.voting.saveAsPDFSuccess.trigger();
    }
  };
  _checkVotingRegistrationTransaction = async () => {
    const {
      confirmations,
      state,
    }: // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    WalletTransaction = await this.getTransactionRequest.execute({
      walletId: this.selectedWalletId,
      transactionId: this.transactionId,
    });

    // Update voting registration confirmations count
    if (this.transactionConfirmations !== confirmations) {
      this._setTransactionConfirmations(confirmations);
    }

    // Update voting registration pending state
    if (this.isTransactionPending && state === TransactionStates.OK) {
      this._setIsTransactionPending(false);
    }

    // Update voting registration confirmed state
    if (
      !this.isTransactionConfirmed &&
      confirmations >= VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS
    ) {
      this._setIsTransactionConfirmed(true);

      if (this.transactionPollingInterval)
        clearInterval(this.transactionPollingInterval);
    }
  };
  @action
  _checkFundPhase = (now: Date) => {
    const phaseValidation = {
      [FundPhase.SNAPSHOT]: (date: Date) =>
        date < this.catalystFund?.current?.startTime,
      [FundPhase.VOTING]: (date: Date) =>
        date >= this.catalystFund?.current?.startTime &&
        date < this.catalystFund?.current?.endTime,
      [FundPhase.TALLYING]: (date: Date) =>
        date >= this.catalystFund?.current?.endTime &&
        date < this.catalystFund?.current?.resultsTime,
      [FundPhase.RESULTS]: (date: Date) =>
        date >= this.catalystFund?.current?.resultsTime,
    };
    this.fundPhase =
      Object.values(FundPhase).find((phase) => phaseValidation[phase](now)) ||
      null;
  };
  _generateVotingRegistrationKey = async () => {
    const { Ed25519ExtendedPrivate: extendedPrivateKey } = await walletUtils;

    this._setVotingRegistrationKey(extendedPrivateKey.generate());
  };
  _getHexFromBech32 = async (key: string): Promise<string> => {
    const { bech32_decode_to_bytes: decodeBech32ToBytes } = await walletUtils;
    return formattedArrayBufferToHexString(decodeBech32ToBytes(key));
  };

  // GETTERS
  @computed
  get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed
  get isVotingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.VOTING.REGISTRATION) > -1;
  }
}
