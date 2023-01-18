import React, { useState } from 'react';
import { addLocaleData } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Provider as MobxProvider } from 'mobx-react';
import faker from '@faker-js/faker';
import {
  render,
  fireEvent,
  screen,
  cleanup,
  within,
  waitForElementToBeRemoved,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import en from 'react-intl/locale-data/en';
// Assets and helpers
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';
import { NUMBER_OPTIONS } from '../../config/profileConfig';
import { DiscreetModeFeatureProvider } from '../../features/discreet-mode';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import { HwDeviceStatuses } from '../../domains/Wallet';
import WalletTokenPicker from './tokens/wallet-token-picker/WalletTokenPicker';
import WalletSendForm, { FormData } from './WalletSendForm';
import { noopAnalyticsTracker } from '../../analytics';

jest.mock(
  '../../containers/wallet/dialogs/send-confirmation/SendConfirmation.container',
  () => {
    function Dialog({
      amount,
      formattedTotalAmount,
    }: {
      amount: number;
      formattedTotalAmount: number;
    }) {
      return (
        <div>
          <span data-testid="confirmation-dialog-ada-amount">{amount}</span>
          <span data-testid="confirmation-dialog-total-amount">
            {formattedTotalAmount}
          </span>
        </div>
      );
    }

    return {
      __esModule: true,
      WalletSendConfirmationDialogContainer: Dialog,
    };
  }
);

describe('wallet/Wallet Send Form', () => {
  beforeEach(() => addLocaleData([...en]));
  afterEach(cleanup);
  const currencyMaxFractionalDigits = 6;

  function createAssets(index: number) {
    const id = `${faker.datatype.uuid()}:${index}`;
    return {
      policyId: id,
      assetName: faker.internet.domainWord(),
      uniqueId: id,
      fingerprint: faker.datatype.uuid(),
      quantity: new BigNumber(faker.finance.amount()),
      decimals: 0,
      recommendedDecimals: null,
      metadata: {
        name: id,
        ticker: faker.finance.currencyCode(),
        description: '',
      },
    };
  }

  const assets = [createAssets(0), createAssets(1)];

  function SetupWallet({
    calculateTransactionFee,
    currentNumberFormat = NUMBER_OPTIONS[0].value,
    validationDebounceWait,
    validateAmount = jest.fn().mockResolvedValue(true),
    onTransactionFeeChange = jest.fn(),
  }: {
    calculateTransactionFee: (...args: Array<any>) => any;
    validateAmount?: (amount: string) => Promise<boolean>;
    currentNumberFormat?: string;
    validationDebounceWait?: number;
    onTransactionFeeChange?: () => Promise<void>;
  }) {
    const [tokenPickerOpen, setTokenPickerOpen] = useState<boolean>(false);
    const [state, setState] = useState<{
      isDialogOpen: boolean;
      formData: FormData;
    }>({ isDialogOpen: false, formData: null });

    return (
      <TestDecorator>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <MobxProvider>
              <WalletSendForm
                currencyMaxFractionalDigits={currencyMaxFractionalDigits}
                currencyMaxIntegerDigits={11}
                currentNumberFormat={currentNumberFormat}
                validateAmount={validateAmount}
                validateAssetAmount={jest.fn().mockResolvedValue(true)}
                calculateTransactionFee={calculateTransactionFee}
                walletAmount={new BigNumber(123)}
                assets={assets}
                addressValidator={() => true}
                onSubmit={(formData) =>
                  setState({ isDialogOpen: true, formData })
                }
                isDialogOpen={(dialog) =>
                  (dialog === WalletTokenPicker && tokenPickerOpen) ||
                  state.isDialogOpen
                }
                isRestoreActive={false}
                hwDeviceStatus={HwDeviceStatuses.READY}
                isHardwareWallet={false}
                isLoadingAssets={false}
                onExternalLinkClick={jest.fn()}
                hasAssets
                selectedAsset={null}
                onUnsetActiveAsset={() => {}}
                isAddressFromSameWallet={false}
                tokenFavorites={{}}
                walletName={faker.name.firstName()}
                onTokenPickerDialogClose={() => setTokenPickerOpen(false)}
                onTokenPickerDialogOpen={() => setTokenPickerOpen(true)}
                analyticsTracker={noopAnalyticsTracker}
                confirmationDialogData={state.formData}
                validationDebounceWait={validationDebounceWait}
                onTransactionFeeChange={onTransactionFeeChange}
              />
            </MobxProvider>
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </TestDecorator>
    );
  }

  function enterReceiverAddress() {
    const address =
      'addr_test1qrjzmxr4x7vhlusn05fd4lt7cs6dy8wtcv6vaf9lff7m9yqkw6whlsg36t3laez562llhkvfy5tny4p9y8zrspe48vgsea3q6m';
    const receiverAddress = screen.getByPlaceholderText('Paste an address');
    fireEvent.change(receiverAddress, {
      target: {
        value: address,
      },
    });
  }

  function getInput(label: string) {
    return screen.getByLabelText(label);
  }

  async function findInput(label: string) {
    return screen.findByLabelText(label);
  }

  async function addToken(value = 1, tokenIndex = 0) {
    const addTokenButton = await screen.findByText('+ Add a token');
    fireEvent.click(addTokenButton);
    const tokenPicker = await screen.findByTestId('WalletTokenPicker');
    const tokenCheckbox = tokenPicker.querySelectorAll(
      'input[type="checkbox"]'
    )[tokenIndex];
    fireEvent.click(tokenCheckbox);
    const addTokenPickerButton = await screen.findByText('Add');
    fireEvent.click(addTokenPickerButton);

    const { uniqueId } = assets[tokenIndex];

    const token = await screen.findByTestId(`assetInput:${uniqueId}`);
    fireEvent.change(token, {
      target: {
        value,
      },
    });
    return async () => {
      fireEvent.mouseEnter(token);
      const removeTokenButton = await screen.findByTestId(
        `removeAsset:${uniqueId}`
      );
      fireEvent.click(removeTokenButton);
    };
  }

  async function waitForMinimumAdaRequiredMsg(minimumAda = 2) {
    const minimumAdaRequiredMsg = screen.getByTestId('minimumAdaRequiredMsg');
    await within(minimumAdaRequiredMsg).findByText(
      `a minimum of ${minimumAda} ADA required`
    );
  }

  function assertAdaInput(amount: number) {
    const adaInput = getInput('Ada');
    expect(adaInput).toHaveValue(
      new BigNumber(amount).toFormat(currencyMaxFractionalDigits)
    );
  }

  function createTransactionFeeMock(times: number, minimumAda: number) {
    const mock = jest.fn().mockResolvedValue({
      fee: new BigNumber(1),
      minimumAda: new BigNumber(1),
    });
    Array.from({
      length: times,
    }).forEach(() => {
      // @ts-ignore
      mock.mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(minimumAda),
      });
    });
    return mock;
  }

  const MINIMUM_AMOUNT_UPDATED_MESSAGE_TEST_ID =
    'WalletSendForm::minimumAmountNotice::updated';

  function assertMinimumAmountNoticeMessage(minimumAda: number) {
    expect(
      screen.getByTestId(MINIMUM_AMOUNT_UPDATED_MESSAGE_TEST_ID)
    ).toHaveTextContent(
      `Note: the ada field was automatically updated because this transaction requires a minimum of ${minimumAda} ADA.`
    );
  }

  function sleep(ms) {
    return new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
  }

  async function waitForTransactionFee() {
    const transactionFeeSpinner = screen.getByTestId('transaction-fee-spinner');

    return waitForElementToBeRemoved(transactionFeeSpinner);
  }

  test('should update Ada input field to minimum required and restore to original value when tokens are removed', async () => {
    expect.assertions(4);

    const minimumAda = 2;
    const calculateTransactionFeeMock = createTransactionFeeMock(1, minimumAda);

    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );

    enterReceiverAddress();

    const removeToken1 = await addToken();

    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(minimumAda);

    const minimumAmountNoticeTestId =
      'WalletSendForm::minimumAmountNotice::updated';

    expect(screen.getByTestId(minimumAmountNoticeTestId)).toHaveTextContent(
      `Note: the ada field was automatically updated because this transaction requires a minimum of ${minimumAda} ADA.`
    );

    await removeToken1();

    await waitForMinimumAdaRequiredMsg(1);

    assertAdaInput(1);

    expect(screen.queryByTestId(minimumAmountNoticeTestId)).toBeInTheDocument();
  });

  test('should display an update button when Ada input field is less than minimum required', async () => {
    expect.assertions(3);
    const minimumAda = 2;
    const calculateTransactionFeeMock = createTransactionFeeMock(2, minimumAda);
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    await addToken();
    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(minimumAda);
    const lowerAdaValue = 1.5;
    const adaInput = getInput('Ada');
    fireEvent.change(adaInput, {
      target: {
        value: lowerAdaValue,
      },
    });
    const minimumAdaRequiredMsg = screen.getByTestId('minimumAdaRequiredMsg');
    const updateButton = await within(minimumAdaRequiredMsg).findByText(
      'UPDATE'
    );
    assertAdaInput(lowerAdaValue);
    fireEvent.click(updateButton);
    await waitForElementToBeRemoved(updateButton);
    assertAdaInput(minimumAda);
  });

  test('should favour user Ada input instead of minimum required when the value is greater than the minimum one', async () => {
    expect.assertions(2);
    const minimumAda = 2.5;
    const calculateTransactionFeeMock = createTransactionFeeMock(2, minimumAda);
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    const userAdaAmount = 1.5;
    const adaInput = await findInput('Ada');
    fireEvent.change(adaInput, {
      target: {
        value: userAdaAmount,
      },
    });
    const removeToken1 = await addToken();
    await waitForMinimumAdaRequiredMsg(minimumAda);
    assertAdaInput(minimumAda);
    await removeToken1();
    const minimumAmountNotice = await screen.findByTestId(
      'WalletSendForm::minimumAmountNotice::restored'
    );
    expect(minimumAmountNotice).toHaveTextContent(
      `Note: the ada field was automatically updated to ${userAdaAmount} ADA because now it fulfills the minimum amount of 1 ADA for the transaction.`
    );
  });

  test('should remove message when user entry is higher than minimum amount', async () => {
    expect.assertions(3);
    const minimumAda = 2.5;
    const calculateTransactionFeeMock = createTransactionFeeMock(2, minimumAda);
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    const userAdaAmount = 1.5;
    const adaInput = await findInput('Ada');
    fireEvent.change(adaInput, {
      target: {
        value: userAdaAmount,
      },
    });
    const removeToken1 = await addToken();
    await waitForMinimumAdaRequiredMsg(minimumAda);
    assertAdaInput(minimumAda);
    await removeToken1();
    const minimumAmountNoticeTestId =
      'WalletSendForm::minimumAmountNotice::restored';
    const minimumAmountNotice = await screen.findByTestId(
      minimumAmountNoticeTestId
    );
    expect(minimumAmountNotice).toBeInTheDocument();
    fireEvent.change(adaInput, {
      target: {
        value: userAdaAmount + 1,
      },
    });
    expect(
      screen.queryByTestId(minimumAmountNoticeTestId)
    ).not.toBeInTheDocument();
  });

  test('should not display any minimum amount notice message when ada input is greater than minimum amount', async () => {
    expect.assertions(2);
    const calculateTransactionFeeMock = jest.fn().mockResolvedValue({
      fee: new BigNumber(1),
      minimumAda: new BigNumber(2),
    });
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    const userAdaAmount = 3.5;
    const adaInput = await findInput('Ada');
    fireEvent.change(adaInput, {
      target: {
        value: userAdaAmount,
      },
    });
    await addToken();
    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(userAdaAmount);
    await expect(
      screen.findByTestId('WalletSendForm::minimumAmountNotice::restored')
    ).rejects.toBeTruthy();
  });

  test('should apply minimum fee to ada field when user has removed the previous update', async () => {
    expect.assertions(3);
    const minimumAda = 2;
    const calculateTransactionFeeMock = createTransactionFeeMock(4, minimumAda);
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    const removeToken1 = await addToken();
    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(minimumAda);
    const adaInput = await findInput('Ada');
    fireEvent.change(adaInput, {
      target: {
        value: '',
      },
    });
    await removeToken1();
    expect(adaInput).toHaveValue('');
    await addToken();
    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(minimumAda);
  });

  test('should format ada input field using numeric format profile', async () => {
    expect.assertions(1);
    const minimumAda = 2;
    const calculateTransactionFeeMock = createTransactionFeeMock(4, minimumAda);
    render(
      <SetupWallet
        calculateTransactionFee={calculateTransactionFeeMock}
        currentNumberFormat={NUMBER_OPTIONS[1].value}
      />
    );
    enterReceiverAddress();
    await addToken();
    await waitForMinimumAdaRequiredMsg();
    const adaInput = getInput('Ada');
    expect(adaInput).toHaveValue(`${minimumAda},000000`);
  });

  test('should calculate transaction fee even when one of the assets are empty', async () => {
    expect.assertions(2);
    const minimumAda = 2;
    const calculateTransactionFeeMock = createTransactionFeeMock(4, minimumAda);
    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );
    enterReceiverAddress();
    await addToken(0);
    await waitForMinimumAdaRequiredMsg(1);
    expect(getInput('Ada')).toHaveValue('');
    await addToken(minimumAda, 1);
    await waitForMinimumAdaRequiredMsg();
    assertAdaInput(minimumAda);
  });

  test('should keep transaction fee when assets are removed and ada field is untouched', async () => {
    expect.assertions(3);

    const fee = 2;
    const minimumAda = 1;
    const calculateTransactionFeeMock = createTransactionFeeMock(1, fee);

    render(
      <SetupWallet calculateTransactionFee={calculateTransactionFeeMock} />
    );

    enterReceiverAddress();

    const removeToken = await addToken();
    await waitForMinimumAdaRequiredMsg();

    assertAdaInput(fee);

    await removeToken();

    await waitForMinimumAdaRequiredMsg(minimumAda);

    assertAdaInput(minimumAda);
    assertMinimumAmountNoticeMessage(minimumAda);
  });

  type Cases = [
    Array<[number, number]>,
    [number, number],
    Array<[number, number]>,
    [string, string, number]
  ];

  const cases: Cases[] = [
    [
      // ada inputs [delay, value]
      [
        [0, 2.5],
        [0, 1.5],
      ],
      // validate amount [delay]
      [50, 5],
      // fee calculation [delay, value]
      [
        [50, 2],
        [0, 1],
      ],
      ['1.500000', '3.500000', 1],
    ],
    [
      [
        [0, 2.5],
        [0, 1.5],
      ],
      [0, 0],
      [
        [0, 1],
        [0, 2],
      ],
      ['1.500000', '3.500000', 1],
    ],
    [
      [
        [50, 2.5],
        [0, 1.5],
      ],
      [0, 0],
      [
        [0, 1],
        [0, 2],
      ],
      ['1.500000', '3.500000', 2],
    ],
  ];

  cases.forEach(
    (
      [
        inputs,
        validateAmountParams,
        feeCalculationParams,
        [expectedAdaAmount, expectedTotalAmount, expectedTimesFeeCalled],
      ],
      idx
    ) => {
      test(`case ${idx}: should not allow to submit before fees are calculated`, async () => {
        expect.assertions(5);

        const validationDebounceWait = 0;
        const onTransactionFeeChangeSpy = jest.fn();

        const calculateTransactionFeeMock = jest.fn();

        feeCalculationParams.forEach(([delay, value]) => {
          calculateTransactionFeeMock.mockImplementationOnce(
            () =>
              new Promise(async (resolve) => {
                await sleep(delay);
                resolve({
                  fee: new BigNumber(value),
                  minimumAda: new BigNumber(1),
                });
              })
          );
        });

        const validateAmountMock = jest.fn();

        validateAmountParams.forEach((delay) => {
          validateAmountMock.mockImplementationOnce(
            () =>
              new Promise(async (resolve) => {
                await sleep(delay);
                return resolve(true);
              })
          );
        });

        render(
          <SetupWallet
            validateAmount={validateAmountMock}
            calculateTransactionFee={calculateTransactionFeeMock}
            validationDebounceWait={validationDebounceWait}
            onTransactionFeeChange={onTransactionFeeChangeSpy}
          />
        );

        enterReceiverAddress();

        const adaField = await screen.findByLabelText('Ada');

        for (const [delay, value] of inputs) {
          fireEvent.change(adaField, {
            target: {
              value,
            },
          });

          await sleep(delay);
        }

        const sendButton: HTMLButtonElement = screen.getByText('Send');
        expect(sendButton).not.toBeEnabled();

        await waitForTransactionFee();

        expect(sendButton).toBeEnabled();

        fireEvent.keyPress(adaField, {
          key: 'Enter',
          code: 13,
          charCode: 13,
          target: adaField,
        });

        const adaAmountConfirmation = await screen.findByTestId(
          'confirmation-dialog-ada-amount',
          {}
        );

        expect(adaAmountConfirmation).toHaveTextContent(expectedAdaAmount);

        const totalAmountConfirmation = screen.getByTestId(
          'confirmation-dialog-total-amount',
          {}
        );

        expect(totalAmountConfirmation).toHaveTextContent(expectedTotalAmount);
        expect(onTransactionFeeChangeSpy).toHaveBeenCalledTimes(
          expectedTimesFeeCalled
        );
      });
    }
  );
});
