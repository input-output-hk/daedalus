// @flow
import React from 'react';
import { addLocaleData } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Provider as MobxProvider } from 'mobx-react';
import faker from 'faker';
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

import WalletSendForm from './WalletSendForm';

describe('wallet/Wallet Send Form', () => {
  beforeEach(() => addLocaleData([...en]));
  afterEach(cleanup);

  const currencyMaxFractionalDigits = 6;

  function createAssets() {
    return {
      policyId: faker.random.uuid(),
      assetName: faker.internet.domainWord(),
      uniqueId: faker.random.uuid(),
      fingerprint: faker.random.uuid(),
      quantity: new BigNumber(faker.finance.amount()),
      decimals: 0,
      recommendedDecimals: null,
      metadata: {
        name: faker.internet.domainWord(),
        ticker: faker.finance.currencyCode(),
        description: '',
      },
    };
  }

  function SetupWallet({
    calculateTransactionFee,
    currentNumberFormat = NUMBER_OPTIONS[0].value,
  }: {
    calculateTransactionFee: Function,
    currentNumberFormat?: string,
  }) {
    return (
      <TestDecorator>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <MobxProvider>
              <WalletSendForm
                currencyMaxFractionalDigits={currencyMaxFractionalDigits}
                currencyMaxIntegerDigits={11}
                currentNumberFormat={currentNumberFormat}
                validateAmount={jest.fn().mockResolvedValue(true)}
                validateAssetAmount={jest.fn().mockResolvedValue(true)}
                calculateTransactionFee={calculateTransactionFee}
                walletAmount={new BigNumber(123)}
                assets={[createAssets(), createAssets()]}
                addressValidator={() => true}
                onOpenDialogAction={jest.fn()}
                isDialogOpen={jest.fn()}
                isRestoreActive={false}
                hwDeviceStatus={HwDeviceStatuses.READY}
                isHardwareWallet={false}
                isLoadingAssets={false}
                onExternalLinkClick={jest.fn()}
                hasAssets
                selectedAsset={null}
                onUnsetActiveAsset={() => {}}
                isAddressFromSameWallet={false}
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
    return screen.getByText(label).parentElement.querySelector('input');
  }

  async function findInput(label: string) {
    const labelNode = await screen.findByText(label);
    return labelNode.parentElement.querySelector('input');
  }

  async function addToken(value: number = 1, tokenIndex: number = 1) {
    const addTokenButton = await screen.findByText('+ Add a token');
    fireEvent.click(addTokenButton);

    const token = getInput(`Token #${tokenIndex}`);
    fireEvent.change(token, {
      target: {
        value,
      },
    });

    return async () => {
      fireEvent.mouseEnter(token);
      const removeTokenButton = await screen.findByText('Remove');
      fireEvent.click(removeTokenButton);
    };
  }

  async function waitForMinimumAdaRequiredMsg(minimumAda: number = 2) {
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

    Array.from({ length: times }).forEach(() => {
      // $FlowFixMe
      mock.mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(minimumAda),
      });
    });

    return mock;
  }

  test('should update Ada input field to minimum required and restore to original value when tokens are removed', async () => {
    expect.assertions(4);

    const minimumAda = 2;

    const calculateTransactionFeeMock = createTransactionFeeMock(2, minimumAda);

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

    const minimumAdaRequiredMsg = screen.getByTestId('minimumAdaRequiredMsg');
    await within(minimumAdaRequiredMsg).findByText(
      `to the minimum of 1 ADA required`
    );

    assertAdaInput(0);
    expect(
      screen.queryByTestId(minimumAmountNoticeTestId)
    ).not.toBeInTheDocument();
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
    fireEvent.change(adaInput, { target: { value: lowerAdaValue } });

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
    fireEvent.change(adaInput, { target: { value: userAdaAmount } });

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
    fireEvent.change(adaInput, { target: { value: userAdaAmount } });

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

    fireEvent.change(adaInput, { target: { value: userAdaAmount + 1 } });

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
    fireEvent.change(adaInput, { target: { value: userAdaAmount } });

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
    fireEvent.change(adaInput, { target: { value: '' } });

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

    await addToken(minimumAda, 2);
    await waitForMinimumAdaRequiredMsg();

    assertAdaInput(minimumAda);
  });
});
