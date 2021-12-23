// @flow
import React from 'react';
import { IntlProvider, addLocaleData } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Provider as MobxProvider } from 'mobx-react';
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
import translations from '../../i18n/locales/en-US.json';
import StoryDecorator from '../../../../../storybook/stories/_support/StoryDecorator';
import { sendFormAssetData } from '../../../../../storybook/stories/wallets/send/WalletSend.stories';

import { NUMBER_OPTIONS } from '../../config/profileConfig';
import { DiscreetModeFeatureProvider } from '../../features/discreet-mode';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import { HwDeviceStatuses } from '../../domains/Wallet';

// Screens
import WalletSendForm from './WalletSendForm';

describe('wallet/Wallet Send Form', () => {
  beforeEach(() => addLocaleData([...en]));
  afterEach(cleanup);

  const currencyMaxFractionalDigits = 6;

  function TestDecorator({
    calculateTransactionFee,
  }: {
    calculateTransactionFee: Function,
  }) {
    return (
      <StoryDecorator>
        <IntlProvider locale="en-US" messages={translations}>
          <BrowserLocalStorageBridge>
            <DiscreetModeFeatureProvider>
              <MobxProvider>
                <WalletSendForm
                  currencyMaxFractionalDigits={currencyMaxFractionalDigits}
                  currencyMaxIntegerDigits={11}
                  currentNumberFormat={NUMBER_OPTIONS[0].value}
                  validateAmount={jest.fn().mockResolvedValue(true)}
                  validateAssetAmount={jest.fn().mockResolvedValue(true)}
                  calculateTransactionFee={calculateTransactionFee}
                  walletAmount={new BigNumber(123)}
                  assets={sendFormAssetData}
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
        </IntlProvider>
      </StoryDecorator>
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

  async function addToken() {
    const addTokenButton = await screen.findByText('+ Add a token');
    fireEvent.click(addTokenButton);

    const token = getInput('Token #1');
    fireEvent.change(token, {
      target: {
        value: 1,
      },
    });

    return async () => {
      fireEvent.mouseEnter(token);
      const removeTokenButton = await screen.findByText('Remove');
      fireEvent.click(removeTokenButton);
    };
  }

  async function waitForMinimumAdaRequiredMsg() {
    const minimumAdaRequiredMsg = screen.getByTestId('minimumAdaRequiredMsg');
    await within(minimumAdaRequiredMsg).findByText(
      `a minimum of 2 ADA required`
    );
  }

  function assertAdaInput(amount: number) {
    const adaInput = getInput('Ada');
    expect(adaInput).toHaveValue(
      new BigNumber(amount).toFormat(currencyMaxFractionalDigits)
    );
  }

  test('should update Ada input field to minimum required and restore to original value when tokens are removed', async () => {
    expect.assertions(4);

    const minimumAda = 2;

    const calculateTransactionFeeStub = jest
      .fn()
      .mockResolvedValue({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(1),
      })
      // $FlowFixMe
      .mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(minimumAda),
      })
      // $FlowFixMe
      .mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(minimumAda),
      });

    render(
      <TestDecorator calculateTransactionFee={calculateTransactionFeeStub} />
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

    const calculateTransactionFeeStub = jest
      .fn()
      .mockResolvedValue({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(1),
      })
      // $FlowFixMe
      .mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(2),
      })
      // $FlowFixMe
      .mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(2),
      });

    render(
      <TestDecorator calculateTransactionFee={calculateTransactionFeeStub} />
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
    expect.assertions(3);

    const minimumAda = 2;

    const calculateTransactionFeeStub = jest
      .fn()
      .mockResolvedValue({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(1),
      })
      // $FlowFixMe
      .mockResolvedValueOnce({
        fee: new BigNumber(1),
        minimumAda: new BigNumber(minimumAda),
      });

    render(
      <TestDecorator calculateTransactionFee={calculateTransactionFeeStub} />
    );

    enterReceiverAddress();

    const removeToken1 = await addToken();

    await waitForMinimumAdaRequiredMsg();

    assertAdaInput(minimumAda);

    const userAdaAmount = 1.5;
    const adaInput = getInput('Ada');
    fireEvent.change(adaInput, { target: { value: userAdaAmount } });

    const minimumAdaRequiredMsg = screen.getByTestId('minimumAdaRequiredMsg');

    expect(minimumAdaRequiredMsg).toHaveTextContent(
      `UPDATE to the minimum of ${minimumAda} ADA required`
    );

    await removeToken1();

    const minimumAmountNotice = await screen.findByTestId(
      'WalletSendForm::minimumAmountNotice::restored'
    );

    expect(minimumAmountNotice).toHaveTextContent(
      `Note: the ada field was automatically updated to ${userAdaAmount} ADA because now it fulfills the minimum amount of 1 ADA for the transaction.`
    );
  });

  test('should not display any minimum amount notice message when ada input is greater than minimum amount', async () => {
    expect.assertions(2);

    const calculateTransactionFeeStub = jest.fn().mockResolvedValue({
      fee: new BigNumber(1),
      minimumAda: new BigNumber(2),
    });

    render(
      <TestDecorator calculateTransactionFee={calculateTransactionFeeStub} />
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
});
