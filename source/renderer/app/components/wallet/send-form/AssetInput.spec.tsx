import React from 'react';
import BigNumber from 'bignumber.js';
import { addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import { render, fireEvent, screen, cleanup } from '@testing-library/react';
import '@testing-library/jest-dom';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { TestDecorator } from '../../../../../../tests/_utils/TestDecorator';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedAmountToNaturalUnits } from '../../../utils/formatters';
import { NUMBER_OPTIONS } from '../../../config/profileConfig';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { NUMBER_FORMATS } from '../../../../../common/types/number.types';
import AssetInput from './AssetInput';

const policyId = '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7';
// 'Cointest'.
const assetName = '436f696e74657374';
const uniqueId = `${policyId}${assetName}`;
const fieldName = `asset_${uniqueId}`;

const buildAsset = (decimals: number | null | undefined) => ({
  policyId,
  assetName,
  uniqueId,
  fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
  quantity: new BigNumber('900000000'),
  decimals,
  recommendedDecimals: null,
  metadata: {
    name: 'Test Coin',
    description: 'A test coin',
    ticker: 'TEST',
  },
});

type AssetFormFields = {
  [assetField: string]: string;
};

const buildForm = () =>
  new ReactToolboxMobxForm<AssetFormFields>(
    {
      fields: {
        [fieldName]: {
          label: 'Amount',
          placeholder: '0',
          value: null,
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: { validateOnChange: false, validateOnBlur: false },
    }
  );

const renderAssetInput = (
  decimals: number | null | undefined,
  numberFormat: string = NUMBER_OPTIONS[0].value
) => {
  const form = buildForm();
  const field = form.$(fieldName);
  const asset = buildAsset(decimals);
  render(
    <TestDecorator>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>
          <AssetInput
            uniqueId={uniqueId}
            getAssetByUniqueId={() => asset}
            assetFields={{ [uniqueId]: field }}
            addFocusableField={() => {}}
            currentNumberFormat={NUMBER_FORMATS[numberFormat]}
            removeAssetRow={() => {}}
            handleSubmitOnEnter={() => {}}
            clearAssetFieldValue={() => {}}
            autoFocus={false}
          />
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </TestDecorator>
  );
  return {
    field,
    input: screen.getByTestId(`assetInput:${uniqueId}`),
  };
};

// One `change` event per cumulative value is what a keystroke sequence produces
// in jsdom. A paste produces a single event carrying the whole string. Both
// reach the same refusal, which happens before react-polymorph reads the
// event's `inputType`, so the two are not distinguishable here and are driven
// as what they are: change events.
const type = (input: HTMLElement, values: Array<string>) =>
  values.forEach((value) => fireEvent.change(input, { target: { value } }));

const paste = (input: HTMLElement, value: string) =>
  fireEvent.change(input, { target: { value } });

describe('AssetInput', () => {
  beforeEach(() => addLocaleData([...en]));
  afterEach(cleanup);

  describe('when the decimal places are unknown', () => {
    it('refuses a typed decimal amount', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1', '1.', '1.5']);
      expect(field.value).toEqual('1');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1');
    });

    it('refuses a typed decimal amount when the decimal places are null', () => {
      const { field, input } = renderAssetInput(null);
      type(input, ['1', '1.', '1.5']);
      expect(field.value).toEqual('1');
    });

    it('never hands a refused character to the form', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1']);
      const onChange = jest.spyOn(field, 'onChange');
      type(input, ['1.', '1.5']);
      expect(onChange).not.toHaveBeenCalled();
      onChange.mockRestore();
    });

    it('refuses a pasted decimal amount', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1']);
      paste(input, '1.5');
      expect(field.value).toEqual('1');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1');
    });

    it('refuses a pasted decimal amount written with a comma', () => {
      // NUMBER_OPTIONS[1] is the profile whose decimal separator is a comma.
      const { field, input } = renderAssetInput(
        undefined,
        NUMBER_OPTIONS[1].value
      );
      type(input, ['1']);
      paste(input, '1,5');
      expect(field.value).toEqual('1');
    });

    it('refuses a pasted amount carrying group separators', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1']);
      paste(input, '1,234');
      expect(field.value).toEqual('1');
    });

    it('refuses pasted text that is not a number', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1']);
      paste(input, 'abc');
      expect(field.value).toEqual('1');
    });

    it('submits the raw units it was given', () => {
      const { field, input } = renderAssetInput(undefined);
      type(input, ['1500000']);
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1500000');
    });

    it('renders a large amount without group separators', () => {
      const { input } = renderAssetInput(undefined);
      type(input, ['1234567']);
      expect(input).toHaveValue('1234567');
    });
  });

  describe('when the decimal places are zero', () => {
    it('refuses a typed decimal amount', () => {
      const { field, input } = renderAssetInput(0);
      type(input, ['1', '1.', '1.5']);
      expect(field.value).toEqual('1');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1');
    });

    it('refuses a pasted decimal amount rather than rounding it', () => {
      const { field, input } = renderAssetInput(0);
      type(input, ['1']);
      paste(input, '1.5');
      expect(field.value).toEqual('1');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1');
    });
  });

  describe('when the decimal places are known', () => {
    it('accepts a typed decimal amount and submits it in natural units', () => {
      const { field, input } = renderAssetInput(6);
      type(input, ['1', '1.', '1.5']);
      expect(field.value).toEqual('1.500000');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1500000');
    });

    it('accepts a pasted decimal amount and submits it in natural units', () => {
      const { field, input } = renderAssetInput(6);
      paste(input, '1.5');
      expect(field.value).toEqual('1.500000');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1500000');
    });

    it('submits the smallest expressible amount without rounding it away', () => {
      const { field, input } = renderAssetInput(6);
      paste(input, '0.000001');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1');
    });
  });
});
