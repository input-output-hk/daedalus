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

const buildAsset = (
  decimals: number | null | undefined,
  metadata: Record<string, unknown> | null = {
    name: 'Test Coin',
    description: 'A test coin',
    ticker: 'TEST',
  }
) => ({
  policyId,
  assetName,
  uniqueId,
  fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
  quantity: new BigNumber('900000000'),
  decimals,
  recommendedDecimals: null,
  metadata,
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

/**
 * `decimals` is the row's snapshotted denomination and is what the component
 * obeys. `assetDecimals` is what the asset itself currently says, which the row
 * deliberately ignores; it defaults to the snapshot so that every case that does
 * not care about the difference reads as it did before the snapshot existed.
 */
const renderAssetInput = (
  decimals: number | null | undefined,
  numberFormat: string = NUMBER_OPTIONS[0].value,
  metadata?: Record<string, unknown> | null,
  options: {
    assetDecimals?: number | null;
    hasDenominationChanged?: boolean;
  } = {}
) => {
  const form = buildForm();
  const field = form.$(fieldName);
  const assetDecimals =
    'assetDecimals' in options ? options.assetDecimals : decimals;
  const asset =
    metadata === undefined
      ? buildAsset(assetDecimals)
      : buildAsset(assetDecimals, metadata);
  // A fresh element each time. `AssetInput` is an `@observer`, so mobx-react
  // gives it a shallow prop comparison; re-rendering the identical element with
  // a mutated plain asset would be skipped. In the application the lookup is a
  // new function on every container render for the same reason.
  const tree = () => (
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
            decimals={rowDecimals}
            hasDenominationChanged={options.hasDenominationChanged === true}
          />
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </TestDecorator>
  );
  let rowDecimals = decimals;
  const { rerender } = render(tree());
  return {
    field,
    asset,
    input: screen.getByTestId(`assetInput:${uniqueId}`),
    label: () => screen.getByTestId(`assetUnitLabel:${uniqueId}`),
    setDecimals: (next: number | null | undefined) => {
      rowDecimals = next;
      rerender(tree());
    },
    rerender: () => rerender(tree()),
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

  describe('the unit label', () => {
    it('names whole ledger units when the decimal places are unknown', () => {
      const { label } = renderAssetInput(undefined);
      expect(label()).toHaveTextContent('Enter a whole number of TEST units');
      expect(label()).toHaveTextContent(
        'decimal places for this token are unknown'
      );
    });

    it('names the unit and the precision when the decimal places are known', () => {
      const { label } = renderAssetInput(6);
      expect(label()).toHaveTextContent(
        'Enter an amount in TEST, to 6 decimal places.'
      );
    });

    it('reads correctly for a token with zero decimal places', () => {
      const { label } = renderAssetInput(0);
      expect(label()).toHaveTextContent(
        'Enter an amount in TEST, to 0 decimal places.'
      );
    });

    it('falls back to the fingerprint when the issuer published no ticker', () => {
      const { label } = renderAssetInput(undefined, NUMBER_OPTIONS[0].value, {
        name: 'Test Coin',
        description: 'A test coin',
      });
      // The same ellipsised spelling the pill above the field uses.
      expect(label()).toHaveTextContent('asset1cvm\u2026kvpa');
    });

    it('never names the asset the minter called it', () => {
      // The asset name bytes decode to "Cointest", and no issuer published a
      // ticker. A label that reached for the decoded name would render a
      // minter-chosen string as the unit of account.
      const { label } = renderAssetInput(
        undefined,
        NUMBER_OPTIONS[0].value,
        null
      );
      expect(label()).not.toHaveTextContent('Cointest');
      expect(label()).toHaveTextContent('asset1cvm\u2026kvpa');
    });

    it('moves with the denomination it describes, in the same render', () => {
      const { input, field, label, setDecimals } = renderAssetInput(undefined);
      expect(label()).toHaveTextContent('Enter a whole number of TEST units');
      type(input, ['1', '1.', '1.5']);
      expect(field.value).toEqual('1');

      // The row's denomination moving, which after the snapshot is the prop and
      // not the asset.
      setDecimals(6);

      expect(label()).toHaveTextContent(
        'Enter an amount in TEST, to 6 decimal places.'
      );
      // Asserted together: a label that moved while the field kept refusing a
      // separator would be worse than no label at all.
      field.clear();
      type(input, ['2', '2.', '2.5']);
      expect(field.value).toEqual('2.500000');
    });
  });

  describe('the snapshotted denomination', () => {
    it('obeys the snapshot and not the asset it is handed', () => {
      // The asset says six decimal places. The row was added when they were
      // unknown, so the row is in raw units and stays there.
      const { field, input, label } = renderAssetInput(
        null,
        NUMBER_OPTIONS[0].value,
        undefined,
        { assetDecimals: 6 }
      );
      type(input, ['1500000', '1500000.', '1500000.5']);
      expect(field.value).toEqual('1500000');
      expect(formattedAmountToNaturalUnits(field.value)).toEqual('1500000');
      expect(label()).toHaveTextContent('Enter a whole number of TEST units');
    });

    it('renders the balance in the snapshotted denomination too', () => {
      // 900000000 raw units, drawn as raw units because the row is in raw units,
      // whatever the asset has since resolved to.
      const { input } = renderAssetInput(
        null,
        NUMBER_OPTIONS[0].value,
        undefined,
        {
          assetDecimals: 6,
        }
      );
      expect(input).toBeInTheDocument();
      expect(screen.getByText('900,000,000 TEST')).toBeInTheDocument();
    });

    it('says nothing about a denomination change on its own', () => {
      renderAssetInput(6);
      expect(
        screen.queryByTestId(`assetDenominationNotice:${uniqueId}`)
      ).not.toBeInTheDocument();
    });

    it('names the token in the notice when a change cleared the amount', () => {
      renderAssetInput(6, NUMBER_OPTIONS[0].value, undefined, {
        hasDenominationChanged: true,
      });
      const notice = screen.getByTestId(`assetDenominationNotice:${uniqueId}`);
      expect(notice).toHaveTextContent(
        'The decimal places published for TEST changed while you were entering an amount'
      );
      expect(notice).toHaveTextContent('Enter it again');
    });
  });
});
