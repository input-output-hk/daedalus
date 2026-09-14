import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import vjf from 'mobx-react-form/lib/validators/VJF';
import ReactToolboxMobxForm from '../../../../source/renderer/app/utils/ReactToolboxMobxForm';
import { AssetDenominations } from '../../../../source/renderer/app/components/wallet/send-form/assetDenominations';
import { formattedAmountToNaturalUnits } from '../../../../source/renderer/app/utils/formatters';

const UNIQUE_ID =
  'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544';
const FIELD_NAME = `asset_${UNIQUE_ID}`;

type AssetFormFields = {
  [assetField: string]: string;
};

/**
 * A real `mobx-react-form` field, the same class the send form builds its asset
 * rows from, so "the field is cleared" is the field actually being cleared.
 */
const buildField = () => {
  const form = new ReactToolboxMobxForm<AssetFormFields>(
    {
      fields: {
        [FIELD_NAME]: {
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
  return form.$(FIELD_NAME);
};

/**
 * Opening a row. The snapshot is taken before the field can be typed into,
 * exactly as `WalletSendForm.addAssetFields` does it.
 */
const openRow = function (this: any, decimals: number | null) {
  this.context.denominations = new AssetDenominations();
  this.context.field = buildField();
  this.context.denominations.snapshot(UNIQUE_ID, decimals);
  this.context.reconciliation = { cleared: [], adopted: [] };
};

Given(
  'a send form row for a token whose decimal places are unknown',
  function () {
    openRow.call(this, null);
  }
);

Given(
  'a send form row for a token with {int} decimal places',
  function (decimals: number) {
    openRow.call(this, decimals);
  }
);

Given('I have entered {string} in that row', function (amount: string) {
  this.context.field.onChange(amount);
  expect(this.context.field.value).to.equal(amount);
});

When(
  'the cache resolves that token to {int} decimal places',
  function (decimals: number) {
    this.context.reconciliation = this.context.denominations.reconcile([
      {
        uniqueId: UNIQUE_ID,
        currentDecimals: decimals,
        field: this.context.field,
      },
    ]);
  }
);

Then("that row's amount field is empty", function () {
  const { value } = this.context.field;
  expect(value === null || value === undefined || value === '').to.equal(true);
});

Then('that row reports a denomination change', function () {
  expect(this.context.reconciliation.cleared).to.deep.equal([UNIQUE_ID]);
});

Then('that row reports no denomination change', function () {
  expect(this.context.reconciliation.cleared).to.deep.equal([]);
});

Then(
  'that row is denominated in {int} decimal places',
  function (decimals: number) {
    expect(this.context.denominations.decimalsFor(UNIQUE_ID)).to.equal(
      decimals
    );
  }
);

// The amount the send form would submit for this row is the field's display
// string with its separators deleted, which is what `selectedAssetsAmounts`
// computes.
Then(
  'the form would submit {string} natural units for that row',
  function (amount: string) {
    expect(formattedAmountToNaturalUnits(this.context.field.value)).to.equal(
      amount
    );
  }
);

Then(
  'the form would not submit {string} natural units for that row',
  function (amount: string) {
    expect(
      formattedAmountToNaturalUnits(this.context.field.value)
    ).to.not.equal(amount);
  }
);
