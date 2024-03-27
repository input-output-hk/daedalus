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
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const mobx_react_1 = require('mobx-react');
const react_2 = require('@testing-library/react');
require('@testing-library/jest-dom');
const local_storage_1 = require('../local-storage');
const context_1 = require('./context');
const ui_1 = require('./ui');
const analytics_1 = require('../../components/analytics');
const analytics_2 = require('../../analytics');
describe('Discreet Mode feature', () => {
  afterEach(react_2.cleanup);
  function TestDecorator({ children, testId }) {
    return react_1.default.createElement(
      react_intl_1.IntlProvider,
      { locale: 'en-US' },
      react_1.default.createElement(
        analytics_1.AnalyticsProvider,
        { tracker: analytics_2.noopAnalyticsTracker },
        react_1.default.createElement(
          local_storage_1.BrowserLocalStorageBridge,
          null,
          react_1.default.createElement(
            context_1.DiscreetModeFeatureProvider,
            null,
            react_1.default.createElement(
              react_1.default.Fragment,
              null,
              react_1.default.createElement(ui_1.DiscreetModeToggle, null),
              react_1.default.createElement(
                'div',
                { 'data-testid': testId },
                children
              )
            )
          )
        )
      )
    );
  }
  const discreetWalletAmountCases = [
    // testId, [amount, currency, withCurrency, long], [expected]
    [
      'custom ticker',
      [new bignumber_js_1.default(1), 'TEST', true, true],
      '1.000000 TEST',
    ],
    [
      'ada as ticker',
      [new bignumber_js_1.default(1), undefined, true, false],
      '1 ADA',
    ],
    ['no currency', [new bignumber_js_1.default(1), 'TEST', false, false], '1'],
  ];
  test.each(discreetWalletAmountCases)(
    '<DiscreetWalletAmount /> should replace wallet amount with %s by sensitive data symbol',
    async (testId, [amount, currency, withCurrency, long], expected) => {
      expect.assertions(2);
      (0, react_2.render)(
        // @ts-ignore ts-migrate(2322) FIXME: Type 'string | (string | boolean | BigNumber)[]' i... Remove this comment to see the full error message
        react_1.default.createElement(
          TestDecorator,
          { testId: testId },
          react_1.default.createElement(
            ui_1.DiscreetWalletAmount,
            // @ts-ignore ts-migrate(2322) FIXME: Type 'string | boolean | BigNumber' is not assigna... Remove this comment to see the full error message
            {
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | boolean | BigNumber' is not assigna... Remove this comment to see the full error message
              amount: amount,
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | boolean | BigNumber' is not assigna... Remove this comment to see the full error message
              currency: currency,
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | boolean | BigNumber' is not assigna... Remove this comment to see the full error message
              withCurrency: withCurrency,
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | boolean | BigNumber' is not assigna... Remove this comment to see the full error message
              long: long,
            }
          )
        )
      );
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      expect(react_2.screen.getByTestId(testId)).toHaveTextContent(expected);
      react_2.fireEvent.click(
        react_2.screen.getByRole('button', {
          name: /discreetModeToggle/i,
        })
      );
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      expect(react_2.screen.getByTestId(testId)).toHaveTextContent('***');
    }
  );
  const discreetTokenWalletAmountCases = [
    // testId, [amount, metada, decimals], [expected]
    ['without ticker', [new bignumber_js_1.default(1), null, 0], '1'],
    [
      'with ticker',
      [
        new bignumber_js_1.default(1),
        {
          ticker: 'TEST',
        },
        5,
      ],
      '0.00001 TEST',
    ],
  ];
  test.each(discreetTokenWalletAmountCases)(
    '<DiscreetTokenWalletAmount /> should replace token wallet amount with %s by sensitive data symbol',
    (testId, [amount, metadata, decimals], expected) => {
      expect.assertions(2);
      (0, react_2.render)(
        // @ts-ignore ts-migrate(2322) FIXME: Type 'string | (number | BigNumber | { ticker: str... Remove this comment to see the full error message
        react_1.default.createElement(
          TestDecorator,
          { testId: testId },
          react_1.default.createElement(
            ui_1.DiscreetTokenWalletAmount,
            // @ts-ignore ts-migrate(2322) FIXME: Type 'string | number | BigNumber | { ticker: stri... Remove this comment to see the full error message
            {
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | number | BigNumber | { ticker: stri... Remove this comment to see the full error message
              amount: amount,
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | number | BigNumber | { ticker: stri... Remove this comment to see the full error message
              metadata: metadata,
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | number | BigNumber | { ticker: stri... Remove this comment to see the full error message
              decimals: decimals,
            }
          )
        )
      );
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      expect(react_2.screen.getByTestId(testId)).toHaveTextContent(expected);
      react_2.fireEvent.click(
        react_2.screen.getByRole('button', {
          name: /discreetModeToggle/i,
        })
      );
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      expect(react_2.screen.getByTestId(testId)).toHaveTextContent('***');
    }
  );
  test('<DiscreetValue /> should replace value by sensitive data symbol', () => {
    expect.assertions(2);
    const value = 'test';
    const testId = 'discreet-value';
    (0, react_2.render)(
      react_1.default.createElement(
        TestDecorator,
        { testId: testId },
        react_1.default.createElement(ui_1.DiscreetValue, null, value)
      )
    );
    expect(react_2.screen.getByTestId(testId)).toHaveTextContent(value);
    react_2.fireEvent.click(
      react_2.screen.getByRole('button', {
        name: /discreetModeToggle/i,
      })
    );
    expect(react_2.screen.getByTestId(testId)).toHaveTextContent('***');
  });
  test('<withDiscreetMode /> high order component should replace value by sensitive data symbol', () => {
    expect.assertions(2);
    const value = 'test';
    const testId = 'discreet-value';
    const HighOrderComponentApi = (0, ui_1.withDiscreetMode)(
      (0, mobx_react_1.observer)(
        class View extends react_1.Component {
          render() {
            return this.props.discreetModeFeature.discreetValue({
              value,
            });
          }
        }
      )
    );
    (0, react_2.render)(
      react_1.default.createElement(
        TestDecorator,
        { testId: testId },
        react_1.default.createElement(HighOrderComponentApi, null)
      )
    );
    expect(react_2.screen.getByTestId(testId)).toHaveTextContent(value);
    react_2.fireEvent.click(
      react_2.screen.getByRole('button', {
        name: /discreetModeToggle/i,
      })
    );
    expect(react_2.screen.getByTestId(testId)).toHaveTextContent('***');
  });
});
//# sourceMappingURL=integration-tests.spec.js.map
