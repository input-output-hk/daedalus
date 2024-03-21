'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const react_2 = require('@testing-library/react');
const hooks_1 = require('./hooks');
const generateAssets = (n) => [...Array(n).keys()].map(tokenGenerator);
const setup = (useHook, args) => {
  const returnVal = {};
  function TestComponent() {
    Object.assign(returnVal, useHook(args));
    return null;
  }
  (0, react_2.render)(react_1.default.createElement(TestComponent, null));
  return returnVal;
};
const tokenGenerator = (id) => ({
  uniqueId: id,
});
describe('WalletTokenPicker hooks', () => {
  test('useCheckboxes initial state', () => {
    const assets = generateAssets(10);
    const hook = setup(hooks_1.useCheckboxes, {
      assets,
      currentAssets: assets,
      previouslyCheckedIds: [],
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'checkboxes' does not exist on type '{}'.
    expect(hook.checkboxes).toMatchObject({});
    // @ts-ignore ts-migrate(2339) FIXME: Property 'checkedIds' does not exist on type '{}'.
    expect(hook.checkedIds.length).toEqual(0);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'totalCheckedCount' does not exist on typ... Remove this comment to see the full error message
    expect(hook.totalCheckedCount).toBe(0);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'previouslyCheckedIdsSet' does not exist ... Remove this comment to see the full error message
    expect(hook.previouslyCheckedIdsSet.size).toEqual(0);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isMaxTotalCount' does not exist on type ... Remove this comment to see the full error message
    expect(hook.isMaxTotalCount).toBe(false);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isToggleAllDisabled' does not exist on t... Remove this comment to see the full error message
    expect(hook.isToggleAllDisabled).toEqual(false);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isClearAllMode' does not exist on type '... Remove this comment to see the full error message
    expect(hook.isClearAllMode).toEqual(false);
  });
  const toggleCases = [
    // testId, assets, sequence, expected [checkedCount, checkedIds]
    [
      'toggle sequence [1]',
      generateAssets(10),
      ['1'],
      [
        {
          1: true,
        },
        ['1'],
      ],
    ],
    [
      'toggle sequence [1, 2]',
      generateAssets(10),
      ['1', '2'],
      [
        {
          1: true,
          2: true,
        },
        ['1', '2'],
      ],
    ],
    [
      'toggle sequence [1, 1, 2, 3]',
      generateAssets(10),
      ['1', '1', '2', '3'],
      [
        {
          1: false,
          2: true,
          3: true,
        },
        ['2', '3'],
      ],
    ],
  ];
  test.each(toggleCases)(
    'useCheckboxes toggle checkbox for %s',
    (testId, assets, sequence, expected) => {
      const hook = setup(hooks_1.useCheckboxes, {
        assets,
        currentAssets: assets,
        previouslyCheckedIds: [],
      });
      for (let i = 0; i < sequence.length; i++) {
        (0, react_2.act)(() => {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleCheckbox' does not exist on type '... Remove this comment to see the full error message
          hook.toggleCheckbox(sequence[i]);
        });
      }
      // @ts-ignore ts-migrate(2339) FIXME: Property 'checkboxes' does not exist on type '{}'.
      expect(hook.checkboxes).toMatchObject(expected[0]);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'checkedIds' does not exist on type '{}'.
      expect(hook.checkedIds).toEqual(expected[1]);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'totalCheckedCount' does not exist on typ... Remove this comment to see the full error message
      expect(hook.totalCheckedCount).toBe(expected[1].length);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isMaxTotalCount' does not exist on type ... Remove this comment to see the full error message
      expect(hook.isMaxTotalCount).toBe(false);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isToggleAllDisabled' does not exist on t... Remove this comment to see the full error message
      expect(hook.isToggleAllDisabled).toEqual(false);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isClearAllMode' does not exist on type '... Remove this comment to see the full error message
      expect(hook.isClearAllMode).toEqual(false);
    }
  );
  const toggleAllFnCases = [
    // testId, [assets, previouslyCheckedIds], alreadyChecked, expected [checkedCount, checkedIds]
    [
      'less than 30 assets',
      [generateAssets(10), []],
      [],
      [10, [...Array(10).keys()].map(String)],
    ],
    [
      'more than 30 assets',
      [generateAssets(35), []],
      [],
      [30, [...Array(30).keys()].map(String)],
    ],
    [
      'more than 30 assets with some already checked',
      [generateAssets(35), []],
      ['32', '33'],
      [30, [...Array(28).keys(), '32', '33'].map(String)],
    ],
    [
      'more than 30 assets with some previously checked',
      [generateAssets(35), ['32', '33']],
      [],
      [30, [...Array(28).keys()].map(String)],
    ],
  ];
  test.each(toggleAllFnCases)(
    'useCheckboxes toggle toggleAllFn for %s',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(testId: string | string[] | (nu... Remove this comment to see the full error message
    (testId, [assets, previouslyCheckedIds], alreadyChecked, expected) => {
      const hook = setup(hooks_1.useCheckboxes, {
        assets,
        currentAssets: assets,
        previouslyCheckedIds,
      });
      for (let i = 0; i < alreadyChecked.length; i++) {
        (0, react_2.act)(() => {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleCheckbox' does not exist on type '... Remove this comment to see the full error message
          hook.toggleCheckbox(alreadyChecked[i]);
        });
      }
      (0, react_2.act)(() => {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleAllFn' does not exist on type '{}'... Remove this comment to see the full error message
        hook.toggleAllFn();
      });
      // @ts-ignore ts-migrate(2339) FIXME: Property 'totalCheckedCount' does not exist on typ... Remove this comment to see the full error message
      expect(hook.totalCheckedCount).toBe(expected[0]);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'checkedIds' does not exist on type '{}'.
      expect(hook.checkedIds).toEqual(expected[1]);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isMaxTotalCount' does not exist on type ... Remove this comment to see the full error message
      expect(hook.isMaxTotalCount).toBe(true);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isToggleAllDisabled' does not exist on t... Remove this comment to see the full error message
      expect(hook.isToggleAllDisabled).toEqual(false);
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isClearAllMode' does not exist on type '... Remove this comment to see the full error message
      expect(hook.isClearAllMode).toEqual(true);
    }
  );
});
//# sourceMappingURL=hooks.spec.js.map
