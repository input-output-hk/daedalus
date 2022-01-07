import React from 'react';
import { render, act } from '@testing-library/react';
import { useCheckboxes } from './hooks';

const generateAssets = (n) => [...Array(n).keys()].map(tokenGenerator);

const setup = (useHook, args) => {
  const returnVal = {};
  function TestComponent() {
    Object.assign(returnVal, useHook(args));
    return null;
  }
  render(<TestComponent />);
  return returnVal;
};

const tokenGenerator = (id) => ({
  uniqueId: id,
});

describe('WalletTokenPicker hooks', () => {
  test('useCheckboxes initial state', () => {
    const assets = generateAssets(10);
    const hook = setup(useCheckboxes, {
      assets,
      currentAssets: assets,
      previousCheckedIds: [],
    });

    expect(hook.checkboxes).toMatchObject({});
    expect(hook.checkedIds.length).toEqual(0);
    expect(hook.totalCheckedCount).toBe(0);
    expect(hook.disabledIdsSet.size).toEqual(0);
    expect(hook.isMaxTotalCount).toBe(false);
    expect(hook.isToggleAllDisabled).toEqual(false);
    expect(hook.isClearAllMode).toEqual(false);
  });

  const toggleCases = [
    // testId, assets, sequence, expected [checkedCount, checkedIds]
    ['toggle sequence [1]', generateAssets(10), ['1'], [{ 1: true }, ['1']]],
    [
      'toggle sequence [1, 2]',
      generateAssets(10),
      ['1', '2'],
      [{ 1: true, 2: true }, ['1', '2']],
    ],
    [
      'toggle sequence [1, 1, 2, 3]',
      generateAssets(10),
      ['1', '1', '2', '3'],
      [{ 1: false, 2: true, 3: true }, ['2', '3']],
    ],
  ];

  test.each(toggleCases)(
    'useCheckboxes toogle checkbox for %s',
    (testId, assets, sequence, expected) => {
      const hook = setup(useCheckboxes, {
        assets,
        currentAssets: assets,
        previousCheckedIds: [],
      });

      for (let i = 0; i < sequence.length; i++) {
        act(() => {
          hook.toggleCheckbox(sequence[i]);
        });
      }

      expect(hook.checkboxes).toMatchObject(expected[0]);
      expect(hook.checkedIds).toEqual(expected[1]);
      expect(hook.totalCheckedCount).toBe(expected[1].length);
      expect(hook.isMaxTotalCount).toBe(false);
      expect(hook.isToggleAllDisabled).toEqual(false);
      expect(hook.isClearAllMode).toEqual(false);
    }
  );

  const toogleAllFnCases = [
    // testId, [assets, previousCheckedIds], alreadyChecked, expected [checkedCount, checkedIds]
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

  test.each(toogleAllFnCases)(
    'useCheckboxes toogle toogleAllFn for %s',
    (testId, [assets, previousCheckedIds], alreadyChecked, expected) => {
      const hook = setup(useCheckboxes, {
        assets,
        currentAssets: assets,
        previousCheckedIds,
      });

      for (let i = 0; i < alreadyChecked.length; i++) {
        act(() => {
          hook.toggleCheckbox(alreadyChecked[i]);
        });
      }

      act(() => {
        hook.toogleAllFn();
      });

      expect(hook.totalCheckedCount).toBe(expected[0]);
      expect(hook.checkedIds).toEqual(expected[1]);
      expect(hook.isMaxTotalCount).toBe(true);
      expect(hook.isToggleAllDisabled).toEqual(false);
      expect(hook.isClearAllMode).toEqual(true);
    }
  );
});
