import React from 'react';
import { render, act } from '@testing-library/react';
import { useCheckboxes } from './hooks';

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
    const hook = setup(useCheckboxes, {
      assets: [...Array(10).keys()].map(tokenGenerator),
      previousCheckedIds: [],
    });

    expect(hook.checkboxes).toMatchObject({});
    expect(hook.checkedCount).toBe(0);
    expect(hook.checkedIds.length).toEqual(0);
    expect(hook.disabledIdsSet.size).toEqual(0);
  });

  const toggleCases = [
    // testId, assets, sequence ,expected [checkedCount, checkedIds]
    [
      'toggle sequence [1]',
      [...Array(10).keys()].map(tokenGenerator),
      ['1'],
      [{ 1: true }, ['1']],
    ],
    [
      'toggle sequence [1, 2]',
      [...Array(10).keys()].map(tokenGenerator),
      ['1', '2'],
      [{ 1: true, 2: true }, ['1', '2']],
    ],
    [
      'toggle sequence [1, 1, 2, 3]',
      [...Array(10).keys()].map(tokenGenerator),
      ['1', '1', '2', '3'],
      [{ 1: false, 2: true, 3: true }, ['2', '3']],
    ],
  ];

  test.each(toggleCases)(
    'useCheckboxes toogle checkbox for %s',
    (testId, assets, sequence, expected) => {
      const hook = setup(useCheckboxes, {
        assets,
        previousCheckedIds: [],
      });

      for (let i = 0; i < sequence.length; i++) {
        act(() => {
          hook.toggleCheckbox(sequence[i]);
        });
      }
      expect(hook.checkboxes).toMatchObject(expected[0]);
      expect(hook.checkedIds).toEqual(expected[1]);
    }
  );

  const check30FirstCases = [
    // testId, [assets, previousCheckedIds], alreadyChecked, expected [checkedCount, checkedIds]
    [
      'less than 30 assets',
      [[...Array(10).keys()].map(tokenGenerator), []],
      [],
      [10, [...Array(10).keys()].map(String)],
    ],
    [
      'more than 30 assets',
      [[...Array(35).keys()].map(tokenGenerator), []],
      [],
      [30, [...Array(30).keys()].map(String)],
    ],
    [
      'more than 30 assets with some already checked',
      [[...Array(35).keys()].map(tokenGenerator), []],
      ['32', '33'],
      [30, [...Array(30).keys()].map(String)],
    ],
    [
      'more than 30 assets with some previously checked',
      [[...Array(35).keys()].map(tokenGenerator), ['32', '33']],
      [],
      [30, [...Array(28).keys()].map(String)],
    ],
  ];

  test.each(check30FirstCases)(
    'useCheckboxes toogle check30First for %s',
    (testId, [assets, previousCheckedIds], alreadyChecked, expected) => {
      const hook = setup(useCheckboxes, {
        assets,
        previousCheckedIds,
      });

      for (let i = 0; i < alreadyChecked.length; i++) {
        act(() => {
          hook.toggleCheckbox(alreadyChecked[i]);
        });
      }

      act(() => {
        hook.check30First();
      });

      expect(hook.checkedCount).toBe(expected[0]);
      expect(hook.checkedIds).toEqual(expected[1]);
    }
  );
});
