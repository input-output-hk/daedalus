// @flow
import { has, pickBy, find, isEqual } from 'lodash';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';

// Checks for properties/CSS vars on existing themes that don't exist on createThemeObj
export const checkCreateTheme = (createThemeObj: Object) => {
  const missingCardanoDefs = [
    ...findMissingDefinitions(CARDANO_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(CARDANO_THEME_CONFIG, createThemeObj),
  ];
  const missingDarkBlueDefs = [
    ...findMissingDefinitions(DARK_BLUE_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(DARK_BLUE_THEME_CONFIG, createThemeObj),
  ];
  const missingLightBlueDefs = [
    ...findMissingDefinitions(LIGHT_BLUE_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(LIGHT_BLUE_THEME_CONFIG, createThemeObj),
  ];

  console.log(`
    createTheme is missing the following theme definitions that exist in the Cardano theme:
    ${JSON.stringify(Object.fromEntries(missingCardanoDefs))}
  `);

  console.log(`
    createTheme is missing the following theme definitions that exist in the Dark-Blue theme:
    ${JSON.stringify(Object.fromEntries(missingDarkBlueDefs))}
  `);

  console.log(`
    createTheme is missing the following theme definitions that exist in the Light-Blue theme:
    ${JSON.stringify(Object.fromEntries(missingLightBlueDefs))}
  `);
};

const findMissingDefinitions = (
  basis: Object,
  target: Object
): Array<[string, Object]> => {
  const targetEntries = Object.entries(target);
  const basisEntries = Object.entries(basis);
  const targetMissingDefs = basisEntries.filter(
    basisEntry =>
      targetEntries.findIndex(
        targetEntry => basisEntry[0] !== targetEntry[0]
      ) >= 0
  );
  return targetMissingDefs;
};

const findMissingCSSVars = (
  basis: Object,
  target: Object
): Array<[string, Object]> => {
  const targetEntries = Object.entries(target);
  const basisEntries = Object.entries(basis);
  const missingVarsOnTarget = basisEntries
    .filter(basisEntry =>
      find(
        targetEntries,
        targetEntry =>
          basisEntry[0] === targetEntry[0] &&
          !isEqual(basisEntry[1], targetEntry[1])
      )
    )
    .reduce(
      (accumulator, entry) => [
        ...accumulator,
        [
          entry[0],
          pickBy(entry[1], (value, key) => !has(target[entry[0]], key)),
        ],
      ],
      []
    );

  return missingVarsOnTarget;
};
