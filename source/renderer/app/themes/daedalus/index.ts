import { isEmpty } from 'lodash';
import { CARDANO_THEME_OUTPUT } from './cardano';
import { DARK_BLUE_THEME_OUTPUT } from './dark-blue';
import { DARK_CARDANO_THEME_OUTPUT } from './dark-cardano';
import { FLIGHT_CANDIDATE_THEME_OUTPUT } from './flight-candidate';
import { INCENTIVIZED_TESTNET_THEME_OUTPUT } from './incentivized-testnet';
import { LIGHT_BLUE_THEME_OUTPUT } from './light-blue';
import { SHELLEY_TESTNET_THEME_OUTPUT } from './shelley-testnet';
import { WHITE_THEME_OUTPUT } from './white';
import { YELLOW_THEME_OUTPUT } from './yellow';

export const EXISTING_THEME_OUTPUTS = [
  ['cardano.ts', CARDANO_THEME_OUTPUT],
  ['dark-blue.ts', DARK_BLUE_THEME_OUTPUT],
  ['dark-cardano.ts', DARK_CARDANO_THEME_OUTPUT],
  ['flight-candidate.ts', FLIGHT_CANDIDATE_THEME_OUTPUT],
  ['incentivized-testnet.ts', INCENTIVIZED_TESTNET_THEME_OUTPUT],
  ['light-blue.ts', LIGHT_BLUE_THEME_OUTPUT],
  ['shelley-testnet.ts', SHELLEY_TESTNET_THEME_OUTPUT],
  ['white.ts', WHITE_THEME_OUTPUT],
  ['yellow.ts', YELLOW_THEME_OUTPUT],
];
export const EXISTING_THEME_OUTPUTS_OBJ = EXISTING_THEME_OUTPUTS.reduce(
  (outputsObj, theme) => {
    const [themeName, themeOutput] = theme;

    if (themeName && !isEmpty(themeOutput)) {
      // @ts-ignore ts-migrate(2538) FIXME: Type '{ aboutWindow: { '--theme-about-window-backg... Remove this comment to see the full error message
      outputsObj[themeName] = themeOutput;
    }

    return outputsObj;
  },
  {}
);
