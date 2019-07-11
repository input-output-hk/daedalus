import {
  createReactPolymorphTheme,
  createDaedalusComponentsTheme,
} from './createTheme';
import { checkCreateTheme } from './checkCreateTheme';
import { CREATE_THEME_MOCK_PARAMS } from './constants';

const createThemeObj = {
  ...createReactPolymorphTheme(CREATE_THEME_MOCK_PARAMS),
  ...createDaedalusComponentsTheme(CREATE_THEME_MOCK_PARAMS),
};

checkCreateTheme(createThemeObj);
