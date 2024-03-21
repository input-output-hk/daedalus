'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.CREATE_THEME_OBJ = exports.CREATE_THEME_PARAMS = exports.THEME_LOGGING_COLORS = exports.DEFAULT_FONTS = void 0;
const createShades_1 = require('./createShades');
const createTheme_1 = require('./createTheme');
exports.DEFAULT_FONTS = {
  black: 'NotoSans-Black, NotoSansCJKjp-Black',
  bold: 'NotoSans-Bold, NotoSansCJKjp-Bold',
  heavy: 'NotoSans-ExtraBold, NotoSansCJKjp-Black',
  light: 'NotoSans-Light, NotoSansCJKjp-Light',
  medium: 'NotoSans-Medium, NotoSansCJKjp-Medium',
  mono: 'SFMono-Light',
  regular: 'NotoSans-Regular, NotoSansCJKjp-Regular',
  semibold: 'NotoSans-SemiBold, NotoSansCJKjp-Medium',
  thin: 'NotoSans-Thin, NotoSansCJKjp-Thin',
  ultralight: 'NotoSans-ExtraLight, NotoSansCJKjp-Thin',
};
exports.THEME_LOGGING_COLORS = {
  'cardano.ts': '#2cbb69',
  'dark-blue.ts': '#2874a6',
  'dark-cardano.ts': '#1fc1c3',
  'flight-candidate.ts': '#ffb923',
  'incentivized-testnet.ts': '#f69ab2',
  'light-blue.ts': '#33c4ff',
  'shelley-testnet.ts': '#898ee6',
  'white.ts': '#29b595',
  'yellow.ts': '#fdcd68',
};
const CREATE_THEME_MOCK_PARAMS = {
  colors: {
    border: '#eee',
    error: (0, createShades_1.createErrorShades)('#eee'),
    focus: '#eee',
    background: {
      primary: (0, createShades_1.createBackgroundShades)('#eee'),
      secondary: (0, createShades_1.createBackgroundShades)('#eee'),
    },
    text: {
      primary: '#eee',
      secondary: '#eee',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_CARDANO_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#efefef',
      secondary: '#2cbb69',
    },
    border: '#c6cdd6',
    error: '#ea4c5b',
    focus: '#5e6066',
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_DARK_BLUE_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#263345',
      secondary: '#536370',
    },
    border: 'rgba(102, 122, 138, 0.3)',
    error: '#ea4c5b',
    focus: '#667a8a',
    text: {
      primary: '#e9f4fe',
      secondary: '#fafbfc',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_DARK_CARDANO_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#121326',
      secondary: '#36374d',
    },
    border: 'rgba(255, 255, 255, 0.2)',
    error: '#ea4c5b',
    focus: 'rgba(255, 255, 255, 0.4)',
    text: {
      primary: '#ffffff',
      secondary: '#000000',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_FLIGHT_CANDIDATE_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#121326',
      secondary: '#36374d',
    },
    border: 'rgba(255, 255, 255, 0.2)',
    error: '#ea4c5b',
    focus: 'rgba(255, 255, 255, 0.4)',
    text: {
      primary: '#ffffff',
      secondary: '#000000',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_INCENTIVIZED_TESTNET_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#121326',
      secondary: '#36374d',
    },
    border: 'rgba(255, 255, 255, 0.2)',
    error: '#eb4a22',
    focus: 'rgba(255, 255, 255, 0.4)',
    text: {
      primary: '#ffffff',
      secondary: '#000000',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_LIGHT_BLUE_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#ebeff2',
      secondary: '#2f496e',
    },
    border: '#c6cdd6',
    error: '#ea4c5b',
    focus: '#5e6066',
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_SHELLEY_TESTNET_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#121326',
      secondary: '#36374d',
    },
    border: 'rgba(255, 255, 255, 0.2)',
    error: '#ea4c5b',
    focus: 'rgba(255, 255, 255, 0.4)',
    text: {
      primary: '#ffffff',
      secondary: '#000000',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_WHITE_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#f9f9f9',
      secondary: '#ffffff',
    },
    border: 'rgba(45, 45, 45, 0.1)',
    error: '#ea4c5b',
    focus: '#2d2d2d',
    text: {
      primary: '#2d2d2d',
      secondary: '#fafbfc',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
const CREATE_YELLOW_THEME_PARAMS = {
  colors: {
    background: {
      primary: '#f8f3ed',
      secondary: '#fdcd68',
    },
    border: '#e1dac6',
    error: '#ea4c5b',
    focus: '#2d2d2d',
    text: {
      primary: '#2d2d2d',
      secondary: '#fafbfc',
    },
  },
  fonts: exports.DEFAULT_FONTS,
};
exports.CREATE_THEME_PARAMS = [
  ['cardano.ts', CREATE_CARDANO_THEME_PARAMS],
  ['dark-blue.ts', CREATE_DARK_BLUE_THEME_PARAMS],
  ['dark-cardano.ts', CREATE_DARK_CARDANO_THEME_PARAMS],
  ['flight-candidate.ts', CREATE_FLIGHT_CANDIDATE_THEME_PARAMS],
  ['incentivized-testnet.ts', CREATE_INCENTIVIZED_TESTNET_THEME_PARAMS],
  ['light-blue.ts', CREATE_LIGHT_BLUE_THEME_PARAMS],
  ['shelley-testnet.ts', CREATE_SHELLEY_TESTNET_THEME_PARAMS],
  ['white.ts', CREATE_WHITE_THEME_PARAMS],
  ['yellow.ts', CREATE_YELLOW_THEME_PARAMS],
];
exports.CREATE_THEME_OBJ = {
  ...(0, createTheme_1.createReactPolymorphTheme)(CREATE_THEME_MOCK_PARAMS),
  ...(0, createTheme_1.createDaedalusComponentsTheme)(CREATE_THEME_MOCK_PARAMS),
};
//# sourceMappingURL=constants.js.map
