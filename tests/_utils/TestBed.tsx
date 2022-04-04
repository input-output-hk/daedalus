import React, { ReactElement } from 'react';
import { IntlProvider } from 'react-intl';
import { render } from '@testing-library/react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';

import {
  BrowserLocalStorageBridge,
  DiscreetModeFeatureProvider,
} from '../../source/renderer/app/features';
import translations from '../../source/renderer/app/i18n/locales/en-US.json';
import { daedalusTheme } from '../../source/renderer/app/themes/daedalus';
import { themeOverrides } from '../../source/renderer/app/themes/overrides';

const TestBed = ({ children }: { children: ReactElement }) => (
  <ThemeProvider
    theme={daedalusTheme}
    skins={SimpleSkins}
    variables={SimpleDefaults}
    themeOverrides={themeOverrides}
  >
    <IntlProvider locale="en-US" messages={translations}>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>{children}</DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </IntlProvider>
  </ThemeProvider>
);

const createTestBed = (component: ReactElement) => {
  return render(<TestBed>{component}</TestBed>);
};

export default createTestBed;
