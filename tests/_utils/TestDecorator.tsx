import React from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { IntlProvider } from 'react-intl';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { daedalusTheme } from '../../source/renderer/app/themes/daedalus';
import { themeOverrides } from '../../source/renderer/app/themes/overrides';
import translations from '../../source/renderer/app/i18n/locales/en-US.json';

type Props = {
  children: Node;
};
export function TestDecorator({ children }: Props) {
  return (
    <IntlProvider locale="en-US" messages={translations}>
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        {children}
      </ThemeProvider>
    </IntlProvider>
  );
}
