import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../themes/daedalus';
import { themeOverrides } from '../../themes/overrides';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import SidebarCategory from './SidebarCategory';

const renderCategory = (hasNotification?: boolean) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <SidebarCategory
          category={CATEGORIES_BY_NAME.GOVERNANCE}
          isActive={false}
          onClick={jest.fn()}
          hasNotification={hasNotification}
        />
      </IntlProvider>
    </ThemeProvider>
  );

describe('SidebarCategory', () => {
  afterEach(cleanup);

  it('draws no marker by default', () => {
    const { container } = renderCategory();
    expect(container.querySelector('.notificationDot')).toBeNull();
  });

  it('draws a marker when the category has a notification', () => {
    const { container } = renderCategory(true);
    expect(container.querySelector('.notificationDot')).not.toBeNull();
  });
});
