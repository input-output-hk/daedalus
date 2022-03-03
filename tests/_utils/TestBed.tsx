import React, { ReactElement } from 'react';
import { IntlProvider } from 'react-intl';
import { Provider as MobxProvider } from 'mobx-react';
import { render } from "@testing-library/react";

import { BrowserLocalStorageBridge, DiscreetModeFeatureProvider } from '../../source/renderer/app/features';
import translations from '../../source/renderer/app/i18n/locales/en-US.json';
import StoryDecorator from '../../storybook/stories/_support/StoryDecorator';

const TestBed = ({ children }: { children: React.ReactNode }) => (
    <StoryDecorator>
        <MobxProvider>
            <IntlProvider locale="en-US" messages={translations}>
                <BrowserLocalStorageBridge>
                    <DiscreetModeFeatureProvider>
                        {children}
                    </DiscreetModeFeatureProvider>
                </BrowserLocalStorageBridge>
            </IntlProvider>
        </MobxProvider>
    </StoryDecorator>
);

const createTestBed = (component: ReactElement) => {
    return render(<TestBed>{component}</TestBed>);
}

export default createTestBed;