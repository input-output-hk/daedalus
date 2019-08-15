// @flow
import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import startCase from 'lodash/startCase';
import StoryLayout from '../_support/StoryLayout';
import StoryProvider from '../_support/StoryProvider';
import StoryDecorator from '../_support/StoryDecorator';
import SettingsLayout from '../../../source/renderer/app/components/settings/SettingsLayout';
import SettingsMenu from '../../../source/renderer/app/components/settings/menu/SettingsMenu';

// import Layout from '../MainLayout';
// import { buildRoute } from '../../utils/routing';

/* eslint-disable react/display-name  */
export default (story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  const getItemFromContext = () => {
    // console.log('context.story', context.story);
    let { story } = context;
    story = story.toLocaleLowerCase();
    // console.log('`settings/${story}`', `settings/${story}`);
    return `settings/${story}`;
    // .replace('Wallet UTXO distribution', 'utxo')
  };

  const menu = (
    <SettingsMenu
      onItemClick={route => {
        // console.log('route', route);
      }}
      isActiveItem={item => {
        // console.log('getItemFromContext()', getItemFromContext());
        // console.log('item', item);
        return item === getItemFromContext();
      }}
    />
  );

  return (
    <StoryDecorator>
      <StoryProvider>
        <StoryLayout
          activeSidebarCategory="/settings"
          storyName={context.story}
        >
          <SettingsLayout menu={menu}>{storyWithKnobs}</SettingsLayout>
        </StoryLayout>
      </StoryProvider>
    </StoryDecorator>
  );
};

/*
export default class Settings extends Component<InjectedContainerProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  isActivePage = (route: string) => {
    const { location } = this.props.stores.router;
    if (location) {
      return location.pathname === buildRoute(route);
    }
    return false;
  };

  render() {
    const { actions, children } = this.props;
    const menu = (
      <SettingsMenu
        onItemClick={route => actions.router.goToRoute.trigger({ route })}
        isActiveItem={this.isActivePage}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu}>{children}</SettingsLayout>
      </Layout>
    );
  }
}
*/
