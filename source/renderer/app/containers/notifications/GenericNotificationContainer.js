// @flow
import React, { Component, Children } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class GenericNotificationContainer extends Component<InjectedContainerProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, children } = this.props;

    const propsForChildren = {
      openNotification: actions.notifications.open,
      closeNotification: actions.notifications.closeActiveNotification,
    };

    return Children.map(children, child =>
      React.cloneElement(child, propsForChildren)
    );
  }
}
