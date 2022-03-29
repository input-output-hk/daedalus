import React, { Component, ComponentClass } from 'react';
import { inject } from 'mobx-react';
import { InjectedProps } from '../types/injectedPropsType';

export function sendPageNavigationEventOnRender(
  title: string
): <P extends object>(WrappedComponent: ComponentClass<P>) => void {
  return <P extends object>(WrappedComponent: ComponentClass<P>) => {
    @inject('stores')
    class SendPageNavigationEventOnRenderWrapper extends Component<
      P & InjectedProps
    > {
      componentDidMount() {
        this.props.stores.analytics.analyticsClient.sendPageNavigationEvent(
          title
        );
      }

      render() {
        return <WrappedComponent {...this.props} />;
      }
    }

    return SendPageNavigationEventOnRenderWrapper;
  };
}
