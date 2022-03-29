import React, {
  Component,
  ComponentClass,
  ComponentType,
  ReactNode,
} from 'react';
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
          title,
          window.location.toString()
        );
      }

      render() {
        return (
          <React.Fragment>
            <WrappedComponent {...this.props} />
          </React.Fragment>
        );
      }
    }

    return SendPageNavigationEventOnRenderWrapper;
  };
}
