// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import About from '../../components/static/About';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class AboutDialog extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { actions } = this.props;
    const { app } = this.props.stores;
    const { openExternalLink, environment } = app;
    const { apiVersion, build, os, version } = environment;
    const { closeAboutDialog } = actions.app;

    return (
        <About
          apiVersion={apiVersion}
          build={build}
          onOpenExternalLink={openExternalLink}
          os={os}
          version={version}
          onClose={closeAboutDialog.trigger}
        />
    );
  }
}
