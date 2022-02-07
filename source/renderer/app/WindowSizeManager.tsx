/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';

export default class WindowSizeManager extends Component {
  componentDidMount() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'minScreenHeight' does not exist on type ... Remove this comment to see the full error message
    this.updateMinScreenHeight(this.props.minScreenHeight);
  }

  componentDidUpdate(prevProps) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'minScreenHeight' does not exist on type ... Remove this comment to see the full error message
    if (this.props.minScreenHeight !== prevProps.minScreenHeight) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'minScreenHeight' does not exist on type ... Remove this comment to see the full error message
      this.updateMinScreenHeight(this.props.minScreenHeight);
    }
  }

  updateMinScreenHeight(minScreenHeight) {
    const rootWindowEl = document.getElementById('root');

    if (rootWindowEl) {
      rootWindowEl.style.minHeight = minScreenHeight;
    }
  }

  render() {
    return <Fragment>{this.props.children}</Fragment>;
  }
}
