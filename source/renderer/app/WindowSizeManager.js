/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';

export default class WindowSizeManager extends Component {
  componentDidMount() {
    this.updateMinScreenHeight(this.props.minScreenHeight);
  }

  componentDidUpdate(prevProps) {
    if (this.props.minScreenHeight !== prevProps.minScreenHeight) {
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
