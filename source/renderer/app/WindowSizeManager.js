import React, { Component, Fragment } from 'react';

export default class WindowSizeManager extends Component {
  componentDidMount() {
    // eslint-disable-next-line react/prop-types
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
    // eslint-disable-next-line react/prop-types
    return <Fragment>{this.props.children}</Fragment>;
  }
}
