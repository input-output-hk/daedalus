// @flow
import React, { Component } from 'react';

class BackToTopButton extends Component {
  state = {
    isActive: false,
  };

  render() {
    return <div>Hello {this.state.world}</div>;
  }
}

export default BackToTopButton;
