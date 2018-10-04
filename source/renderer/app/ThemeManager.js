import React, { Component } from 'react';
import updateCSSVariables from './utils/updateCSSVariables';

export default class ThemeManager extends Component {
  componentDidMount() {
    updateCSSVariables(this.props.variables);
  }

  componentDidUpdate(prevProps) {
    if (this.props.variables !== prevProps.variables) {
      updateCSSVariables(this.props.variables);
    }
  }

  render() {
    return <div>{this.props.children}</div>;
  }
}
