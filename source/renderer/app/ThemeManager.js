/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';
import { map } from 'lodash';

export default class ThemeManager extends Component {
  componentDidMount() {
    this.updateCSSVariables(this.props.variables);
  }

  componentDidUpdate(prevProps) {
    if (this.props.variables !== prevProps.variables) {
      this.updateCSSVariables(this.props.variables);
    }
  }

  updateCSSVariables(variables) {
    const flattenedTheme = this.flattenTheme(variables);
    map(flattenedTheme, (value, prop) => {
      document.documentElement.style.setProperty(prop, value);
    });
  }

  flattenTheme(daedalusTheme) {
    return Object.values(daedalusTheme).reduce(
      (theme, componentVars) => ({ ...theme, ...componentVars }),
      {}
    );
  }

  render() {
    return <Fragment>{this.props.children}</Fragment>;
  }
}
