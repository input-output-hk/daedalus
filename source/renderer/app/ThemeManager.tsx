/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';
import { map } from 'lodash';

export default class ThemeManager extends Component {
  componentDidMount() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
    this.updateCSSVariables(this.props.variables);
  }

  componentDidUpdate(prevProps) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
    if (this.props.variables !== prevProps.variables) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'variables' does not exist on type 'Reado... Remove this comment to see the full error message
      this.updateCSSVariables(this.props.variables);
    }
  }

  updateCSSVariables(variables) {
    const flattenedTheme = this.flattenTheme(variables);
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    map(flattenedTheme, (value, prop) => {
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
      document.documentElement.style.setProperty(prop, value);
    });
  }

  flattenTheme(daedalusTheme) {
    return Object.values(daedalusTheme).reduce(
      // @ts-ignore ts-migrate(2698) FIXME: Spread types may only be created from object types... Remove this comment to see the full error message
      (theme, componentVars) => ({ ...theme, ...componentVars }),
      {}
    );
  }

  render() {
    return <Fragment>{this.props.children}</Fragment>;
  }
}
