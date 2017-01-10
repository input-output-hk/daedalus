// @flow
import React from 'react';
import { Dropdown } from 'react-toolbox/lib/dropdown/Dropdown';
import { themr } from 'react-css-themr';
import { DROPDOWN } from 'react-toolbox/lib/identifiers.js';

/**
 * Simple extension of the react-toolbox dropdown because it provides
 * no prop to explicitely set the opening direction :(
 */
class Dropup extends Dropdown {
  // Override the open method to always set { up: true }
  open = () => {
    if (this.inputNode) this.inputNode.blur();
    this.setState({ active: true, up: true });
  };
}

export default themr(DROPDOWN)(Dropup);
