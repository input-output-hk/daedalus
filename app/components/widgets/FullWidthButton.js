import React, { Component, PropTypes } from 'react';
import Button from 'react-toolbox/lib/button/Button';
import styles from './FullWidthButton.scss';

export default class FullWidthButton extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    onMouseUp: PropTypes.func.isRequired
  };

  render() {
    return (
      <Button
        className={styles.button}
        label={this.props.label}
        onMouseUp={this.props.onMouseUp}
        primary
        fullWidth
      />
    );
  }

}
