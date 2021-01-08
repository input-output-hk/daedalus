// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DisplaySettings.scss';

const messages = defineMessages({});

type Props = {
  currencySelected: string,
  currencyRate: number,
  currencyList: Array<any>,
};

@observer
export default class DisplaySettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    return <div>WALLETS</div>;
  }
}
