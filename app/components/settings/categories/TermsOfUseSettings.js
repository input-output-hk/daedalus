// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import TermsOfUseText from '../../profile/terms-of-use/TermsOfUseText';
import styles from './TermsOfUseSettings.scss';

type Props = {
  localizedTermsOfUse: string,
};

@observer
export default class TermsOfUseSettings extends Component<Props> {
  render() {
    const { localizedTermsOfUse } = this.props;
    return (
      <div className={styles.component}>
        <TermsOfUseText localizedTermsOfUse={localizedTermsOfUse} />
      </div>
    );
  }

}
