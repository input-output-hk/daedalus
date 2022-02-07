// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import TermsOfUseText from '../../profile/terms-of-use/TermsOfUseText';
import styles from './TermsOfUseSettings.scss';

type Props = {
  localizedTermsOfUse: string,
  onOpenExternalLink: Function,
};

@observer
export default class TermsOfUseSettings extends Component<Props> {
  render() {
    const { localizedTermsOfUse, onOpenExternalLink } = this.props;
    return (
      <div className={styles.component}>
        <TermsOfUseText
          localizedTermsOfUse={localizedTermsOfUse}
          onOpenExternalLink={onOpenExternalLink}
        />
      </div>
    );
  }
}
