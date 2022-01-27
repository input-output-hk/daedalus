import React, { Component } from 'react';
import { observer } from 'mobx-react';
import TermsOfUseText from '../../profile/terms-of-use/TermsOfUseText';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TermsOfUseSettings.scss' or ... Remove this comment to see the full error message
import styles from './TermsOfUseSettings.scss';

type Props = {
  localizedTermsOfUse: string;
  onOpenExternalLink: (...args: Array<any>) => any;
};

@observer
class TermsOfUseSettings extends Component<Props> {
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

export default TermsOfUseSettings;
