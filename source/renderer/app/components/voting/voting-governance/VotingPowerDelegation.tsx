import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';

type Props = {
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
};

function VotingPowerDelegation({ intl, onExternalLinkClick }: Props) {
  return (
    <div className={styles.component}>
      <BorderedBox>
        <h1 className={styles.heading}>
          {intl.formatMessage(messages.heading)}
        </h1>
        <div className={styles.info}>
          <p>
            <FormattedHTMLMessageWithLink
              message={{
                ...messages.paragraph1,
                values: {
                  linkLabel: messages.learnMoreLinkLabel,
                  linkURL: messages.paragraph1LinkUrl,
                },
              }}
              onExternalLinkClick={onExternalLinkClick}
            />
          </p>
          <p>
            <FormattedHTMLMessage {...messages.paragraph2} />
          </p>
          <p>
            <FormattedHTMLMessageWithLink
              message={{
                ...messages.paragraph3,
                values: {
                  linkLabel: messages.learnMoreLinkLabel,
                  linkURL: messages.paragraph3LinkUrl,
                },
              }}
              onExternalLinkClick={onExternalLinkClick}
            />
          </p>
        </div>
      </BorderedBox>
    </div>
  );
}

export default observer(injectIntl(VotingPowerDelegation));
