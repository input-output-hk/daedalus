import React from 'react';
import { Link } from 'react-polymorph/lib/components/Link';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingFooterLinks.scss' or i... Remove this comment to see the full error message
import styles from './VotingFooterLinks.scss';

type FooterLink = {
  url: string;
  label: string;
};
type Props = {
  onClickExternalLink: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
};
const messages = defineMessages({
  newsletter: {
    id: 'voting.catalystFooterLinks.newsletter',
    defaultMessage: 'Newsletter',
    description: '"Newsletter" link for Project Catalyst footer',
  },
  announcements: {
    id: 'voting.catalystFooterLinks.announcements',
    defaultMessage: 'Announcements Channel',
    description: '"Announcements Channel" link for Project Catalyst footer',
  },
  community: {
    id: 'voting.catalystFooterLinks.community',
    defaultMessage: 'Community Chat',
    description: '"Community Chat" link for Project Catalyst footer',
  },
  projects: {
    id: 'voting.catalystFooterLinks.projects',
    defaultMessage: 'Projects',
    description: '"Projects" link for Project Catalyst footer',
  },
});
export const VotingFooterLinks = injectIntl(
  observer(({ onClickExternalLink, intl }: Props) => {
    const links: FooterLink[] = [
      {
        url: 'https://bit.ly/3dSZJvx',
        label: intl.formatMessage(messages.newsletter),
      },
      {
        url: 'https://t.me/cardanocatalyst',
        label: intl.formatMessage(messages.announcements),
      },
      {
        url: 'https://t.me/ProjectCatalystChat',
        label: intl.formatMessage(messages.community),
      },
      {
        url: 'https://cardano.ideascale.com/',
        label: intl.formatMessage(messages.projects),
      },
    ];
    return (
      <ul className={styles.component}>
        {links.map((link) => (
          <li key={link.url}>
            <Link
              className={styles.link}
              label={link.label}
              onClick={(event) => onClickExternalLink(link.url, event)}
            />
          </li>
        ))}
      </ul>
    );
  })
);
