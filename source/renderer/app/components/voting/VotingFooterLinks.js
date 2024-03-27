'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.VotingFooterLinks = void 0;
const react_1 = __importDefault(require('react'));
const Link_1 = require('@react-polymorph/components/Link');
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const VotingFooterLinks_scss_1 = __importDefault(
  require('./VotingFooterLinks.scss')
);
const messages = (0, react_intl_1.defineMessages)({
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
exports.VotingFooterLinks = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(({ onClickExternalLink, intl }) => {
    const links = [
      {
        url: 'https://bit.ly/ProjCatNews',
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
    return react_1.default.createElement(
      'ul',
      { className: VotingFooterLinks_scss_1.default.component },
      links.map((link) =>
        react_1.default.createElement(
          'li',
          { key: link.url },
          react_1.default.createElement(Link_1.Link, {
            className: VotingFooterLinks_scss_1.default.link,
            label: link.label,
            onClick: (event) => onClickExternalLink(link.url, event),
          })
        )
      )
    );
  })
);
//# sourceMappingURL=VotingFooterLinks.js.map
