// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import News from '../../domains/News';
import ButtonLink from '../widgets/ButtonLink';
import styles from './IncidentOverlay.scss';

type Props = {
  incident: News.News,
  onOpenExternalLink: Function,
  onProceedNewsAction: Function,
  currentDateFormat: string,
};

@observer
export default class IncidentOverlay extends Component<Props> {
  localizedDateFormat: 'MM/DD/YYYY';

  UNSAFE_componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  contentClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }

  onProceedNewsAction = (event: SyntheticMouseEvent<HTMLElement>) => {
    const { incident, onProceedNewsAction } = this.props;
    onProceedNewsAction(incident, event);
  };

  renderAction = (action: Object) => {
    if (action && (action.url || action.event)) {
      return (
        <ButtonLink
          className={styles.actionBtn}
          onClick={this.onProceedNewsAction}
          skin={ButtonSkin}
          label={action.label}
          linkProps={{
            className: styles.externalLink,
            hasIconBefore: false,
            hasIconAfter: action.url && true,
          }}
        />
      );
    }
    return null;
  };

  render() {
    const { incident, currentDateFormat } = this.props;
    const { content, date, action, title } = incident;
    return (
      <div className={styles.component}>
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.date}>
          {moment(date).format(currentDateFormat)}
        </span>
        <div
          className={styles.content}
          role="presentation"
          onClick={this.contentClickHandler.bind(this)}
        >
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {this.renderAction(action)}
      </div>
    );
  }
}
