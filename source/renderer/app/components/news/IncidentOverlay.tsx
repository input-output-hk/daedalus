import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { get, camelCase } from 'lodash';
import ReactMarkdown from 'react-markdown';
import classnames from 'classnames';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import News from '../../domains/News';
import ButtonLink from '../widgets/ButtonLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './IncidentOverlay.scss' or its... Remove this comment to see the full error message
import styles from './IncidentOverlay.scss';

type Props = {
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  incident: News.News;
  onOpenExternalLink: (...args: Array<any>) => any;
  onProceedNewsAction: (...args: Array<any>) => any;
  currentDateFormat: string;
};

@observer
class IncidentOverlay extends Component<Props> {
  localizedDateFormat: 'MM/DD/YYYY';

  componentDidMount() {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type '"MM/DD/YY... Remove this comment to see the full error message
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  contentClickHandler(event: React.MouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);

    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }

  onProceedNewsAction = (event: React.MouseEvent<HTMLElement>) => {
    const { incident, onProceedNewsAction } = this.props;
    onProceedNewsAction(incident, event);
  };
  renderAction = (action: Record<string, any>) => {
    if (action && (action.url || action.event)) {
      return (
        <ButtonLink
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
    const componentClasses = classnames([
      styles.component,
      styles[camelCase(incident.color)],
    ]);
    return (
      <div className={componentClasses}>
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

export default IncidentOverlay;
