// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import News from '../../domains/News';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import ButtonLink from '../widgets/ButtonLink';
import styles from './AlertsOverlay.scss';

type State = {
  showOverlay: boolean,
};

type Props = {
  alerts: Array<News.News>,
  onCloseOpenAlert: Function,
  onMarkNewsAsRead: Function,
  onOpenExternalLink: Function,
  onProceedNewsAction: Function,
  allAlertsCount: number,
  hideCounter?: boolean,
  currentDateFormat: string,
};

@observer
export default class AlertsOverlay extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      showOverlay: true,
    };
  }

  localizedDateFormat: 'MM/DD/YYYY';

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  contentClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }

  onClose = () => {
    const { alerts, onMarkNewsAsRead, onCloseOpenAlert } = this.props;
    if (alerts.length <= 1) {
      onMarkNewsAsRead([alerts[0].id]);
      onCloseOpenAlert();
      this.setState({ showOverlay: false });
      return;
    }
    onMarkNewsAsRead([alerts[0].id]);
  };

  onProceedNewsAction = (event: SyntheticMouseEvent<HTMLElement>) => {
    const { onProceedNewsAction, alerts } = this.props;
    onProceedNewsAction(alerts[0], event);
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

  renderCounter = (alerts: Array<News.News>) => {
    const { allAlertsCount, hideCounter } = this.props;
    if (!hideCounter) {
      return (
        <span className={styles.counter}>
          {allAlertsCount - alerts.length + 1} / {allAlertsCount}
        </span>
      );
    }
    return null;
  };

  render() {
    const { showOverlay } = this.state;
    const { alerts, currentDateFormat } = this.props;
    const [alert] = alerts;
    const { content, date, action, title } = alert;
    return (
      showOverlay && (
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={this.onClose}
          />
          {this.renderCounter(alerts)}
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
      )
    );
  }
}
