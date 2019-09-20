// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import SVGInline from 'react-svg-inline';
import News from '../../domains/News';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './AlertsOverlay.scss';

type State = {
  showOverlay: boolean,
};

type Props = {
  alerts: Array<News.News>,
  onCloseOpenAlert: Function,
  onMarkNewsAsRead: Function,
  onOpenExternalLink: Function,
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
    const { alerts } = this.props;
    if (alerts.length <= 1) {
      this.props.onMarkNewsAsRead(alerts[0].date);
      this.props.onCloseOpenAlert();
      this.setState({ showOverlay: false });
      return;
    }
    this.props.onMarkNewsAsRead(alerts[0].date);
  };

  renderAction = (action: Object) => {
    if (action && action.url) {
      return (
        <button
          className={styles.actionBtn}
          onClick={() => this.props.onOpenExternalLink(action.url)}
        >
          {action.label}
          <SVGInline svg={externalLinkIcon} />
        </button>
      );
    }
    return null;
  };

  renderCounter = (alerts: Array<News.News>) => {
    if (alerts.length > 1) {
      return <span className={styles.counter}>1 / {alerts.length}</span>;
    }
    return null;
  };

  render() {
    const { showOverlay } = this.state;
    const { alerts } = this.props;
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
            {moment(date).format(this.localizedDateFormat)}
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
