import React, { Component } from "react";
import moment from "moment";
import { observer } from "mobx-react";
import { get, camelCase } from "lodash";
import ReactMarkdown from "react-markdown";
import classnames from "classnames";
import { ButtonSkin } from "react-polymorph/lib/skins/simple/ButtonSkin";
import News from "../../domains/News";
import ButtonLink from "../widgets/ButtonLink";
import styles from "./IncidentOverlay.scss";
type Props = {
  incident: News.News;
  onOpenExternalLink: (...args: Array<any>) => any;
  onProceedNewsAction: (...args: Array<any>) => any;
  currentDateFormat: string;
};
export default @observer
class IncidentOverlay extends Component<Props> {
  localizedDateFormat: "MM/DD/YYYY";

  componentDidMount() {
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
    const {
      incident,
      onProceedNewsAction
    } = this.props;
    onProceedNewsAction(incident, event);
  };
  renderAction = (action: Record<string, any>) => {
    if (action && (action.url || action.event)) {
      return <ButtonLink className={styles.actionBtn} onClick={this.onProceedNewsAction} skin={ButtonSkin} label={action.label} linkProps={{
        className: styles.externalLink,
        hasIconBefore: false,
        hasIconAfter: action.url && true
      }} />;
    }

    return null;
  };

  render() {
    const {
      incident,
      currentDateFormat
    } = this.props;
    const {
      content,
      date,
      action,
      title
    } = incident;
    const componentClasses = classnames([styles.component, styles[camelCase(incident.color)]]);
    return <div className={componentClasses}>
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.date}>
          {moment(date).format(currentDateFormat)}
        </span>
        <div className={styles.content} role="presentation" onClick={this.contentClickHandler.bind(this)}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {this.renderAction(action)}
      </div>;
  }

}