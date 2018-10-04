// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../common/ipc-api';
import updateCSSVariables from './app/utils/updateCSSVariables';
// import './app/themes/index.global.scss';

declare class File {
  data: [],
  name: string,
}


const support = () => {

  type ZendeskInfo = {
    locale: string,
    themeVars: {
      '--theme-support-widget-header-color': string
    }
  };

  type LogsInfo = {
    compressedLogsFileData: any,
    compressedLogsFileName: string,
    environment: any,
  };

  const locales = {
    'en-US': 'en-US',
    'ja-JP': 'ja',
  };

  const loadFormHandlerWhenIframeIsReady = (logsInfo: LogsInfo) => {
    let count = 0;
    const check = () => {
      try {
        const webWidget = document.getElementById('webWidget');
        const iframeDocument = webWidget && webWidget.contentWindow.document;
        const fileInput = iframeDocument.getElementById('dropzone-input');

        if (fileInput) {
          formHandler(iframeDocument, fileInput, logsInfo);
          clearInterval(interval);
        }
      } catch (e) { // eslint-disable-line
        count++;
        if (count > 20) {
          clearInterval(interval);
        }
      }

    };
    const interval = setInterval(check, 500);
  };

  const formHandler = (
    iframeDocument, fileInput, {
      compressedLogsFileData, compressedLogsFileName
    }: LogsInfo
  ) => {
    const dT = new DataTransfer();
    if (dT.items) {
      const file = new File([compressedLogsFileData], compressedLogsFileName);
      dT.items.add(file);
      fileInput.files = dT.files;
    }

    // Closes the support window when cancelling the form
    const form = iframeDocument.forms[0];
    const cancelButton = form.querySelector('button');
    cancelButton.onclick = window.top.close;

    // Hides the loading overlay
    if (document.body) {
      document.body.classList.add('hideOverlay');
    }

  };

  const closeWindow = () => window.close();

  ipcRenderer.on(
    SUPPORT_WINDOW.ZENDESK_INFO,
    (event, { locale, themeVars }: ZendeskInfo) => {
      updateCSSVariables(themeVars);
      window.zE(() => {
        // window.zE.hide();
        if (locale !== 'en-US') window.zE.setLocale(locales[locale]);
        window.zE.activate();
      });
      window.zESettings = {
        webWidget: {
          color: {
            theme: themeVars['--theme-support-widget-header-color'],
          }
        }
      };
    }
  );

  ipcRenderer.on(SUPPORT_WINDOW.CLOSE, () => closeWindow);

  ipcRenderer.on(SUPPORT_WINDOW.LOGS_INFO, (event, logsInfo: LogsInfo) =>
    loadFormHandlerWhenIframeIsReady(logsInfo));

};

support();
