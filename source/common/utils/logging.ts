import type {
  FormatMessageContextParams,
  ConstructMessageBodyParams,
  MessageBody,
  ElectronLoggerMessage,
} from '../types/logging.types';

const DEFAULT_MESSAGE_BODY = {
  ns: ['daedalus'],
  data: {},
  app: ['daedalus'],
};
const isProd = process.env.NODE_ENV === 'production';
const isSilentMode = process.env.NODE_ENV === 'silence';

const stringifyMessageBody = (messageBody: MessageBody): string => {
  const spacing = isProd ? 0 : 2;
  return JSON.stringify(messageBody, null, spacing);
};

export const filterLogData = (
  data: Record<string, any>
): Record<string, any> => {
  const sensitiveData = [
    'spendingPassword',
    'oldPassword',
    'newPassword',
    'mnemonic',
    'recoveryPhrase',
    'passphrase',
    'password',
    'votingKey',
    'stakeKey',
    'signature',
    'accountPublicKey',
    'extendedPublicKey',
    'publicKeyHex',
    'chainCodeHex',
    'signedTransactionBlob',
    'withdrawal',
    // Governance vote target redaction (slice-1 sanitization floor).
    // omit-deep-lodash recurses by key name at any depth, so these
    // cover delegation.active.voting, delegation.next[*].voting,
    // certificates[*].vote, drepId anywhere, etc.
    'drepId',
    'dRepId',
    'vote',
    'voting',
  ];

  const redact = (value: any): any => {
    if (Array.isArray(value)) {
      return value.map(redact);
    }

    if (value && typeof value === 'object') {
      return Object.keys(value).reduce(
        (result, key) => {
          if (sensitiveData.includes(key)) {
            return result;
          }

          result[key] = redact(value[key]);
          return result;
        },
        {} as Record<string, any>
      );
    }

    return value;
  };

  return redact(data);
};
export const stringifyData = (data: any) => JSON.stringify(data, null, 2);
export const stringifyError = (error: any) =>
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2);
export const formatContext = (context: FormatMessageContextParams): string => {
  const { appName, electronProcess, level, network } = context;
  return `[${appName}.*${network}*:${level}:${electronProcess}]`;
};
export const formatMessageTime = (date: Date): string => {
  const [year, time] = date.toISOString().split('T');
  return `[${year}T${time.slice(0, -1)}Z]`;
};
export const constructMessageBody = (
  bodyData: ConstructMessageBodyParams
): MessageBody => {
  let messageBody = { ...DEFAULT_MESSAGE_BODY, ...bodyData };

  if (typeof messageBody.data === 'string') {
    messageBody = {
      ...messageBody,
      data: {
        response: messageBody.data,
      },
    };
  }

  const { at, env, ns, data, app, msg, pid, sev, thread } = messageBody;
  return {
    at,
    env,
    ns,
    data,
    app,
    msg,
    pid,
    sev,
    thread,
  };
};
export const formatMessage = (loggerMessage: ElectronLoggerMessage): string => {
  const at = loggerMessage.date.toISOString();
  // @ts-ignore ts-migrate(2488) FIXME: Type 'LogSystemInfoParams' must have a '[Symbol.it... Remove this comment to see the full error message
  const [context, messageData] = loggerMessage.data;
  const { level } = loggerMessage;
  const { message: msg, environmentData } = messageData;
  const { network, os, platformVersion, version } = environmentData;
  const messageBodyParams: ConstructMessageBodyParams = {
    at,
    env: `${network}:${os}:${platformVersion}`,
    ns: ['daedalus', `v${version}`, `*${network}*`],
    ...(!!messageData.data && {
      data: messageData.data,
    }),
    msg,
    pid: '',
    sev: level,
    thread: '',
  };
  const messageBody: MessageBody = constructMessageBody(messageBodyParams);
  if (isSilentMode) return '';
  if (isProd) return stringifyMessageBody(messageBody);
  const messageTime: string = formatMessageTime(loggerMessage.date);
  return `${messageTime} ${context} ${stringifyMessageBody(messageBody)}`;
};
