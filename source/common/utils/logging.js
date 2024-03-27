'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.formatMessage = exports.constructMessageBody = exports.formatMessageTime = exports.formatContext = exports.stringifyError = exports.stringifyData = exports.filterLogData = void 0;
const omit_deep_lodash_1 = __importDefault(require('omit-deep-lodash'));
const DEFAULT_MESSAGE_BODY = {
  ns: ['daedalus'],
  data: {},
  app: ['daedalus'],
};
const isProd = process.env.NODE_ENV === 'production';
const isSilentMode = process.env.NODE_ENV === 'silence';
const stringifyMessageBody = (messageBody) => {
  const spacing = isProd ? 0 : 2;
  return JSON.stringify(messageBody, null, spacing);
};
const filterLogData = (data) => {
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
  ];
  return (0, omit_deep_lodash_1.default)(data, ...sensitiveData);
};
exports.filterLogData = filterLogData;
const stringifyData = (data) => JSON.stringify(data, null, 2);
exports.stringifyData = stringifyData;
const stringifyError = (error) =>
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2);
exports.stringifyError = stringifyError;
const formatContext = (context) => {
  const { appName, electronProcess, level, network } = context;
  return `[${appName}.*${network}*:${level}:${electronProcess}]`;
};
exports.formatContext = formatContext;
const formatMessageTime = (date) => {
  const [year, time] = date.toISOString().split('T');
  return `[${year}T${time.slice(0, -1)}Z]`;
};
exports.formatMessageTime = formatMessageTime;
const constructMessageBody = (bodyData) => {
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
exports.constructMessageBody = constructMessageBody;
const formatMessage = (loggerMessage) => {
  const at = loggerMessage.date.toISOString();
  // @ts-ignore ts-migrate(2488) FIXME: Type 'LogSystemInfoParams' must have a '[Symbol.it... Remove this comment to see the full error message
  const [context, messageData] = loggerMessage.data;
  const { level } = loggerMessage;
  const { message: msg, environmentData } = messageData;
  const { network, os, platformVersion, version } = environmentData;
  const messageBodyParams = {
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
  const messageBody = (0, exports.constructMessageBody)(messageBodyParams);
  if (isSilentMode) return '';
  if (isProd) return stringifyMessageBody(messageBody);
  const messageTime = (0, exports.formatMessageTime)(loggerMessage.date);
  return `${messageTime} ${context} ${stringifyMessageBody(messageBody)}`;
};
exports.formatMessage = formatMessage;
//# sourceMappingURL=logging.js.map
