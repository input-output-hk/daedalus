'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.MnemonicInput = void 0;
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const validations_1 = require('../../../utils/validations');
const MnemonicAutocompleteContainer_1 = require('./MnemonicAutocompleteContainer');
const constants_1 = require('./constants');
const MnemonicInput_scss_1 = __importDefault(require('./MnemonicInput.scss'));
const messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseNoResults: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.noResults',
    defaultMessage: '!!!No results',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase no results label.',
  },
  mnemonicCounter: {
    id: 'paper.wallet.create.certificate.verification.dialog.mnemonicCounter',
    defaultMessage:
      '!!!{providedWordCount} of {requiredWordCount} words entered',
    description: 'Mnemonic input word counter',
  },
});
const MnemonicInput = (0, react_intl_1.injectIntl)(
  ({
    intl,
    onChange,
    value: selectedWords,
    disabled,
    availableWords = [],
    wordCount,
    error,
    reset = false,
    label,
  }) => {
    (0, react_1.useEffect)(() => {
      if (selectedWords.length < 1) {
        onChange((0, lodash_1.times)(wordCount, (0, lodash_1.constant)('')));
      }
    }, [selectedWords]);
    const providedWordCount = selectedWords.filter((word) => word?.length)
      .length;
    const showError =
      providedWordCount === wordCount &&
      error &&
      error !== validations_1.INCOMPLETE_MNEMONIC_MARKER;
    const wordsPerColumn = Math.ceil(wordCount / constants_1.COLUMNS_COUNT);
    const inputIndicesByColumnIndex = (0, react_1.useMemo)(
      () => (0, lodash_1.chunk)((0, lodash_1.times)(wordCount), wordsPerColumn),
      [wordCount]
    );
    const inputRefs = (0, lodash_1.times)(wordCount, () =>
      (0, react_1.useRef)()
    );
    const createHandleWordChange = (0, react_1.useCallback)(
      (idx) => (newValue) => {
        if (!onChange || newValue === selectedWords[idx]) return;
        const newSelectedWords = [...selectedWords];
        newSelectedWords[idx] = newValue;
        onChange(newSelectedWords);
      },
      [selectedWords, onChange]
    );
    const handleInputPaste = (0, react_1.useCallback)(
      (event) => {
        const pastedWords = event.clipboardData
          .getData('Text')
          .trim()
          .split(' ');
        const filteredWords = pastedWords.filter((word) =>
          availableWords.includes(word)
        );
        // single word paste will be handled by `onChange` event
        // multiple word paste will be handled by `onPaste` event itself, and prevents `onChange` invocation
        if (filteredWords.length > 1) {
          event.preventDefault();
        }
        if (filteredWords.length === wordCount) {
          onChange(filteredWords);
        }
      },
      [availableWords, onChange]
    );
    const createHandleConfirmSelection = (0, react_1.useCallback)(
      (idx) => () => {
        inputRefs[idx + 1]?.current.focus();
      },
      [inputRefs]
    );
    return react_1.default.createElement(
      'div',
      { className: MnemonicInput_scss_1.default.root },
      !disabled &&
        react_1.default.createElement(
          'div',
          { className: MnemonicInput_scss_1.default.header },
          label &&
            react_1.default.createElement(
              'div',
              {
                className: (0, classnames_1.default)(
                  MnemonicInput_scss_1.default.headerSlot,
                  MnemonicInput_scss_1.default.headerLeftSlot
                ),
              },
              label
            ),
          react_1.default.createElement(
            'div',
            {
              className: (0, classnames_1.default)(
                MnemonicInput_scss_1.default.headerSlot,
                MnemonicInput_scss_1.default.headerRightSlot,
                showError && MnemonicInput_scss_1.default.headerError
              ),
            },
            showError
              ? error
              : intl.formatMessage(messages.mnemonicCounter, {
                  providedWordCount,
                  requiredWordCount: wordCount,
                })
          )
        ),
      react_1.default.createElement(
        'div',
        { className: MnemonicInput_scss_1.default.content },
        inputIndicesByColumnIndex.map((inputIndices) =>
          react_1.default.createElement(
            'div',
            {
              key: inputIndices.join(''),
              className: MnemonicInput_scss_1.default.inputList,
            },
            inputIndices.map((idx) => {
              const value = selectedWords[idx];
              return react_1.default.createElement(
                'div',
                {
                  key: idx,
                  className: MnemonicInput_scss_1.default.inputWrapper,
                },
                react_1.default.createElement(
                  MnemonicAutocompleteContainer_1.MnemonicAutocompleteContainer,
                  {
                    ordinalNumber: idx + 1,
                    reset: reset,
                    options: availableWords,
                    value: value,
                    onChange: createHandleWordChange(idx),
                    onConfirmSelection: createHandleConfirmSelection(idx),
                    onPaste: handleInputPaste,
                    inputRef: inputRefs[idx],
                    disabled: disabled,
                    maxVisibleOptions: 5,
                    noResultsMessage: intl.formatMessage(
                      messages.recoveryPhraseNoResults
                    ),
                  }
                )
              );
            })
          )
        )
      )
    );
  }
);
exports.MnemonicInput = MnemonicInput;
//# sourceMappingURL=MnemonicInput.js.map
