import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './InlineEditingSmallInput.scss... Remove this comment to see the full error message
import styles from './InlineEditingSmallInput.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/pen.inl... Remove this comment to see the full error message
import penIcon from '../../../assets/images/pen.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import crossIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/arrow-r... Remove this comment to see the full error message
import arrowIcon from '../../../assets/images/arrow-right.inline.svg';

type Props = {
  className?: string;
  isActive: boolean;
  isDisabled: boolean;
  placeholder?: string;
  inputFieldLabel?: string;
  inputFieldValue: string;
  onStartEditing?: (...args: Array<any>) => any;
  onStopEditing?: (...args: Array<any>) => any;
  onCancelEditing?: (...args: Array<any>) => any;
  onSubmit: (...args: Array<any>) => any;
  isValid: (...args: Array<any>) => any;
  validationErrorMessage: string;
  successfullyUpdated: boolean;
  inputBlocked?: boolean;
  maxLength?: number;
};
type State = {
  isActive: boolean;
};

@observer
class InlineEditingSmallInput extends Component<Props, State> {
  state = {
    isActive: false,
  };
  static defaultProps = {
    onStartEditing: () => {},
    onStopEditing: () => {},
    onCancelEditing: () => {},
  };
  validator = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        inputField: {
          value: this.props.inputFieldValue,
          validators: [
            ({ field }) => [
              this.props.isValid(field.value),
              this.props.validationErrorMessage,
            ],
          ],
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.validator.submit({
      onSuccess: (form) => {
        const { inputField } = form.values();
        this.setState({
          isActive: false,
        });

        if (inputField !== this.props.inputFieldValue) {
          this.props.onStopEditing();
          this.props.onSubmit(inputField);
        } else {
          this.props.onCancelEditing();
        }

        this.input.blur();
      },
      onError: (form) => {
        const { inputField } = form.values();

        if (!inputField || !form.isValid) {
          this.setState({
            isActive: false,
          });
          this.props.onSubmit(inputField);
        }
      },
    });
  };
  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === 13) {
      // ENTER key
      this.onBlur();
    }

    if (event.which === 27) {
      // ESCAPE key
      this.onCancel();
    }
  };
  onFocus = () => {
    this.setState({
      isActive: true,
    });
    if (this.props.onStartEditing) this.props.onStartEditing();
  };
  onBlur = () => {
    if (this.state.isActive) {
      this.setState({
        isActive: false,
      });
      this.submit();
    }
  };
  onCancel = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = this.validator.$('inputField');
    inputField.value = this.props.inputFieldValue;
    this.setState({
      isActive: false,
    });
    if (this.props.onCancelEditing) this.props.onCancelEditing();
    this.input.blur();
  };

  componentDidUpdate() {
    if (this.props.isActive) {
      const { inputBlocked } = this.props;
      // eslint-disable-next-line no-unused-expressions
      this.inputField && !inputBlocked && this.inputField.focus();
    }
  }

  get input() {
    const fallbackInput = {
      blur: () => {},
      focus: () => {},
    };
    return get(this, 'inputField.inputElement.current', fallbackInput);
  }

  inputField: Input;

  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      isDisabled,
      inputBlocked,
      maxLength,
      placeholder,
    } = this.props;
    const { isActive } = this.state;
    let { successfullyUpdated } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = validator.$('inputField');
    const arrowIconIsVisible = inputField.value !== this.props.inputFieldValue;
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? styles.isActive : null,
      isDisabled ? styles.disabled : null,
      inputField.error ? styles.hasError : null,
      !arrowIconIsVisible ? styles.withoutRightButton : null,
    ]);

    if (isActive || inputBlocked) {
      successfullyUpdated = false;
    }

    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);
    const leftButtonStyles = classnames([
      styles.leftButton,
      !arrowIconIsVisible ? styles.withoutRightButton : null,
    ]);
    return (
      <div
        className={componentStyles}
        onBlur={this.onBlur}
        onMouseUp={() => {
          this.input.focus();
          this.onFocus();
        }}
        role="presentation"
        aria-hidden
      >
        <Input
          className={inputStyles}
          themeOverrides={styles}
          type="text"
          maxLength={maxLength}
          label={inputFieldLabel}
          value={inputField.value}
          onChange={inputField.onChange}
          onFocus={inputField.onFocus}
          onBlur={inputField.onBlur}
          onKeyDown={(event) => this.handleInputKeyDown(event)}
          error={isActive || inputBlocked ? inputField.error : null}
          disabled={isDisabled}
          placeholder={placeholder || ''}
          ref={(input) => {
            this.inputField = input;
          }}
          skin={InputSkin}
        />
        {!isDisabled && (
          <>
            {!isActive ? (
              <Button
                className={styles.rightButton}
                label={
                  <SVGInline
                    svg={penIcon}
                    className={styles.penIcon}
                    // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                    style={{
                      pointerEvents: 'none',
                    }}
                  />
                }
                onMouseUp={() => this.input.focus()}
                onMouseDown={(event: React.MouseEvent<HTMLElement>) => {
                  event.persist();
                  event.preventDefault();
                  event.stopPropagation();
                }}
                skin={ButtonSkin}
              />
            ) : (
              <>
                <Button
                  className={leftButtonStyles}
                  label={
                    <SVGInline
                      svg={crossIcon}
                      className={styles.crossIcon}
                      // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                      style={{
                        pointerEvents: 'none',
                      }}
                    />
                  }
                  onMouseUp={() => {
                    this.onCancel();
                    this.input.blur();
                  }}
                  onMouseDown={(event: React.MouseEvent<HTMLElement>) => {
                    event.persist();
                    event.preventDefault();
                    event.stopPropagation();
                  }}
                  skin={ButtonSkin}
                />
                {arrowIconIsVisible && (
                  <Button
                    className={styles.rightButton}
                    label={
                      <SVGInline
                        svg={arrowIcon}
                        className={styles.arrowIcon}
                        // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                        style={{
                          pointerEvents: 'none',
                        }}
                      />
                    }
                    onMouseUp={() => this.input.blur()}
                    onMouseDown={(event: React.MouseEvent<HTMLElement>) => {
                      event.persist();
                      event.preventDefault();
                      event.stopPropagation();
                    }}
                    skin={ButtonSkin}
                  />
                )}
              </>
            )}
          </>
        )}
      </div>
    );
  }
}

export default InlineEditingSmallInput;
