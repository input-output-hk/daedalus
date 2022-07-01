declare module 'mobx-react-form' {
  export interface SubmitOptions {
    onSuccess: (fieldset) => any;
    onError: (fieldset) => any;
  }

  export interface Field {
    onChange: (...args: any) => void;
    onToggle: (value) => void;
    onFocus: () => void;
    onBlur: () => void;
    onClear: (e) => void;
    onReset: (e) => void;
    bind: (opt?: { onChange?: any }) => Field;

    debouncedValidation: {
      cancel: () => void;
    };

    reset: () => void;
    clear: () => void;
    showErrors: (showMessage: boolean) => void;
    validate: ({ showErrors: boolean }) => void;
    set: (prop: any, value?: any) => void;
    focus: () => void;

    changed: boolean;
    isValid: boolean;
    value: any;
    checked: boolean;
    error: string;
    name: string;
    type: string;
    id: string;
    disabled: boolean;
  }

  export abstract class MobxReactForm<T> {
    constructor(
      fields: { fields: any },
      config?: { hooks?: any; options: any; plugins: any }
    );

    validate(fieldName);

    init(options);

    $(fieldName: keyof T): Field;

    isValid: boolean;
    validating: boolean;

    reset: () => void;
    showErrors: (showMessage: boolean) => void;

    each: (fn: (field: Field) => void) => void;
    add: (field: { name: string; value: string; key: string }) => void;
    del: (field: string) => void;
    values: () => T;
    select: (fieldName: string) => Field;
    submit: (opt: {
      onSuccess?: (form: MobxReactForm<T>) => void;
      onError?: (form: MobxReactForm<T>) => void;
    }) => void;
    clear: () => void;
    fields: {
      get: (fieldName: keyof T) => Field;
      toJSON(): T;
    };
    validator: {
      promises: Array<Promises<void>>;
    };
  }

  export default MobxReactForm;
}
