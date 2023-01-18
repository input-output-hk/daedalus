# IOHK JS / CSS Best Practices

*An evolving set of best practices for writing maintainable Javascript and CSS*

## Table of Contents

1. [Javascript](#javascript)
    - [Variables](#variables)
    - [Naming Conventions](#naming-conventions)
    - [Whitespace](#whitespace)
    - [Modules](#modules)
    - [Properties](#properties)
    - [Comparison Operators & Equality](#comparison-operators--equality)
    - [Blocks](#blocks)
    - [Control Statements](#control-statements)
    - [Strings](#strings)
    - [Arrow Functions](#arrow-functions)
1. [React](#react)
    - [Naming](#naming)
    - [Alignment](#alignment)
    - [Spacing](#spacing)
    - [Quotes](#quotes)
    - [Props](#props)
    - [Flow](#flow)
1. [CSS](#css)
    - [Formatting Selectors](#formatting)
    - [Properties](#properties)
    - [Sass](#sass)
    - [Variables](#variables)
    - [Comments](#comments)
1. [Other](#other)
    - [Git](#git)
    - [CHANGELOG](#changelog)

# Javascript

## Variables

Use `const` instead of `var` for all of your variable declarations that will not be reassigned .

:white_check_mark: ***Do***

```javascript
  const a = 1;
  const b = 2;
```

:no_entry_sign: ***Don't***

```javascript
  var a = 1;
  var b = 2;
```

To reassign references, use `let` instead of `var`.

:white_check_mark: ***Do***

```javascript
  let count = 1;
  if (true) {
    count += 1;
  }
```

:no_entry_sign: ***Don't***

```javascript
  var count = 1;
  if (true) {
    count += 1;
  }
```

Declare variables separately as individual statements where each has its own line. Chaining variable declarations together with trailing commas is brittle and error prone.

:white_check_mark: ***Do***

```javascript
const firstVar;
const secondVar;
const thirdVar;
```

:no_entry_sign: ***Don't***

```javascript
const header,
      topnav,
      content;
```

## Naming Conventions

Avoid single letter names for variables, functions, and object properties.

:no_entry_sign: ***Don't***

```javascript
  const a = 1;

  const b = () => {};

  const c = {
    d: false,
    e() {}
  };
```

A variable's name should be descriptive so it indicates how/where it is used and/or clearly communicates the significance of the data it references.

:white_check_mark: ***Do***

```javascript
  const age = 20;

  const isAdult = age => {
    return age >= 18;
  };
```

:no_entry_sign: ***Don't***

```javascript
  const val = 20;

  const calc = data => {
    return data >= 18;
  };
```

The name of a function or method should clearly communicate intended functionality. The name should indicate ***what the function does***, not how it is done.

:white_check_mark: ***Do***

```javascript
  const robot = {
    greeting: 'Hello master!',
    greet() {
      alert(this.greeting);
    }
  };
```

:no_entry_sign: ***Don't***

```javascript
const robot = {
  phrase: 'Hello master!',
  talk() {
    alert(this.phrase);
  }
};
```

## Whitespace

Place 1 space before the leading brace.

:white_check_mark: ***Do***

```javascript
function test() {
  console.log('test');
}

dog.set('attr', {
  age: '1 year',
  breed: 'Bernese Mountain Dog',
});
```

:no_entry_sign: ***Don't***

```javascript
function test(){
  console.log('test');
}

dog.set('attr',{
  age: '1 year',
  breed: 'Bernese Mountain Dog',
});
```

Place 1 space before the opening parenthesis in control statements (`if`, `while`, `for`, etc...). Place no space between the argument list and the function name in function calls and declarations.

:white_check_mark: ***Do***

```javascript
if (isJedi) {
  fight();
}

function fight() {
  console.log('Swooosh!');
}
```

:no_entry_sign: ***Don't***

```javascript
if(isJedi) {
  fight ();
}

function fight () {
  console.log ('Swooosh!');
}
```

Set off operators with spaces.

:white_check_mark: ***Do***

```javascript
const x = y + 5;
```

:no_entry_sign: ***Don't***

```javascript
const x=y+5;
```

Add a space after each curly brace when using the destructuring assignment syntax with objects.

:white_check_mark: ***Do***

```javascript
const { inputs, outputs } = txn;
```

:no_entry_sign: ***Don't***

```javascript
const {inputs, outputs} = txn;
```

Do not pad your blocks with blank lines.

:no_entry_sign: ***Don't***

```javascript
function bar() {

  console.log(foo);

}

if (baz) {

  console.log(qux);
} else {
  console.log(foo);

}

class Foo {

  constructor(bar) {
    this.bar = bar;
  }
}
```

:white_check_mark: ***Do***

```javascript
function bar() {
  console.log(foo);
}

if (baz) {
  console.log(qux);
} else {
  console.log(foo);
}
```

## Modules

Use named exports instead of default exports when possible.

:white_check_mark: ***Do***

```javascript
export const foo = () => {};
```

```javascript
const bar = () => {};

export bar;
```

Only import from a path in one place.

:white_check_mark: ***Do***

```javascript
import foo, { named1, named2 } from 'foo';
```

```javascript
import foo, {
  named1,
  named2,
} from 'foo';
```

:no_entry_sign: ***Don't***

```javascript
import foo from 'foo';
// … some other imports … //
import { named1, named2 } from 'foo';
```

Put all `import` statements above non-import statements.

:white_check_mark: ***Do***

```javascript
import { foo } from 'foo';
import { bar } from 'bar';

foo.init();
```

:no_entry_sign: ***Don't***

```javascript
import { foo } from 'foo';
foo.init();
import { bar } from 'bar';
```

Multiline imports should be indented just like multiline array and object literals.

:white_check_mark: ***Do***

```javascript
import {
  longNameA,
  longNameB,
  longNameC,
  longNameD,
  longNameE,
} from 'path';
```

:no_entry_sign: ***Don't***

```javascript
import { longNameA, longNameB, longNameC, longNameD, longNameE } from 'path';
```

## Properties

Use dot notation when accessing properties.

:white_check_mark: ***Do***

```javascript
const luke = {
  jedi: true,
  age: 28,
};

const isJedi = luke.jedi;
```

:no_entry_sign: ***Don't***

```javascript
const luke = {
  jedi: true,
  age: 28,
};

const isJedi = luke['jedi'];
```

Use bracket notation `[]` when accessing properties with a variable.

:white_check_mark: ***Do***

```javascript
const luke = {
  jedi: true,
  age: 28,
};

function getProp(prop) {
  return luke[prop];
}

const isJedi = getProp('jedi');
```

## Comparison Operators & Equality

Use `===` and `!==` over `==` and `!=` to avoid unintended coercions by the `ToBoolean` abstract method that evaluates expressions in JavaScript.

- **Objects** evaluate to **true**
- **Undefined** evaluates to **false**
- **Null** evaluates to **false**
- **Booleans** evaluate to **the value of the boolean**
- **Numbers** evaluate to **false** if **+0, -0, or NaN**, otherwise **true**
- **Strings** evaluate to **false** if an empty string `''`, otherwise **true**

```javascript
// objects, empty or not, evaluate to true
if ({}) {
  // true
}

// an array (even an empty one) is an object
if ([0] && []) {
  // true
}
```

Use shortcuts for booleans, but explicit comparisons for strings and numbers.

:white_check_mark: ***Do***

```javascript
if (isValid) {
  // ...
}

if (name !== '') {
  // ...
}

if (collection.length > 0) {
  // ...
}
```

:no_entry_sign: ***Don't***

```javascript
if (isValid === true) {
  // use a comparison shortcut for booleans
}

if (collection.length) {
  // ...
}

if (name) {
  // can unexpectedly evaluate to truthy
  // use explicit comparison instead
  // see example below
}

// Example: unexpected truthy evaluation for a string
// Result: alerts "It's true"

const name = ' ';

if (name) {
  alert("It's true");
} else {
  alert("It's false")
}
```

When writing a switch statement, use curly braces to create block scope for each `case` and `default` clause that contain lexical declarations. When each `case`/`default` clause has its own block scope, it prevents unintentional pollution of the namespace with duplicate lexical declarations.

:white_check_mark: ***Do***

```javascript
switch (foo) {
  case 1: {
    let x = 1;
    break;
  }
  case 2: {
    let x = 2;
    break;
  }
  case 3: {
    const y = 1;
    function f() {
      alert(y);
    }
    break;
  }
  case 4:
    const y = 2;
    function f() {
      alert(y);
    }
    break;
  default: {
    class C {}
  }
}
```

:no_entry_sign: ***Don't***

```javascript
switch (foo) {
  case 1:
    let x = 1;
    break;
  case 2:
    let x = 2;
    break;
  case 3:
    const y = 1;
    break;
  default:
    const y = 2;
    class C {}
}
```

Ternaries should not be nested and generally be single line expressions.

:no_entry_sign: ***Don't***

```javascript
const foo = maybe1 > maybe2
  ? "bar"
  : value1 > value2 ? "baz" : null;
```

:white_check_mark: ***Do***

```javascript
// split into 2 separated ternary expressions
const maybeNull = value1 > value2 ? 'baz' : null;

// better, but still uses multiple lines
const foo = maybe1 > maybe2
  ? 'bar'
  : maybeNull;

 // single line is best
const foo = maybe1 > maybe2 ? 'bar' : maybeNull;
```

Avoid unneeded ternary statements.

:no_entry_sign: ***Don't***

```javascript
const foo = a ? a : b;
const bar = c ? true : false;
const baz = c ? false : true;
```

:white_check_mark: ***Do***

```javascript
const foo = a || b;
const bar = !!c;
const baz = !c;
```

When mixing operators, enclose them in parentheses. The only exception is the standard arithmetic operators (`+`, `-`, `*`, & `/`) since their precedence is broadly understood.

:white_check_mark: ***Do***

```javascript
const foo = (a && b < 0) || c > 0 || (d + 1 === 0);

const bar = (a ** b) - (5 % d);

if (a || (b && c)) {
  return d;
}

const bar = a + b / c * d;
```

:no_entry_sign: ***Don't***

```javascript
const foo = a && b < 0 || c > 0 || d + 1 === 0;

const bar = a ** b - 5 % d;

// one may be confused into thinking (a || b) && c
if (a || b && c) {
  return d;
}
```

## Blocks

Use braces with all multi-line blocks.

:white_check_mark: ***Do***

```javascript
if (test) return false;

if (test) { return false; }

if (test) {
  return false;
}

function bar() {
  return false;
}
```

:no_entry_sign: ***Don't***

```javascript
if (test)
  return false;
```

If you’re using multi-line blocks with `if` and `else`, put `else` on the same line as your `if` block’s closing brace.

:white_check_mark: ***Do***

```javascript
if (test) {
  thing1();
  thing2();
} else {
  thing3();
}
```

:no_entry_sign: ***Don't***

```javascript
if (test) {
  thing1();
  thing2();
}
else {
  thing3();
}
```

If an `if` block always executes a `return` statement, the subsequent `else` block is unnecessary. A `return` in an `else if` block following an `if` block that contains a `return` can be separated into multiple `if` blocks.

:white_check_mark: ***Do***

```javascript
function foo() {
  if (x) {
    return x;
  }
  return y;
}

function cats() {
  if (x) {
    return x;
  }

  if (y) {
    return y;
  }
}

function dogs(x) {
  if (x) {
    if (z) {
      return y;
    }
    return q;
  }
  return z;
}
```

:no_entry_sign: ***Don't***

```javascript
function foo() {
  if (x) {
    return x;
  } else {
    return y;
  }
}

function cats() {
  if (x) {
    return x;
  } else if (y) {
    return y;
  }
}

function dogs() {
  if (x) {
    return x;
  } else {
    if (y) {
      return y;
    }
  }
}
```

## Control Statements

If your control statement (`if`, `while` etc.) exceeds the maximum line length, each (grouped) condition could be put into a new line. The logical operator should begin the line.

- *Requiring operators at the beginning of the line persists alignment and follows a pattern similar to method chaining. This visual improvement assists the reader in following complex logic when reading the statement.*

:white_check_mark: ***Do***

```javascript
if (
  foo === 123
  && bar === 'abc'
) {
  thing1();
}

if (
  (foo === 123 || bar === 'abc')
  && doesItLookGoodWhenItBecomesThatLong()
  && isThisReallyHappening()
) {
  thing1();
}

if (foo === 123 && bar === 'abc') {
  thing1();
}
```

:no_entry_sign: ***Don't***

```javascript
if ((foo === 123 || bar === 'abc') && doesItLookGoodWhenItBecomesThatLong() && isThisReallyHappening()) {
  thing1();
}

if (foo === 123 &&
  bar === 'abc') {
  thing1();
}

if (foo === 123
  && bar === 'abc') {
  thing1();
}

if (
  foo === 123 &&
  bar === 'abc'
) {
  thing1();
}
```

When a function contains a control statement, always return early if the return value is known or `null`.

:white_check_mark: ***Do***

```javascript
function meetsMinimum(amount) {
  // return early
  if (!amount) return;

  return amount >= 1;
}
```

:no_entry_sign: ***Don't***

```javascript
function txnIsPending(txn) {
  if (txn && txn.status === 'creating') {
    return true;
  }
  return false;
}
```

## Strings

Use single quotes `''`.

:white_check_mark: ***Do***

```javascript
  const name = 'Ada Lovelace';
```

:no_entry_sign: ***Don't***

```javascript
  const name = "Mary Poppins";
  const name = `Mary Poppins`;
```

Template string literals should contain interpolation of data or newlines.

:white_check_mark: ***Do***

```javascript
  const firstName = 'Ada';
  const lastName = 'Lovelace';
  const fullName = `${firstName} ${lastName}`;

  const bio = `
    My name is ${fullName}. I was born in London, England.
    The year was 1815 and winter was upon us.
  `;
```

:no_entry_sign: ***Don't***

```javascript
  const fullName = `Ada Lovelace`;
```

## Arrow Functions

If your function takes a single argument, omit the parentheses for less visual clutter.

:white_check_mark: ***Do***

```javascript
[1, 2, 3].map(x => x * x);

[1, 2, 3].map(number => (
  `A long string with the ${number}. It’s so long that we don’t want it to take up space on the .map line!`
));
```

:no_entry_sign: ***Don't***

```javascript
[1, 2, 3].map((x) => x * x);

[1, 2, 3].map((x) => {
  const y = x + 1;
  return x * y;
});
```

Enforce the location of arrow function bodies with implicit returns.

:white_check_mark: ***Do***

```javascript
foo => bar;
foo => (bar);
foo => (
    bar
)
```

:no_entry_sign: ***Don't***

```javascript
foo =>
  bar;

foo =>
  (bar);
```

In case the expression spans over multiple lines, wrap it in parentheses for better readability.

:white_check_mark: ***Do***

```javascript
['get', 'post', 'put'].map(httpMethod => (
  Object.prototype.hasOwnProperty.call(
    httpMagicObjectWithAVeryLongName,
    httpMethod,
  )
));
```

:no_entry_sign: ***Don't***

```javascript
['get', 'post', 'put'].map(httpMethod => Object.prototype.hasOwnProperty.call(
    httpMagicObjectWithAVeryLongName,
    httpMethod,
  )
);
```

If the function body consists of a single statement omit the braces and use the implicit return.

:white_check_mark: ***Do***

```javascript
[1, 2, 3].map(number => `A string containing the ${number}.`);

[1, 2, 3].map((number, index) => ({
  [index]: number,
}));
```

# React

## Naming

- **Extensions**: Use the `.js` extension for React components.

- **Filenames**: Use PascalCase for component filenames. :point_right: `TransactionsList.js`.


- **Component Naming**: Use the component name as the filename. For example, `TransactionsList.js` should contain a component named `TransactionsList`.

- **HOC Naming**: Use camelCase to name a higher-order component. Also, use the component name as the filename.  For example, `withTheme.js` should contain an HOC named `withTheme`.

- **HOC displayName**: To create the `displayName` of a higher-order component, combine the HOC's name with the passed-in component’s name. For example, if the HOC's name is `withTheme` and the passed-in component's name is `TransactionsList`, the result is :point_right: `withTheme(TransactionsList)`.


- **Props Naming**: Avoid using DOM component prop names for different purposes. People expect props like `style` and `className` to mean one specific thing.

## Alignment

Follow these alignment styles for JSX.

:no_entry_sign: ***Don't***

```jsx
<Foo superLongParam="bar"
     anotherSuperLongParam="baz" />
```

:white_check_mark: ***Do***

```jsx
<Foo
  superLongParam="bar"
  anotherSuperLongParam="baz"
/>

// if all props fit on one line, that's good!

<Foo bar="bar" />
```

:no_entry_sign: ***Don't***

```jsx
{showButton &&
  <Button />
}

{
  showButton &&
    <Button />
}

```

:white_check_mark: ***Do***

```jsx
{showButton && (
  <Button />
)}

// if the expression fits on one line, that's good!

{showButton && <Button />}
```

## Spacing

Always include a single space in your self-closing tag.

:white_check_mark: ***Do***

```jsx
<Foo />
```

:no_entry_sign: ***Don't***

```jsx
<Foo/>

<Foo
/>

<Foo   />
```

Do **not** pad JSX curly braces with spaces.

:no_entry_sign: ***Don't***

```jsx
<Foo bar={ baz } />
```

:white_check_mark: ***Do***

```jsx
<Foo bar={baz} />
```

## Quotes

Always use double quotes `""` for JSX attributes.

Use single quotes `''` for all other JS.

:white_check_mark: ***Do***

```jsx
<Foo bar="bar" />

<Foo style={{ left: '20px' }} />
```

:no_entry_sign: ***Don't***

```jsx
<Foo bar='bar' />

<Foo style={{ left: "20px" }} />
```

## Props

Always use camelCase for prop names.

:white_check_mark: ***Do***

```jsx
<Foo
  userName="hello"
  phoneNumber={12345678}
/>
```

:no_entry_sign: ***Don't***

```jsx
<Foo
  UserName="hello"
  phone_number={12345678}
/>
```

Omit the value of the prop when it is explicitly `true`.

:white_check_mark: ***Do***

```jsx
<Foo hidden />
```

:no_entry_sign: ***Don't***

```jsx
<Foo hidden={true} />
```

Filter out unused props when using the spread operator to pass props.

:white_check_mark: ***Do***

```jsx
render() {
  const { unusedProp1, unusedProp2, ...usedProps  } = this.props;
  return <WrappedComponent {...usedProps} />;
}
```

:no_entry_sign: ***Don't***

```jsx
render() {
  const { unusedProp1, unusedProp2, ...usedProps  } = this.props;
  return <WrappedComponent {...this.props} />;
}
```

## Flow

When creating a flow type definition, define its properties in **ABC order**.

:white_check_mark: ***Do***

```javascript
type Props = {
  autoFocus: boolean,
  autoResize: boolean,
  className?: string,
  context: ThemeContextProp,
  disabled?: boolean,
  label?: string | Element<any>,
  error?: string | Node,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
};
```

For preventing syntax errors, leave a comma after the last key/value pair in case the type definition's properties are rearranged or expanded upon in the future.

:white_check_mark: ***Do***

```jsx
type State = {
  inputValue: string,
  error: string,
  isOpen: boolean,
  composedTheme: Object,
};
```

:no_entry_sign: ***Don't***

```jsx
type State = {
  inputValue: string,
  error: string,
  isOpen: boolean,
  composedTheme: Object
};
```

## Parentheses

Wrap JSX tags in parentheses when they span more than one line. Multiline JSX should not start on the same line as a parentheses.

:white_check_mark: ***Do***

```jsx
// single line
render() {
  const body = <div>hello</div>;

  return <MyComponent>{body}</MyComponent>;
}

// multiline
render() {
  return (
    <MyComponent foo="bar">
      <MyChild />
    </MyComponent>
  );
}

// both
render() {
  const body = (
    <div>
      <h1>Hello</h1>
    </div>
  );

  return <MyComponent>{body}</MyComponent>;
}

// stateless single line
const Name = () => <h1>Bobby Smith</h1>;

// stateless multiline
const Names = () => (
  <div>
    <h1>Bobby Smith</h1>
    <h1>Brenda Smith</h1>
  </div>
);
```

:no_entry_sign: ***Don't***

```jsx
render() {
  return <MyComponent variant="long body" foo="bar">
            <MyChild />
          </MyComponent>;
}

render() {
  return (<MyComponent variant="long body" foo="bar">
            <MyChild />
          </MyComponent>);
}

const Names = () => (<div>
    <h1>Bobby Smith</h1>
    <h1>Brenda Smith</h1>
  </div>);
```

# CSS

## Formatting Selectors

* Use class selectors instead of ID selectors.
* Use camel case or dashed-case instead of pascal case or names_with_underscores.
* Give each selector its own line.
* Put a space before the opening brace `{` in rule declarations.
* Put closing braces `}` of rule declarations on a new line.
* In properties, put a space after, but not before, the `:` character.
* Put blank lines between rule declarations.

:white_check_mark: ***Do***

```css
.avatar {
  border-radius: 50%;
  border: 2px solid white;
}

.one,
.selector,
.perLine {
  // ...
}
```

:no_entry_sign: ***Don't***

```css
.avatar{
    border-radius:50%;
    border:2px solid white; }

.no, .nope, .not_good {
    // ...
}
#id_selector {
  // ...
}
```

## Properties

The properties of a selector should always be defined in **ABC order**.

:white_check_mark: ***Do***

```css
.supportRequest {
  bottom: 0;
  position: absolute;
  width: auto;
}
```

:no_entry_sign: ***Don't***

```css
.title {
  text-align: center;
  cursor: default;
  margin-bottom: 8.5px;
  line-height: 1.2;
  font-family: var(--font-regular);
  font-size: 20px;
}
```

End every property declaration with a semicolon for consistency and extensibility.

:white_check_mark: ***Do***

```css
.test {
  display: block;
  height: 100px;
}
```

:no_entry_sign: ***Don't***

```css
.test {
  display: block;
  height: 100px
}
```

## Sass

* Always use the `.scss` extension when creating a Sass file.
* Nested selectors go last and nothing goes after them. 
* Add whitespace between your rule declarations.
* Add whitespace between your nested selectors and between adjacent nested selectors.
* The properties of nested selectors should always be defined in **ABC order**.

:white_check_mark: ***Do***

```scss
.btn {
  background: green;
  font-weight: bold;
  @include transition(background 0.5s ease);

  .icon {
    margin-right: 10px;
  }
}
```

:no_entry_sign: ***Don't***

```scss
.btn {
  width: 30px;
  background: green;
  font-weight: bold;
  .icon {
    margin-right: 10px;
  }
  @include transition(background 0.5s ease);
}
```

Define nested selectors in the same order they are applied to their associated HTML. 

**Example**:

```jsx
import React from 'react';

const Modal = () => (
  <div className="modal"> /* --- 1st --- */
    <h3 className="title">Your Modal</h3> /* --- 2nd --- */
    <button className="btn">Accept</button> /* --- 3rd --- */
    <span className="close">X</span> /* --- 4th  --- */
  </div>
);
```

:white_check_mark: ***Do***

```scss
.modal {
  // comes 1st...
  height: 300px;
  width: 400px;
  z-index: 1;

  .title {
    // comes 2nd...
  }

  .btn {
    // comes 3rd...
  }

  .close {
    // comes 4th...
  }
}
```

:no_entry_sign: ***Don't***

```scss
.modal {
  // comes 1st...
  height: 300px;
  width: 400px;
  z-index: 1;

  .btn {
    // // comes 3rd...
  }

  .close {
    // comes 4th...
  }

  .title {
    // comes 2nd...
  }
}
```

Do not nest selectors more than three levels deep. When selectors become this long, you're likely writing CSS that is:

* Strongly coupled to the HTML (fragile) **-- or --**
* Overly specific **-- or --**
* Not reusable

:no_entry_sign: ***Don't***

```scss
.pageContainer {
  .content {
    .profile {
      // STOP!
    }
  }
}
```

## Variables

* Begin a variable name with two dashes `--`.
* Use dash-case CSS variable names (e.g. `$--font-regular`).
* Do ***not*** use camelCase or snake_case variable names. 


## Comments

* Prefer line comments (`//` for Sass) to block comments.
* Prefer comments on their own line. Avoid end-of-line comments.
* Write detailed comments for code that isn't self-documenting:
  - Uses of z-index
  - Compatibility or browser-specific hacks

# Other

## Git

#### Branch Types

- The Daedalus `master` branch is protected by `develop`. Only `develop` is merged into `master`.
- A release branch contains the version in its name in this format `release/version`. Example: (`release/0.12.0`).
- Release branches are based from `master`, but their version specific changes are not merged into `master`.
- To make additions to the Daedalus codebase, create a branch based from `develop`.
- There are three main branch types --> `feature`, `chore`, and `fix`.
- A `feature` branch adds a new feature to Daedalus that does not yet exist.
- A `chore` branch is appropriate when cleaning up or refactoring an area within the codebase.
- A `fix` branch is used to introduce a fix for a bug.

#### Branch Prefix

- A branch prefix is a combination of its type (`feature`, `chore`, or `fix`) and its YouTrack ID.
- YouTrack ID's for Daedalus start with the letters `DDW-` + **issue number**.
- A branch suffix is a concise description of the changes contained in the branch.

*Examples*:

* `feature/ddw-41-create-daedalus-best-practices-guide`
* `chore/ddw-225-refactor-api-to-use-v1`
* `fix/ddw-315-fix-wallet-stuck-on-syncing`

#### Commits

Commit messages should begin with the YouTrack issue ID.

Example: `git commit -m "[DDW-41] Adds Git section to Best Practices"`

## CHANGELOG

* All entries should be written in **past tense**, preferably be **1 sentence** in length, and ordered from **most recent to least recent** using their PR's time of creation.
* The CHANGELOG.md document is organized into sections by release number.
* Each release section contains 3 subsections titled to align with the branch types: `features`, `chores`, `fixes`.
* Add your entry to the appropriate subsection based on its content.
* Add 1 entry per PR and include a URL to the PR at the end of your entry.
  - Example: `- Reduced resource usage ([PR 886](https://github.com/input-output-hk/daedalus/pull/886))`

**[⬆ back to top](#table-of-contents)**
