# IOHK JS / CSS Best Practices

*An evolving set of best practices for writing maintainable Javascript and CSS*

## Table of Contents

1. [Javascript](#javascript)
    - [Variables](#variables)
    - [Naming Conventions](#naming-conventions)
    - [Strings](#strings)
    - [Whitespace](#whitespace)
1. [React](#react)
    - [Naming](#naming)
    - [Alignment](#alignment)
    - [Quotes](#quotes)
1. [CSS](#css)
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

# React

## Naming

- **Extensions**: Use `.js` extension for React components.

- **Filenames**: Use PascalCase for component filenames. :point_right: `TransactionsList.js`.


- **Component Naming**: Use the filename as the component name. For example, `TransactionsList.js` should contain a component named `TransactionsList`.

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

**[⬆ back to top](#table-of-contents)**

:white_check_mark: ***Do***

```jsx
```

:no_entry_sign: ***Don't***

```jsx
```

:white_check_mark: ***Do***
```javascript
```

:no_entry_sign: ***Don't***

```javascript
```
