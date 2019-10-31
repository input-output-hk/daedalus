// @flow

type ClassName = string;
type Element = ClassName | HTMLElement;

export const getRelativePosition = (
  targetElement: Element,
  parentElement?: Element
): Object => {
  const targetHTMLElement = getElementHTMLElement(targetElement);
  const parentHTMLElement = parentElement
    ? getElementHTMLElement(parentElement)
    : getParentNode(targetHTMLElement);
  const relativePosition = {};
  if (
    parentHTMLElement instanceof HTMLElement &&
    targetHTMLElement instanceof HTMLElement
  ) {
    const parentPosition = parentHTMLElement.getBoundingClientRect();
    const childrenPosition = targetHTMLElement.getBoundingClientRect();
    relativePosition.top = childrenPosition.top - parentPosition.top;
    relativePosition.left = childrenPosition.left - parentPosition.left;
  }
  return relativePosition;
};

const getElementHTMLElement = (element: Element) =>
  typeof element === 'string' ? document.querySelector(element) : element;

const getParentNode = (element?: ?HTMLElement) =>
  element instanceof HTMLElement ? element.parentNode : element;
