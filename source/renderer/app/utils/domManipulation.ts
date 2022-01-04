type ClassName = string;
type Element = ClassName | HTMLElement;
export const getRelativePosition = (
  targetElement: Element,
  parentElement?: Element | null | undefined
): Record<string, any> => {
  const targetHTMLElement = getElementHTMLElement(targetElement);
  const parentHTMLElement = parentElement
    ? getElementHTMLElement(parentElement)
    : // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Element' is not assignable to pa... Remove this comment to see the full error message
      getParentNode(targetHTMLElement);
  const relativePosition = {};

  if (
    parentHTMLElement instanceof HTMLElement &&
    targetHTMLElement instanceof HTMLElement
  ) {
    const parentPosition = parentHTMLElement.getBoundingClientRect();
    const childrenPosition = targetHTMLElement.getBoundingClientRect();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'top' does not exist on type '{}'.
    relativePosition.top = childrenPosition.top - parentPosition.top;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'left' does not exist on type '{}'.
    relativePosition.left = childrenPosition.left - parentPosition.left;
  }

  return relativePosition;
};

const getElementHTMLElement = (element: Element) =>
  typeof element === 'string' ? document.querySelector(element) : element;

const getParentNode = (element?: HTMLElement | null | undefined) =>
  element instanceof HTMLElement ? element.parentNode : element;
