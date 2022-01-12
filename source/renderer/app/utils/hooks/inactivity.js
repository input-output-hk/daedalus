// @flow
import React, {
  createContext,
  useRef,
  useState,
  useContext,
  useEffect,
  useCallback,
} from 'react';
import type { Node } from 'react';

type Status = 'active' | 'inactive';

const StatusEnum: EnumMap<string, Status> = {
  ACTIVE: 'active',
  INACTIVE: 'inactive',
};

type ReactRef<T> = { current: T };

interface Props {
  children: Node;
}

const INACTIVE_TIMEOUT = 5000; // 5 sec | unit: milliseconds
const EVENTS = ['wheel', 'mousedown', 'keypress'];

export const InactivityContext = createContext<Status>(StatusEnum.ACTIVE);

const configTimeout = (
  ref: ReactRef<TimeoutID | null>,
  setStatus: Function
) => {
  if (ref.current) {
    clearTimeout(ref.current);
  }
  ref.current = setTimeout(
    () => setStatus(StatusEnum.INACTIVE),
    INACTIVE_TIMEOUT
  );
};

export const InactivityProvider = ({ children }: Props) => {
  const [status, setStatus] = useState<Status>(StatusEnum.ACTIVE);
  const checkInactiveTimeout = useRef<TimeoutID | null>(null);

  const resetTimeout = useCallback(() => {
    setStatus(StatusEnum.ACTIVE);
    configTimeout(checkInactiveTimeout, setStatus);
  }, [checkInactiveTimeout, setStatus]);

  useEffect(() => {
    configTimeout(checkInactiveTimeout, setStatus);

    EVENTS.forEach((event) => window.addEventListener(event, resetTimeout));

    return () => {
      clearTimeout(checkInactiveTimeout.current);
      EVENTS.forEach((event) =>
        window.removeEventListener(event, resetTimeout)
      );
    };
  }, [checkInactiveTimeout, setStatus, resetTimeout]);
  console.log(status);
  return (
    <InactivityContext.Provider value={status}>
      {children}
    </InactivityContext.Provider>
  );
};

export const useInactivity = () => {
  const status = useContext(InactivityContext);
  return status;
};
