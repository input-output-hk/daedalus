import path from 'path';
import ts from 'typescript';

export type WrapperCallTarget = {
  method: string;
  receiver: ts.Expression | null;
};

const unalias = (
  symbol: ts.Symbol | undefined,
  checker: ts.TypeChecker
): ts.Symbol | undefined =>
  symbol?.flags && symbol.flags & ts.SymbolFlags.Alias
    ? unalias(checker.getAliasedSymbol(symbol), checker)
    : symbol;

const wrapperMethod = (
  symbol: ts.Symbol | undefined,
  checker: ts.TypeChecker,
  root: string
): string | null => {
  const resolved = unalias(symbol, checker);
  if (
    !resolved ||
    !['onReceive', 'onRequest', 'send', 'request'].includes(resolved.name)
  )
    return null;
  const isWrapperMethod = resolved.declarations?.some((declaration) =>
    /source\/(common|main|renderer)\/.*ipc.*\/(lib\/)?(IpcChannel|IpcConversation|MainIpcChannel|MainIpcConversation|RendererIpcChannel|RendererIpcConversation)\.ts$/.test(
      path
        .relative(root, declaration.getSourceFile().fileName)
        .replace(/\\/g, '/')
    )
  );
  return isWrapperMethod ? resolved.name : null;
};

export const resolveWrapperCallTarget = (
  expression: ts.Expression,
  checker: ts.TypeChecker,
  root: string,
  parameterTargets = new Map<ts.Symbol, WrapperCallTarget>(),
  seen = new Set<ts.Symbol>()
): WrapperCallTarget | null => {
  if (
    ts.isPropertyAccessExpression(expression) ||
    ts.isElementAccessExpression(expression)
  ) {
    const name = ts.isPropertyAccessExpression(expression)
      ? expression.name
      : expression.argumentExpression;
    const method = wrapperMethod(
      checker.getSymbolAtLocation(name),
      checker,
      root
    );
    return method ? { method, receiver: expression.expression } : null;
  }
  if (!ts.isIdentifier(expression)) return null;
  const symbol = unalias(checker.getSymbolAtLocation(expression), checker);
  if (!symbol || seen.has(symbol)) return null;
  const parameterTarget = parameterTargets.get(symbol);
  if (parameterTarget) return parameterTarget;
  seen.add(symbol);
  for (const declaration of symbol.declarations || []) {
    if (ts.isVariableDeclaration(declaration) && declaration.initializer) {
      let { initializer } = declaration;
      if (
        ts.isCallExpression(initializer) &&
        ts.isPropertyAccessExpression(initializer.expression) &&
        initializer.expression.name.text === 'bind'
      )
        initializer = initializer.expression.expression;
      const nested = resolveWrapperCallTarget(
        initializer,
        checker,
        root,
        parameterTargets,
        seen
      );
      if (nested) return nested;
    }
    if (ts.isBindingElement(declaration)) {
      const variable = declaration.parent.parent;
      if (ts.isVariableDeclaration(variable) && variable.initializer) {
        const method = wrapperMethod(
          checker.getPropertyOfType(
            checker.getTypeAtLocation(variable.initializer),
            (declaration.propertyName || declaration.name).getText()
          ),
          checker,
          root
        );
        if (method) return { method, receiver: variable.initializer };
      }
    }
  }
  const declaration = checker.getSignaturesOfType(
    checker.getTypeAtLocation(expression),
    ts.SignatureKind.Call
  )[0]?.declaration;
  const method =
    declaration &&
    (ts.isMethodDeclaration(declaration) || ts.isMethodSignature(declaration))
      ? wrapperMethod(
          checker.getSymbolAtLocation(declaration.name),
          checker,
          root
        )
      : null;
  return method ? { method, receiver: null } : null;
};

export const collectWrapperParameterTargets = (
  program: ts.Program,
  files: string[],
  root: string
): Map<ts.Symbol, WrapperCallTarget> => {
  const checker = program.getTypeChecker();
  const targets = new Map<ts.Symbol, WrapperCallTarget>();
  for (let pass = 0; pass < 3; pass += 1)
    for (const source of program
      .getSourceFiles()
      .filter(({ fileName }) => files.includes(fileName)))
      source.forEachChild(function visit(node) {
        if (ts.isCallExpression(node)) {
          const declaration = checker.getResolvedSignature(node)?.declaration;
          if (declaration && ts.isFunctionLike(declaration))
            declaration.parameters.forEach((parameter, index) => {
              const argument = node.arguments[index];
              const symbol = checker.getSymbolAtLocation(parameter.name);
              const target =
                argument &&
                resolveWrapperCallTarget(argument, checker, root, targets);
              if (symbol && target) targets.set(symbol, target);
            });
        }
        node.forEachChild(visit);
      });
  return targets;
};

export type MethodTarget = {
  method: string | null;
  receiverType: ts.Type | null;
};

export const resolveIdentifierMethod = (
  identifier: ts.Identifier,
  checker: ts.TypeChecker,
  parameterMethods = new Map<ts.Symbol, MethodTarget>(),
  seen = new Set<ts.Symbol>()
): MethodTarget => {
  const symbol = unalias(checker.getSymbolAtLocation(identifier), checker);
  if (!symbol || seen.has(symbol)) return { method: null, receiverType: null };
  const parameterMethod = parameterMethods.get(symbol);
  if (parameterMethod) return parameterMethod;
  seen.add(symbol);
  for (const declaration of symbol.declarations || []) {
    if (ts.isBindingElement(declaration)) {
      const variable = declaration.parent.parent;
      if (ts.isVariableDeclaration(variable) && variable.initializer)
        return {
          method:
            checker.getPropertyOfType(
              checker.getTypeAtLocation(variable.initializer),
              (declaration.propertyName || declaration.name).getText()
            )?.name || null,
          receiverType: checker.getTypeAtLocation(variable.initializer),
        };
    }
    if (ts.isVariableDeclaration(declaration) && declaration.initializer) {
      let { initializer } = declaration;
      if (
        ts.isCallExpression(initializer) &&
        ts.isPropertyAccessExpression(initializer.expression) &&
        initializer.expression.name.text === 'bind'
      )
        initializer = initializer.expression.expression;
      if (ts.isIdentifier(initializer)) {
        const nested = resolveIdentifierMethod(
          initializer,
          checker,
          parameterMethods,
          seen
        );
        if (nested.method || nested.receiverType) return nested;
      }
      if (ts.isPropertyAccessExpression(initializer))
        return {
          method:
            unalias(checker.getSymbolAtLocation(initializer.name), checker)
              ?.name || null,
          receiverType: checker.getTypeAtLocation(initializer.expression),
        };
    }
    if (
      ts.isParameter(declaration) &&
      declaration.type &&
      ts.isTypeQueryNode(declaration.type) &&
      ts.isQualifiedName(declaration.type.exprName)
    ) {
      const { left, right } = declaration.type.exprName;
      return {
        method:
          unalias(checker.getSymbolAtLocation(right), checker)?.name || null,
        receiverType: checker.getTypeAtLocation(left),
      };
    }
  }
  const declaration = checker.getSignaturesOfType(
    checker.getTypeAtLocation(identifier),
    ts.SignatureKind.Call
  )[0]?.declaration;
  if (
    declaration &&
    (ts.isMethodSignature(declaration) || ts.isMethodDeclaration(declaration))
  ) {
    const { parent } = declaration;
    const parentName =
      (ts.isClassDeclaration(parent) ||
        ts.isClassExpression(parent) ||
        ts.isInterfaceDeclaration(parent)) &&
      parent.name;
    const parentSymbol = parentName
      ? checker.getSymbolAtLocation(parentName)
      : undefined;
    return {
      method:
        unalias(checker.getSymbolAtLocation(declaration.name), checker)?.name ||
        null,
      receiverType: parentSymbol
        ? checker.getDeclaredTypeOfSymbol(parentSymbol)
        : null,
    };
  }
  return { method: null, receiverType: null };
};

export const collectMethodParameterTargets = (
  program: ts.Program,
  files: string[]
): Map<ts.Symbol, MethodTarget> => {
  const checker = program.getTypeChecker();
  const targets = new Map<ts.Symbol, MethodTarget>();
  for (let pass = 0; pass < 3; pass += 1)
    for (const source of program
      .getSourceFiles()
      .filter(({ fileName }) => files.includes(fileName)))
      source.forEachChild(function visit(node) {
        if (ts.isCallExpression(node)) {
          const declaration = checker.getResolvedSignature(node)?.declaration;
          if (declaration && ts.isFunctionLike(declaration))
            declaration.parameters.forEach((parameter, index) => {
              const argument = node.arguments[index];
              const symbol = unalias(
                checker.getSymbolAtLocation(parameter.name),
                checker
              );
              if (!argument || !symbol) return;
              if (ts.isIdentifier(argument)) {
                const target = resolveIdentifierMethod(
                  argument,
                  checker,
                  targets
                );
                if (target.method || target.receiverType)
                  targets.set(symbol, target);
              } else if (ts.isPropertyAccessExpression(argument))
                targets.set(symbol, {
                  method:
                    unalias(checker.getSymbolAtLocation(argument.name), checker)
                      ?.name || null,
                  receiverType: checker.getTypeAtLocation(argument.expression),
                });
            });
        }
        node.forEachChild(visit);
      });
  return targets;
};

export const createAuditFixtureProgram = (
  root: string,
  options: ts.CompilerOptions,
  files: Record<string, string>
): ts.Program => {
  const normalized = new Map(
    Object.entries(files).map(([file, source]) => [
      path.join(root, file),
      source,
    ])
  );
  const host = ts.createCompilerHost(options);
  const originalGetSourceFile = host.getSourceFile.bind(host);
  host.fileExists = (file) => normalized.has(file) || ts.sys.fileExists(file);
  host.readFile = (file) => normalized.get(file) || ts.sys.readFile(file);
  host.getSourceFile = (file, languageVersion, onError, shouldCreate) =>
    normalized.has(file)
      ? ts.createSourceFile(
          file,
          normalized.get(file) || '',
          languageVersion,
          true
        )
      : originalGetSourceFile(file, languageVersion, onError, shouldCreate);
  return ts.createProgram([...normalized.keys()], options, host);
};

export const electronAuditFixtures = {
  files: {
    audit: 'source/main/ipc-audit-fixture.ts',
    reexport: 'source/main/ipc-audit-reexport.ts',
    wrapper: 'source/main/ipc-wrapper-audit-fixture.ts',
  },
  electronFiles: [
    'source/main/ipc-audit-fixture.ts',
    'source/main/ipc-audit-reexport.ts',
  ],
  collectorResults: {
    mainContracts: ['GET_LOGS_CHANNEL', 'GET_LOGS_CHANNEL'],
    unresolvedConstructions: 2,
    registrationContracts: ['GET_LOGS_CHANNEL'],
    callerContracts: ['GET_LOGS_CHANNEL', 'GET_LOGS_CHANNEL'],
    unresolvedCalls: 0,
  },
  profile: [
    [
      'addListener',
      'handle',
      'handleOnce',
      'on',
      'once',
      'prependListener',
      'prependOnceListener',
    ],
    ['invoke', 'postMessage', 'send', 'sendSync'],
    ['ipc-message', 'ipc-message-sync'],
    ['message'],
  ],
  reexport: `
    import { ipcMain } from 'electron';
    const firstForward = ipcMain.on.bind(ipcMain);
    export const forwarded = firstForward;
  `,
  wrapper: `
    import { MainIpcChannel } from './ipc/lib/MainIpcChannel';
    import { GET_LOGS_CHANNEL } from '../common/ipc/api';
    const channel = new MainIpcChannel<void, void>(GET_LOGS_CHANNEL);
    declare const Channel: typeof MainIpcChannel;
    new Channel(GET_LOGS_CHANNEL);
    new MainIpcChannel('unmanifested');
    class Other { constructor(_channel: string) {} }
    new Other(GET_LOGS_CHANNEL);
    const first = channel.onRequest;
    const second = first;
    second(async () => {});
    const bound = channel.send.bind(channel);
    bound(undefined, null as never);
    function typed(call: (message: void, sender: any) => Promise<void>) {
      call(undefined, null as never);
    }
    typed(channel.request);
  `,
  audit: `
    import {
      ipcMain,
      ipcRenderer,
      IpcMain,
      WebContents,
      WebFrameMain,
      ServiceWorkerMain,
      MessagePortMain,
    } from 'electron';
    import { forwarded } from './ipc-audit-reexport';
    ipcMain.on('a', () => {});
    ipcMain.once('a', () => {});
    ipcMain.addListener('a', () => {});
    ipcMain.prependListener('a', () => {});
    ipcMain.prependOnceListener('a', () => {});
    ipcMain.handle('a', () => {});
    ipcMain.handleOnce('a', () => {});
    const { on } = ipcMain;
    on('a', () => {});
    const extracted = ipcMain.once;
    const second = extracted;
    second('a', () => {});
    const bound = ipcMain.addListener.bind(ipcMain);
    bound('a', () => {});
    forwarded('a', () => {});
    function typed(register: typeof ipcMain.on) {
      register('a', () => {});
    }
    typed(ipcMain.on);
    function structural(
      register: (channel: string, listener: (event: any) => void) => IpcMain
    ) {
      register('a', () => {});
    }
    structural(ipcMain.on);
    declare const frame: WebFrameMain;
    frame.ipc.on('a', () => {});
    declare const contents: WebContents;
    contents.ipc.handle('a', () => {});
    declare const worker: ServiceWorkerMain;
    worker.ipc.once('a', () => {});
    const eventName = 'ipc-message' as const;
    contents.on(eventName, () => {});
    contents.prependOnceListener('ipc-message-sync', () => {});
    declare const widenedEventName: string;
    contents.on(widenedEventName, () => {});
    ipcMain['on']('a', () => {});
    declare const port: MessagePortMain;
    port.on('message', () => {});
    port.prependListener('message', () => {});
    ipcRenderer.send('a');
    ipcRenderer.sendSync('a');
    ipcRenderer.invoke('a');
    ipcRenderer.postMessage('a', null, [port]);
    ipcRenderer.sendToHost('not-main');
  `,
};
