import fs from 'fs';
import path from 'path';
import ts from 'typescript';
import {
  collectMethodParameterTargets,
  collectWrapperParameterTargets,
  createAuditFixtureProgram,
  electronAuditFixtures,
  resolveIdentifierMethod,
  resolveWrapperCallTarget,
} from '../../../tests/security/privilegedIpcAuditFixtures';
import { privilegedIpcManifest } from './privilegedIpcManifest';

const root = path.resolve(__dirname, '../../..');
const configPath = ts.findConfigFile(root, ts.sys.fileExists, 'tsconfig.json');
if (!configPath) throw new Error('tsconfig.json not found');
const config = ts.readConfigFile(configPath, ts.sys.readFile);
const parsed = ts.parseJsonConfigFileContent(config.config, ts.sys, root);
const productionFiles = parsed.fileNames
  .filter(
    (file) =>
      file.startsWith(path.join(root, 'source/main/')) ||
      file.startsWith(path.join(root, 'source/renderer/'))
  )
  .filter((file) => !/\.(spec|test)\.[jt]sx?$/.test(file));
// These runtime fixtures deliberately construct wrappers and send raw hostile IPC.
const wrapperAuditFiles = productionFiles.filter(
  (file) =>
    !file.endsWith('/source/main/dapp/DappGuestSecurityHarness.ts') &&
    !file.endsWith('/source/main/preloads/dappSecurityHarness.ts')
);
const program = ts.createProgram(productionFiles, parsed.options);
const checker = program.getTypeChecker();
const relative = (file: string) =>
  path.relative(root, file).replace(/\\/g, '/');

const walk = (
  node: ts.Node,
  // eslint-disable-next-line no-unused-vars
  visit: (visitedNode: ts.Node) => void
): void => {
  visit(node);
  node.forEachChild((child) => walk(child, visit));
};

const unalias = (
  symbol: ts.Symbol | undefined,
  typeChecker: ts.TypeChecker
): ts.Symbol | undefined => {
  if (!symbol) return undefined;
  return symbol.flags & ts.SymbolFlags.Alias
    ? unalias(typeChecker.getAliasedSymbol(symbol), typeChecker)
    : symbol;
};

const apiSource = program.getSourceFile(
  path.join(root, 'source/common/ipc/api.ts')
);
const apiModule = apiSource && checker.getSymbolAtLocation(apiSource);
// ponytail: the watchdog refactor left this renderer adapter without a main
// endpoint; remove the exemption when wallet export is reintroduced.
const inactiveRendererOnlyContracts: Record<string, true> = {
  EXPORT_WALLETS_CHANNEL: true,
};
const apiContracts = new Map<ts.Symbol, string>();
for (const symbol of apiModule ? checker.getExportsOfModule(apiModule) : []) {
  if (
    privilegedIpcManifest.some(({ contract }) => contract === symbol.name) ||
    inactiveRendererOnlyContracts[symbol.name]
  )
    apiContracts.set(unalias(symbol, checker) as ts.Symbol, symbol.name);
}

const declaredSymbol = (file: string, name: string): ts.Symbol => {
  const source = program.getSourceFile(path.join(root, file));
  let result: ts.Symbol | undefined;
  if (source)
    walk(source, (node) => {
      if (
        (ts.isClassDeclaration(node) || ts.isInterfaceDeclaration(node)) &&
        node.name?.text === name
      )
        result = checker.getSymbolAtLocation(node.name);
    });
  if (!result) throw new Error(`Declaration not found: ${file}#${name}`);
  return result;
};

type WrapperKind = {
  process: 'main' | 'renderer';
  transport: 'channel' | 'conversation';
};
const wrapperKinds = new Map<ts.Symbol, WrapperKind>([
  [
    declaredSymbol('source/main/ipc/lib/MainIpcChannel.ts', 'MainIpcChannel'),
    { process: 'main', transport: 'channel' },
  ],
  [
    declaredSymbol(
      'source/main/ipc/lib/MainIpcConversation.ts',
      'MainIpcConversation'
    ),
    { process: 'main', transport: 'conversation' },
  ],
  [
    declaredSymbol(
      'source/renderer/app/ipc/lib/RendererIpcChannel.ts',
      'RendererIpcChannel'
    ),
    { process: 'renderer', transport: 'channel' },
  ],
  [
    declaredSymbol(
      'source/renderer/app/ipc/lib/RendererIpcConversation.ts',
      'RendererIpcConversation'
    ),
    { process: 'renderer', transport: 'conversation' },
  ],
]);
const wrapperKind = (
  symbol: ts.Symbol | undefined
): WrapperKind | undefined => {
  const direct = symbol && wrapperKinds.get(symbol);
  if (direct) return direct;
  const declaration = symbol?.declarations?.find((item) =>
    /\/(Main|Renderer)Ipc(Channel|Conversation)\.ts$/.test(
      item.getSourceFile().fileName
    )
  );
  if (!declaration) return undefined;
  const name = symbol?.name || '';
  return {
    process: name.startsWith('Main') ? 'main' : 'renderer',
    transport: name.endsWith('Conversation') ? 'conversation' : 'channel',
  };
};
const hardwareChannelsSymbol = declaredSymbol(
  'source/main/ipc/createHardwareWalletIPCChannels.ts',
  'HardwareWalletChannels'
);

const referencesHardwareChannels = (node: ts.Node): boolean => {
  let found = false;
  walk(node, (child) => {
    if (
      ts.isIdentifier(child) &&
      unalias(checker.getSymbolAtLocation(child), checker) ===
        hardwareChannelsSymbol
    )
      found = true;
  });
  return found;
};

const isHardwareProperty = (symbol: ts.Symbol | undefined): boolean =>
  !!symbol?.declarations?.some(
    (declaration) =>
      declaration
        .getSourceFile()
        .fileName.endsWith('createHardwareWalletIPCChannels.ts') ||
      referencesHardwareChannels(declaration)
  );

const propertyContracts = new Map<ts.Symbol, string>();
const hardwarePropertyContracts = new Map<string, string>();
const parameterContracts = new Map<ts.Symbol, string>();
const currentWindowParameters = new Set<ts.Symbol>();
const parameterWrappers = new Map<ts.Symbol, WrapperKind>();
for (const sourceFile of program
  .getSourceFiles()
  .filter(({ fileName }) =>
    fileName.endsWith('createHardwareWalletIPCChannels.ts')
  )) {
  walk(sourceFile, (node) => {
    if (
      !ts.isPropertyAssignment(node) ||
      !ts.isNewExpression(node.initializer) ||
      !node.initializer.arguments?.length ||
      !ts.isIdentifier(node.initializer.arguments[0])
    )
      return;
    const propertySymbol = checker.getSymbolAtLocation(node.name);
    const contractSymbol = unalias(
      checker.getSymbolAtLocation(node.initializer.arguments[0]),
      checker
    );
    const contract = contractSymbol && apiContracts.get(contractSymbol);
    if (contract) {
      hardwarePropertyContracts.set(node.name.getText(sourceFile), contract);
      if (propertySymbol) propertyContracts.set(propertySymbol, contract);
    }
  });
  walk(sourceFile, (node) => {
    if (
      (!ts.isPropertySignature(node) && !ts.isPropertyDeclaration(node)) ||
      !node.name
    )
      return;
    const contract = hardwarePropertyContracts.get(
      node.name.getText(sourceFile)
    );
    const symbol = checker.getSymbolAtLocation(node.name);
    if (contract && symbol) propertyContracts.set(symbol, contract);
  });
}

const contractFromExpression = (
  expression: ts.Expression,
  typeChecker: ts.TypeChecker,
  seen = new Set<ts.Symbol>()
): string | null => {
  if (ts.isPropertyAccessExpression(expression)) {
    const propertySymbol = checker.getSymbolAtLocation(expression.name);
    const mapped =
      propertySymbol &&
      (propertyContracts.get(propertySymbol) ||
        (isHardwareProperty(propertySymbol)
          ? hardwarePropertyContracts.get(expression.name.text)
          : undefined));
    if (mapped) return mapped;
    return contractFromExpression(expression.expression, typeChecker, seen);
  }
  if (!ts.isIdentifier(expression)) return null;
  const symbol = unalias(
    typeChecker.getSymbolAtLocation(expression),
    typeChecker
  );
  if (!symbol || seen.has(symbol)) return null;
  const directContract = apiContracts.get(symbol);
  if (directContract) return directContract;
  if (
    privilegedIpcManifest.some(({ contract }) => contract === symbol.name) &&
    symbol.declarations?.some((declaration) =>
      declaration.getSourceFile().fileName.endsWith('/source/common/ipc/api.ts')
    )
  )
    return symbol.name;
  const parameterContract = parameterContracts.get(symbol);
  if (parameterContract) return parameterContract;
  seen.add(symbol);
  for (const declaration of symbol.declarations || []) {
    if (ts.isVariableDeclaration(declaration) && declaration.initializer) {
      if (
        ts.isNewExpression(declaration.initializer) &&
        declaration.initializer.arguments?.[0]
      )
        return contractFromExpression(
          declaration.initializer.arguments[0],
          typeChecker,
          seen
        );
      const nested = contractFromExpression(
        declaration.initializer,
        typeChecker,
        seen
      );
      if (nested) return nested;
    }
    if (ts.isBindingElement(declaration)) {
      const mappedSymbol = checker.getSymbolAtLocation(
        declaration.propertyName || declaration.name
      );
      const mapped =
        (mappedSymbol && propertyContracts.get(mappedSymbol)) ||
        (referencesHardwareChannels(declaration.parent.parent)
          ? hardwarePropertyContracts.get(declaration.name.getText())
          : undefined);
      if (mapped) return mapped;
    }
    if (
      (ts.isPropertySignature(declaration) ||
        ts.isPropertyDeclaration(declaration)) &&
      declaration.name
    ) {
      const mappedSymbol = checker.getSymbolAtLocation(declaration.name);
      const mapped = mappedSymbol && propertyContracts.get(mappedSymbol);
      if (mapped) return mapped;
    }
  }
  return null;
};

for (const sourceFile of program
  .getSourceFiles()
  .filter(({ fileName }) => productionFiles.includes(fileName))) {
  walk(sourceFile, (node) => {
    if (!ts.isCallExpression(node)) return;
    const declaration = checker.getResolvedSignature(node)?.declaration;
    if (!declaration || !ts.isFunctionLike(declaration)) return;
    declaration.parameters.forEach((parameter, index) => {
      const argument = node.arguments[index];
      if (!argument || !ts.isIdentifier(parameter.name)) return;
      const contract = contractFromExpression(argument, checker);
      const symbol = unalias(
        checker.getSymbolAtLocation(parameter.name),
        checker
      );
      if (contract && symbol) parameterContracts.set(symbol, contract);
      const argumentSymbol = ts.isIdentifier(argument)
        ? unalias(checker.getSymbolAtLocation(argument), checker)
        : undefined;
      const wrapper = argumentSymbol && wrapperKinds.get(argumentSymbol);
      if (wrapper && symbol) parameterWrappers.set(symbol, wrapper);
      if (
        symbol &&
        argument.getText(sourceFile) === 'currentWindowSender.sender'
      )
        currentWindowParameters.add(symbol);
    });
  });
}

const resolvedWrapper = (
  expression: ts.Expression,
  typeChecker: ts.TypeChecker,
  seen = new Set<ts.Symbol>()
): WrapperKind | null => {
  const symbol = unalias(
    typeChecker.getSymbolAtLocation(expression),
    typeChecker
  );
  if (!symbol || seen.has(symbol)) return null;
  const direct = wrapperKind(symbol) || parameterWrappers.get(symbol);
  if (direct) return direct;
  seen.add(symbol);
  for (const declaration of symbol.declarations || [])
    if (ts.isVariableDeclaration(declaration) && declaration.initializer) {
      const nested = resolvedWrapper(
        declaration.initializer,
        typeChecker,
        seen
      );
      if (nested) return nested;
    }
  const constructor = typeChecker.getSignaturesOfType(
    typeChecker.getTypeAtLocation(expression),
    ts.SignatureKind.Construct
  )[0]?.declaration;
  if (constructor && ts.isConstructorDeclaration(constructor)) {
    const owner = constructor.parent.name;
    const kind = wrapperKind(
      owner && unalias(typeChecker.getSymbolAtLocation(owner), typeChecker)
    );
    if (kind) return kind;
  }
  return null;
};

type Construction = {
  contract: string;
  owner: string;
  transport: 'channel' | 'conversation';
};

const isUnresolvedConstruction = (
  wrapper: WrapperKind | null,
  contract: string | null
) => (!!wrapper && !contract) || (!!contract && !wrapper);
const collectConstructions = (targetProgram: ts.Program, files: string[]) => {
  const targetChecker = targetProgram.getTypeChecker();
  const main: Construction[] = [];
  const renderer: Construction[] = [];
  const unresolved: string[] = [];
  for (const sourceFile of targetProgram
    .getSourceFiles()
    .filter(({ fileName }) => files.includes(fileName)))
    walk(sourceFile, (node) => {
      if (!ts.isNewExpression(node) || !node.arguments?.[0]) return;
      const wrapper = resolvedWrapper(node.expression, targetChecker);
      const contract = contractFromExpression(node.arguments[0], targetChecker);
      if (
        contract &&
        inactiveRendererOnlyContracts[contract] &&
        wrapper?.process === 'renderer'
      )
        return;
      if (isUnresolvedConstruction(wrapper, contract)) {
        unresolved.push(`${relative(sourceFile.fileName)}:${node.getStart()}`);
        return;
      }
      if (!contract || !wrapper) return;
      const construction = {
        contract,
        owner: relative(sourceFile.fileName),
        transport: wrapper.transport,
      };
      (wrapper.process === 'main' ? main : renderer).push(construction);
    });
  return { main, renderer, unresolved };
};
const {
  main: mainConstructions,
  renderer: rendererConstructions,
  unresolved: unresolvedWrapperConstructions,
} = collectConstructions(program, wrapperAuditFiles);

type Registration = {
  contract: string;
  owner: string;
  receive: 'request' | 'broadcast' | 'conversation';
};
type Caller = {
  contract: string;
  owner: string;
  settlement: 'awaited' | 'fire-and-forget-owned' | 'unowned';
  currentWindowTarget: boolean;
};
const collectWrapperOperations = (
  targetProgram: ts.Program,
  files: string[]
) => {
  const targetChecker = targetProgram.getTypeChecker();
  const registrations: Registration[] = [];
  const callers: Caller[] = [];
  const unresolved: string[] = [];
  const parameterTargets = collectWrapperParameterTargets(
    targetProgram,
    files,
    root
  );
  for (const sourceFile of targetProgram
    .getSourceFiles()
    .filter(({ fileName }) => files.includes(fileName))
    .filter(
      ({ fileName }) =>
        !fileName.endsWith('/ipc/lib/MainIpcChannel.ts') &&
        !fileName.endsWith('/ipc/lib/MainIpcConversation.ts')
    ))
    walk(sourceFile, (node) => {
      if (!ts.isCallExpression(node)) return;
      const target = resolveWrapperCallTarget(
        node.expression,
        targetChecker,
        root,
        parameterTargets
      );
      if (!target) return;
      const { method, receiver } = target;
      const contract = receiver
        ? contractFromExpression(receiver, targetChecker)
        : null;
      if (!contract) {
        unresolved.push(`${relative(sourceFile.fileName)}:${node.getStart()}`);
        return;
      }
      if (method === 'onReceive' || method === 'onRequest') {
        const manifest = privilegedIpcManifest.find(
          (entry) => entry.contract === contract
        );
        let receive: Registration['receive'] = 'request';
        if (manifest?.transport === 'conversation') receive = 'conversation';
        else if (method === 'onReceive') receive = 'broadcast';
        registrations.push({
          contract,
          owner: relative(sourceFile.fileName),
          receive,
        });
      }
      if (method !== 'send' && method !== 'request') return;
      let settlement: Caller['settlement'] = 'unowned';
      const ownerCall = ts.isCallExpression(node.parent) ? node.parent : null;
      if (ownerCall && ts.isIdentifier(ownerCall.expression)) {
        if (ownerCall.expression.text === 'consumeIpcResponse')
          settlement = 'fire-and-forget-owned';
        if (ownerCall.expression.text === 'awaitIpcResponse')
          settlement = 'awaited';
      }
      callers.push({
        contract,
        owner: relative(sourceFile.fileName),
        settlement,
        currentWindowTarget:
          node.arguments[1]?.getText(sourceFile) ===
            'currentWindowSender.sender' ||
          (!!node.arguments[1] &&
            ts.isIdentifier(node.arguments[1]) &&
            currentWindowParameters.has(
              unalias(
                targetChecker.getSymbolAtLocation(node.arguments[1]),
                targetChecker
              ) as ts.Symbol
            )),
      });
    });
  return { registrations, callers, unresolved };
};
const mainProductionFiles = productionFiles.filter((fileName) =>
  fileName.startsWith(path.join(root, 'source/main/'))
);
const {
  registrations,
  callers,
  unresolved: unresolvedWrapperCalls,
} = collectWrapperOperations(program, mainProductionFiles);

const stringValues = (
  expression: ts.Expression | undefined,
  typeChecker: ts.TypeChecker
): string[] => {
  if (!expression) return [];
  if (ts.isStringLiteral(expression)) return [expression.text];
  const type = typeChecker.getTypeAtLocation(expression);
  if (type.isStringLiteral()) return [type.value];
  return type.isUnion()
    ? type.types.reduce<string[]>((values, item) => {
        if (item.isStringLiteral()) values.push(item.value);
        return values;
      }, [])
    : [];
};

const electronDeclaration = program
  .getSourceFiles()
  .find((source) =>
    /node_modules\/electron\/electron\.d\.ts$/.test(source.fileName)
  );
if (!electronDeclaration) throw new Error('Electron declarations not found');

const electronTypes = new Map<string, ts.Type>();
walk(electronDeclaration, (node) => {
  if (
    (ts.isClassDeclaration(node) || ts.isInterfaceDeclaration(node)) &&
    node.name &&
    [
      'IpcMain',
      'IpcMainServiceWorker',
      'IpcRenderer',
      'WebContents',
      'MessagePortMain',
    ].includes(node.name.text) &&
    !electronTypes.has(node.name.text)
  ) {
    const symbol = checker.getSymbolAtLocation(node.name);
    if (symbol)
      electronTypes.set(
        node.name.text,
        checker.getDeclaredTypeOfSymbol(symbol)
      );
  }
});

const methodSignatures = (method: ts.Symbol) => {
  const declaration = method.valueDeclaration || method.declarations?.[0];
  return declaration
    ? checker.getSignaturesOfType(
        checker.getTypeOfSymbolAtLocation(method, declaration),
        ts.SignatureKind.Call
      )
    : [];
};

const hasCallableParameter = (
  signature: ts.Signature,
  index: number
): boolean => {
  const parameter = signature.parameters[index];
  const declaration =
    parameter?.valueDeclaration || parameter?.declarations?.[0];
  return !!(
    parameter &&
    declaration &&
    checker.getSignaturesOfType(
      checker.getTypeOfSymbolAtLocation(parameter, declaration),
      ts.SignatureKind.Call
    ).length
  );
};

const registrationMethods = (
  typeName: string,
  firstParameterNames: Set<string>
): Set<string> => {
  const type = electronTypes.get(typeName);
  if (!type) throw new Error(`Electron type not found: ${typeName}`);
  return new Set(
    checker
      .getPropertiesOfType(type)
      .filter(
        (method) =>
          !/^(off|remove)/.test(method.name) &&
          methodSignatures(method).some((signature) => {
            const first = signature.parameters[0];
            const declaration =
              first?.valueDeclaration || first?.declarations?.[0];
            return (
              !!declaration &&
              firstParameterNames.has(
                (declaration as ts.ParameterDeclaration).name.getText()
              ) &&
              hasCallableParameter(signature, 1)
            );
          })
      )
      .map(({ name }) => name)
  );
};

const ipcMainMethods = new Set([
  ...registrationMethods('IpcMain', new Set(['channel', 'event'])),
  ...registrationMethods('IpcMainServiceWorker', new Set(['channel', 'event'])),
]);
const webContentsMethods = registrationMethods(
  'WebContents',
  new Set(['event'])
);
const messagePortMethods = registrationMethods(
  'MessagePortMain',
  new Set(['event'])
);
const rendererMethods = new Set(
  checker
    .getPropertiesOfType(electronTypes.get('IpcRenderer') as ts.Type)
    .filter(
      (method) =>
        !/^(off|remove)/.test(method.name) &&
        method.name !== 'sendToHost' &&
        methodSignatures(method).some((signature) => {
          const first = signature.parameters[0];
          const declaration =
            first?.valueDeclaration || first?.declarations?.[0];
          return (
            (declaration as
              | ts.ParameterDeclaration
              | undefined)?.name.getText() === 'channel' &&
            !hasCallableParameter(signature, 1)
          );
        })
    )
    .map(({ name }) => name)
);

const declaredEventValues = (
  typeName: string,
  methods: Set<string>
): Set<string> => {
  const values = new Set<string>();
  const type = electronTypes.get(typeName);
  if (!type) return values;
  for (const method of checker.getPropertiesOfType(type)) {
    if (methods.has(method.name)) {
      for (const signature of methodSignatures(method)) {
        const parameter = signature.parameters[0];
        const declaration =
          parameter?.valueDeclaration || parameter?.declarations?.[0];
        if (parameter && declaration) {
          const eventType = checker.getTypeOfSymbolAtLocation(
            parameter,
            declaration
          );
          const candidates = eventType.isUnion()
            ? eventType.types
            : [eventType];
          for (const candidate of candidates)
            if (candidate.isStringLiteral()) values.add(candidate.value);
        }
      }
    }
  }
  return values;
};

const webContentsIpcEvents = new Set(
  [...declaredEventValues('WebContents', webContentsMethods)].filter((event) =>
    event.startsWith('ipc-message')
  )
);
const messagePortIpcEvents = new Set(
  [...declaredEventValues('MessagePortMain', messagePortMethods)].filter(
    (event) => event === 'message'
  )
);

const isElectronType = (
  type: ts.Type | null,
  expected: string,
  typeChecker: ts.TypeChecker
): boolean => {
  if (!type) return false;
  const candidates = type.isUnionOrIntersection() ? type.types : [type];
  return candidates.some((candidate) => {
    const symbol = unalias(
      candidate.aliasSymbol || candidate.getSymbol(),
      typeChecker
    );
    if (
      symbol?.name === expected &&
      symbol.declarations?.some((declaration) =>
        /node_modules\/electron\/electron\.d\.ts$/.test(
          declaration.getSourceFile().fileName
        )
      )
    )
      return true;
    return !!(
      candidate.flags & ts.TypeFlags.Object &&
      (candidate as ts.ObjectType).objectFlags &
        ts.ObjectFlags.ClassOrInterface &&
      typeChecker
        .getBaseTypes(candidate as ts.InterfaceType)
        .some((base) => isElectronType(base, expected, typeChecker))
    );
  });
};

const findRawElectronCalls = (
  targetProgram: ts.Program,
  files: string[]
): string[] => {
  const typeChecker = targetProgram.getTypeChecker();
  const violations: string[] = [];
  const parameterMethods = collectMethodParameterTargets(targetProgram, files);
  for (const sourceFile of targetProgram
    .getSourceFiles()
    .filter(({ fileName }) => files.includes(fileName))) {
    walk(sourceFile, (node) => {
      if (!ts.isCallExpression(node)) return;
      let method: string | null = null;
      let type: ts.Type | null = null;
      if (
        ts.isPropertyAccessExpression(node.expression) ||
        ts.isElementAccessExpression(node.expression)
      ) {
        const methodSymbol = unalias(
          typeChecker.getSymbolAtLocation(
            ts.isPropertyAccessExpression(node.expression)
              ? node.expression.name
              : node.expression.argumentExpression
          ),
          typeChecker
        );
        method = methodSymbol?.name || null;
        type = typeChecker.getTypeAtLocation(node.expression.expression);
      } else if (ts.isIdentifier(node.expression)) {
        ({ method, receiverType: type } = resolveIdentifierMethod(
          node.expression,
          typeChecker,
          parameterMethods
        ));
      }
      const values = stringValues(node.arguments[0], typeChecker);
      const isScopedReceiver =
        isElectronType(type, 'IpcMain', typeChecker) ||
        isElectronType(type, 'IpcMainServiceWorker', typeChecker);
      const isWebContentsIpcEvent =
        isElectronType(type, 'WebContents', typeChecker) &&
        webContentsMethods.has(method || '') &&
        (!values.length ||
          values.some((value) => webContentsIpcEvents.has(value)));
      const isMessagePortReceiver =
        isElectronType(type, 'MessagePortMain', typeChecker) &&
        messagePortMethods.has(method || '') &&
        (!values.length ||
          values.some((value) => messagePortIpcEvents.has(value)));
      const isRendererCaller =
        isElectronType(type, 'IpcRenderer', typeChecker) &&
        rendererMethods.has(method || '');
      const unresolvedApplicableCall =
        !method &&
        (isScopedReceiver ||
          isElectronType(type, 'WebContents', typeChecker) ||
          isElectronType(type, 'MessagePortMain', typeChecker) ||
          isElectronType(type, 'IpcRenderer', typeChecker));
      const isDedicatedGuestGateway =
        (relative(sourceFile.fileName) === 'source/main/preloads/dapp.ts' &&
          method === 'invoke' &&
          values.length === 1 &&
          values[0] === 'dapp-cip30-gateway') ||
        (relative(sourceFile.fileName) === 'source/main/cip30/Cip30Broker.ts' &&
          method === 'handle' &&
          values.length === 1 &&
          values[0] === 'dapp-cip30-gateway');
      if (
        !isDedicatedGuestGateway &&
        ((isScopedReceiver && ipcMainMethods.has(method || '')) ||
          isWebContentsIpcEvent ||
          isMessagePortReceiver ||
          isRendererCaller ||
          unresolvedApplicableCall)
      )
        violations.push(`${relative(sourceFile.fileName)}:${node.getStart()}`);
    });
  }
  return violations;
};

describe('privileged IPC manifest', () => {
  it('matches every live constructor, transport, and renderer adapter exactly once', () => {
    expect(unresolvedWrapperConstructions).toEqual([]);
    expect(privilegedIpcManifest).toHaveLength(84);
    expect(mainConstructions).toHaveLength(84);
    expect(rendererConstructions).toHaveLength(84);
    const expected = privilegedIpcManifest
      .map(({ contract, constructorOwner: owner, transport }) => ({
        contract,
        owner,
        transport,
      }))
      .sort((left, right) => left.contract.localeCompare(right.contract));
    expect(
      mainConstructions.sort((left, right) =>
        left.contract.localeCompare(right.contract)
      )
    ).toEqual(expected);
    const expectedRenderer = privilegedIpcManifest
      .map(({ contract, rendererOwner: owner, transport }) => ({
        contract,
        owner,
        transport,
      }))
      .sort((left, right) => left.contract.localeCompare(right.contract));
    expect(
      rendererConstructions.sort((left, right) =>
        left.contract.localeCompare(right.contract)
      )
    ).toEqual(expectedRenderer);
  });

  it('matches every persistent main registration and declared receive endpoint', () => {
    expect(unresolvedWrapperCalls).toEqual([]);
    const expected = privilegedIpcManifest
      .filter(({ receive }) => receive !== 'none')
      .map(({ contract, registrationOwner: owner, receive }) => ({
        contract,
        owner,
        receive,
      }))
      .sort((left, right) => left.contract.localeCompare(right.contract));
    expect(
      registrations.sort((left, right) =>
        left.contract.localeCompare(right.contract)
      )
    ).toEqual(expected);
  });

  it('matches every main caller owner and terminal settlement policy', () => {
    expect(callers).toHaveLength(
      privilegedIpcManifest.reduce(
        (total, entry) => total + entry.callerCount,
        0
      )
    );
    expect(callers.every(({ settlement }) => settlement !== 'unowned')).toBe(
      true
    );
    expect(
      callers.every(({ currentWindowTarget }) => currentWindowTarget)
    ).toBe(true);
    for (const entry of privilegedIpcManifest) {
      const actual = callers.filter(
        ({ contract }) => contract === entry.contract
      );
      expect(actual).toHaveLength(entry.callerCount);
      expect(new Set(actual.map(({ owner }) => owner))).toEqual(
        new Set(entry.callerOwners)
      );
      expect(new Set(actual.map(({ settlement }) => settlement))).toEqual(
        actual.length ? new Set([entry.settlement]) : new Set()
      );
      let expectedDirection = 'renderer-to-main';
      if (entry.receive === 'none') expectedDirection = 'main-to-renderer';
      else if (actual.length) expectedDirection = 'bidirectional';
      expect(entry.direction).toBe(expectedDirection);
    }
  });

  it('detects the frozen Electron 41 declaration set and evasions', () => {
    expect(findRawElectronCalls(program, wrapperAuditFiles)).toEqual([]);
    expect([
      [...ipcMainMethods].sort(),
      [...rendererMethods].sort(),
      [...webContentsIpcEvents].sort(),
      [...messagePortIpcEvents],
    ]).toEqual(electronAuditFixtures.profile);
    const { files } = electronAuditFixtures;
    const fixtureProgram = createAuditFixtureProgram(root, parsed.options, {
      [files.reexport]: electronAuditFixtures.reexport,
      [files.audit]: electronAuditFixtures.audit,
      [files.wrapper]: electronAuditFixtures.wrapper,
    });
    expect(
      findRawElectronCalls(
        fixtureProgram,
        electronAuditFixtures.electronFiles.map((file) => path.join(root, file))
      )
    ).toHaveLength(26);
    const fixtureFiles = [path.join(root, files.wrapper)];
    const constructions = collectConstructions(fixtureProgram, fixtureFiles);
    const operations = collectWrapperOperations(fixtureProgram, fixtureFiles);
    expect({
      mainContracts: constructions.main.map(({ contract }) => contract),
      unresolvedConstructions: constructions.unresolved.length,
      registrationContracts: operations.registrations.map(
        ({ contract }) => contract
      ),
      callerContracts: operations.callers.map(({ contract }) => contract),
      unresolvedCalls: operations.unresolved.length,
    }).toEqual(electronAuditFixtures.collectorResults);
  });

  it('uses only real owners and exact production roots', () => {
    expect(productionFiles.length).toBeGreaterThan(100);
    for (const entry of privilegedIpcManifest) {
      expect(fs.existsSync(path.join(root, entry.constructorOwner))).toBe(true);
      expect(fs.existsSync(path.join(root, entry.rendererOwner))).toBe(true);
      expect(
        !entry.registrationOwner ||
          fs.existsSync(path.join(root, entry.registrationOwner))
      ).toBe(true);
      for (const owner of entry.callerOwners)
        expect(fs.existsSync(path.join(root, owner))).toBe(true);
      expect(entry.authority).toBe('exact-active-trusted-main-frame');
    }
  });
});
