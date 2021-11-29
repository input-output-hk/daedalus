# MIGRATION TO TYPESCRIPT SHELL SCRIPT

: '
This file is designed to assist in the migration of flow to typescript
This script pauses after completion of each step so that incremental changes can be commited
After this you will still need to complete the following steps:

Sections of the codebase to migrate
> Source
    > Common
    > Main
    > Renderer
> Storybook
> Tests
> Translations
> Utils


SIMPLIFIED STEPS

- PRE-REQUISITE STEPS (globstar, remove modules)
- UPDATE CONFIGURATION FILES (non-js)
- REMOVE FLOW AND REPLACE WITH TYPESCRIPT
- CHANGE TO CORRECT PACKAGES + ADD TYPES
- CONVERT JS TO TS/TSX - retaining git history
- PRE-CONVERSIONS STEPS
- FLOW CONVERSION
- TS-MIGRATE WITH @TS-IGNORE, @TS-EXPECT-ERROR ETC.
- FIX LOCKFILE + CLEANUP STEPS
'

#GLOBAL VARS
RUN_STAGE=UNSET
PAUSE_AFTER_STAGE=false
STEP=0
# FOLDERS TO MIGRATE
MIGRATION_FOLDERS=(
    scripts
    source
    storybook
    tests
    translations
    utils
)

# PARAMAETER FLAGS FOR SCRIPT
while [ "$1" != "" ]; do
    case $1 in
    -p | --pauseAfterStage)
        shift # remove `-p` or `--pauseAfterStage` from `$flag`
        PAUSE_AFTER_STAGE=true
        ;;
    -s | --stage)
        RUN_STAGE=$2
        shift # remove `-s` or `--stage` from `$flag`
        ;;
    *)
        echo "Incorrect flag passed $1"
        exit 1
        ;;
    esac
    shift # remove the current value for `$1` and use the next
done

# ERROR HANDLING
set -e # EXIT WHEN ANY COMMAND FAILS
# ECHO AN ERROR MESSAGE BEFORE EXITING
catch() {
  if [ "$1" != "0" ];
    then echo -e "\033[K${LAST_COMMAND} Failed ❌"
    else echo -ne "\nEXECUTION COMPLETE ✅"
  fi
}
trap 'catch $?' EXIT


# HELPER FUNCTIONS
function pause() {
    # ALLOW SCRIPT EXECUTION TO PAUSE IF -p FLAG SET SO THAT INDIVIDUAL STEPS CAN BE COMMITTED AS SEPARATE STEPS
    if [ "$PAUSE_AFTER_STAGE" = true ];
        then
            read -p $"🚀 ${STEP}. ${1^^}, you may wish to commit the changes at this point. Press any key to resume ..."
        else
            echo -e "\033[K🚀 ${STEP}. ${1^^}"
    fi
}

function spin_while_executing() {
    pid=$! # PROCESS ID OF THE PREVIOUS RUNNING COMMAND
    spin='-\|/'

    i=0
    while kill -0 $pid 2>/dev/null
        do
        i=$(( (i+1) %4 ))
        echo -ne "\rExecuting ${LAST_COMMAND}: ${spin:$i:1}"
        sleep .1
    done
    echo -ne "\r"
}

## TODOS
# figure out do we need ts-loader vs babel-loader??
# typescript emit files for build?
# dependencyNamesToRemove??

# Check if check:all function still passes
function check_all() {
    yarn check:all
    pause "check:all success! the project is not broken"
}

# PRE-REQUISITE FUNCTIONS
function enable_globstar() {
    shopt -s globstar # ALLOWS RECURSION GLOBBING (NOT ALWAYS ENABLED BY DEFAULT IN TERMINAL)
    pause "enabled globstar"
}

function remove_node_modules() {
    rm -rf node_modules & spin_while_executing # START FROM FRESH TO AVOID PACKAGE CONFLICTS
    pause "remove node modules"
}

prerequisite_functions=(
    enable_globstar
    remove_node_modules
)


# CONFIGURATION FUNCTIONS
function remove_flow_files() {
    rm -f ./.flowconfig & rm -rf ./flow #& spin_while_executing
    pause "remove flow files + config"
}

function remove_packages() {
    # TODO find a way to avoid errors when packages are already uninstalled --silent isn't working
    yarn remove @babel/preset-flow eslint-plugin-flowtype flow-bin gulp-flow-remove-types babel-eslint
    pause "un-install flow packages"
}

function add_type_packages() {
    yarn add -D @types/react @types/aes-js @types/qrcode.react @types/react-copy-to-clipboard @types/react-svg-inline @types/node ts-migrate babel-eslint @typescript-eslint/eslint-plugin
    # Might need @types/react-dom @types/electron
    pause "add @type packages"
}

function install_packages() {
    yarn install
    # INSTALL TS PACKAGES
    yarn add typescript ts-node ts-loader@8.2.0 @babel/plugin-transform-typescript @babel/preset-typescript
    add_type_packages
    pause "install typescript packages"
}

function update_babelrc() {
    sed -i 's/flow/typescript/' ./.babelrc
    pause "update babel configuration"
}

function update_package_json() {
    sed -i 's/"flow:test": "flow; test $? -eq 0 -o $? -eq 2"/"compile": "tsc --noEmit"/' ./package.json
    sed -i 's/flow:test/compile/' ./package.json
    sed -i "s/\(\"lint\".*\)\(.js\)/\1.ts,*.tsx/g" ./package.json
    sed -i "s/\(.js\)\(['|\"| ][^:]\)/.ts\2/g" ./package.json
    sed -i 's/\/main\/index.ts/\/main\/index.js/' ./package.json
    sed -i "s/\(\"\|\s\)node /\1ts-node /g" ./package.json
    pause "update package.json"
}

function update_prettierignore() {
    sed -i "s/\\!\\*\\.js$/!*.ts\\n!*.tsx/" ./.prettierignore
    pause "update pretter ignore"
}

function create_tsconfig() {
    tsc --init \
        --noImplicitAny false \
        --noImplicitThis false \
        --strict false \
        --allowSyntheticDefaultImports \
        --experimentalDecorators \
        --resolveJsonModule \
        --useDefineForClassFields \
        --noEmitOnError \
        --jsx react
        #moduleResolution node

    # EXCLUDE NODE_MODULES FROM TSC
    pause '.tsconfig generation'
}

function change_webpack_configuration() {
    # substitutions
    MODULE_FIND='(module: {)'
    MODULE_ADD_RESOLVE="resolve: {\n\t\textensions: \[\'.tsx\', \'.ts\', \'.js\', \'.json\'],\n\t},"

    LOADER_FIND="'babel-loader'"
    LOADER_REPLACE="{\n\t\t\t\t\t\tloader: \'babel-loader\',\n\t\t\t\t\t\toptions: {\n\t\t\t\t\t\t\tpresets: \[\n\t\t\t\t\t\t\t\t\'\@babel\/preset-env\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-react\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-typescript\',\n\t\t\t\t\t\t\t\],\n\t\t\t\t\t\t},\n\t\t\t\t\t}"

    # source/main
    perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./source/main/webpack.config.js
    perl -i -pe "s/$LOADER_FIND/\n\t\t\t\t\t${LOADER_REPLACE},\n\t\t\t\t/" ./source/main/webpack.config.js
    sed -i "s/\(index\|preload\)\.js/\1.ts/g" ./source/main/webpack.config.js
    sed -i "s/jsx/tsx/g" ./source/main/webpack.config.js
    # source/renderer
    perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./source/renderer/webpack.config.js
    perl -i -pe "s/$LOADER_FIND/${LOADER_REPLACE}/" ./source/renderer/webpack.config.js
    sed -i "s/\(renderer\/index\).js/\1.ts/g" ./source/renderer/webpack.config.js
    sed -i "s/jsx/tsx/g" ./source/renderer/webpack.config.js

    # storybook
    LOADER_REPLACE_STORYBOOK="{\n\t\t\t\t\t\ttest: \/\.tsx?\\$\/,\n\t\t\t\t\t\tloader: \'babel-loader\',\n\t\t\t\t\t\toptions: {\n\t\t\t\t\t\t\tpresets: \[\n\t\t\t\t\t\t\t\t\'\@babel\/preset-env\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-react\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-typescript\',\n\t\t\t\t\t\t\t\],\n\t\t\t\t\t\t},\n\t\t\t\t\t}"
    perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./storybook/webpack.config.js
    perl -i -pe "s/(jsxRule,\n)/\1\t\t\t\t${LOADER_REPLACE_STORYBOOK},/g" ./storybook/webpack.config.js
    pause "change_webpack_configuration"
}

function remove_flow_from_eslintrc() {
    sed -i "s/flowtype/@typescript-eslint/g" ./.eslintrc
    pause "remove flow from eslintrc"
}

function add_declaration_dts() {
    touch declarations.d.ts
    echo "declare module '*.svg' {
  const content: any;
  export default content;
}

declare module '*.scss' {
  const content: any;
  export default content;
}

type Daedalus = {
    actions: ActionsMap,
    api: Api,
    environment: Object,
    reset: Function,
    stores: StoresMap,
    translations: Object,
    utils: {
      crypto: {
        generateMnemonic: Function
      }
    },
  };

//declare type EnumMap<K: string, V, O: Object = *> = O & { [K]: V & <O, K> };

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      WALLET_COUNT: number;
    }
  }
  var daedalus: Daedalus;
}

export {};
" > declarations.d.ts
    pause "create declaration file"
}

configuration_functions=(
    remove_flow_files
    remove_packages
    install_packages
    update_babelrc
    update_package_json
    update_prettierignore
    create_tsconfig
    change_webpack_configuration
    remove_flow_from_eslintrc
    add_declaration_dts
)

function convert_js_to_ts() {
    for migration_folder in ${!MIGRATION_FOLDERS[@]}
        do
            folder=${MIGRATION_FOLDERS[migration_folder]}
            (
                ls ./$folder/**/**/*.js |
                    while read line;
                    do
                        if [[ ! $line =~ webpack.config.js$ ]]; then
                            git mv -- $line ${line%.js}.ts;
                        fi
                    done
             ) & spin_while_executing
        done
    pause "convert js to ts"
}

function convert_ts_to_tsx() {
    for migration_folder in ${!MIGRATION_FOLDERS[@]}
        do
            folder=${MIGRATION_FOLDERS[migration_folder]}
            (
                find ./$folder -type f -name "*.ts" |
                    xargs grep 'import React[ ,]' |
                    cut -d: -f1 |
                    uniq |
                    while read line; do git mv -- $line ${line%.ts}.tsx; done
        ) & spin_while_executing
        done
    pause "convert ts to tsx"
}

conversion_functions=(
    convert_js_to_ts
    convert_ts_to_tsx
)

#44 CURRENTLY UNUSED
function switch_flow_maybe_type_in_function_parameters_to_typescript_optional() {
    for migration_folder in ${!MIGRATION_FOLDERS[@]}
        do
            folder=${MIGRATION_FOLDERS[migration_folder]}
            # FIND + REPLACE CLASSES WITH DEFAULT EXPORT (e.g GeneralSettingsPage.js)
            find "./$folder" -type f -name "*.ts" -exec perl -0777 -i -pe "s/(?!const(\w+))\??:\s\?(\w+[,|\)|;])/\$1?: \$2/g" {} + #& spin_while_executing
        done
    pause "flow maybe types converted to typescript optional"
}

function gulpfile_remove_flowRemoveTypes_references() {
    sed -i "/flowRemoveTypes/d" ./gulpfile.js
    pause "gulpfile remove flowRemoveTypes references"
}

function change_wallet_import_exports() {
    sed -i 's/module\.exports = {/export \{/' ./utils/api-importer/mnemonics.ts
    pause "update mnemonics export"
}

function gulpfile_change_js_to_js_references() {
    sed -i "s/.js/.ts/" ./gulpfile.js
    pause "update gulpfile"
}

function change_theme_js_references() {
    themes=(
      cardano
      dark-blue
      dark-cardano
      flight-candidate
      incentivized-testnet
      light-blue
      shelley-testnet
      white
      yellow
    )

    (
        for theme in ${!themes[@]}
        do
            # Update type definition
            sed -i "s/${themes[theme]}.js/${themes[theme]}.ts/" ./source/renderer/app/themes/types.ts
            # Update theme key name
            sed -i "s/${themes[theme]}.js/${themes[theme]}.ts/" ./source/renderer/app/themes/daedalus/index.ts
            # Update THEME_LOGGING_COLORS reference to theme key name
            sed -i "s/${themes[theme]}.js/${themes[theme]}.ts/" ./source/renderer/app/themes/utils/constants.ts

        done
    ) & spin_while_executing
    pause "change theme references"
}

function change_app_themeVars_reference() {
    sed -i "s/\${currentTheme}.js/\${currentTheme}.ts/" ./source/renderer/app/App.tsx
    pause "update App.tsx theme references"
}

function update_packager() {
    #update ./scripts/package.ts
    perl -i -pe "s/(const DEFAULT_OPTS)( = {)/\$1:any\$2/g" ./scripts/package.ts
    perl -i -pe "s/(packager\(opts),\s(cb\))/\$1).then(\$2.catch(e=>console.error(e))/g" ./scripts/package.ts
    pause "update paackager"
}

function remove_storybook_register_import() {
    perl -i -pe "s/(import '.\/addons\/DaedalusMenu\/register';)/\/\/\1/" ./storybook/addons.ts
    pause "update storybook import"
}

function replace_in_all_folders() {
    (
        for migration_folder in ${!MIGRATION_FOLDERS[@]}
            do
                folder=${MIGRATION_FOLDERS[migration_folder]}
                find ./$folder -type f -name "*.ts" -or -name "*.tsx" |
                while read line;
                do
                    # remove maybe types and use optionals
                    #perl -i -pe "s/(?!const(\w+))\??:\s\?(\w+[,|\)|;])/\$1?: \$2/g" $line
                    #lift aannotations
                    perl -0777 -i -pe "s/((@.*\n?)+)((export\sdefault\s)((class\s(\w+))(.*\n*)*))/\$1\$5\n\$4\$7/" $line
                    # FIND CLASSES WITHOUT DEFAULT EXPORT (e.g StakePoolsTableBody.js)
                    perl -0777 -i -pe "s/((@.*\n?)+)((export\s)((class\s(\w+))(.*\n*)*))/\$1\$5\n\$4\{ \$7 \}/" $line
                    # require to import
                    #perl -0777 -i -pe "s/[^ ](const|var)\s(\{?\n?((?!argv).+)\}?)\s=\srequire\(('.*')\).*;/\nimport \$2 from \$4;/g" $line
                    sed -i "/declare var daedalus: Daedalus;/d" $line
                    sed -i "/import type { Daedalus }/d" $line
                    sed -i 's/$FlowFixMe/@ts-ignore/g' $line
                done
            done
    ) & spin_while_executing
    pause "global replacements in all files"
}

function convert_flow_code() {
    sed -i '25i (() => {' ./utils/create-news-verification-hashes/index.ts
    sed -i '93i })()' ./utils/create-news-verification-hashes/index.ts

    (
        for migration_folder in ${!MIGRATION_FOLDERS[@]}
            do
                folder=${MIGRATION_FOLDERS[migration_folder]}
                npx @khanacademy/flow-to-ts --inline-utility-types --write -o tsx "./${folder}/**/*.tsx"
                npx @khanacademy/flow-to-ts --inline-utility-types --write -o ts "./${folder}/**/*.ts"
            done
    ) & spin_while_executing
    yarn prettier:format # Fixing prettier ensures ts-migrate fixes will be on the correct line
    pause "convert flow code"
}

function reignore() {
    # One folder at a time or we run out of memory
    ts-migrate migrate . --sources="./scripts/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    ts-migrate migrate . --sources="./source/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    ts-migrate migrate . --sources="./storybook/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    ts-migrate migrate . --sources="./tests/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    ts-migrate migrate . --sources="./translations/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    ts-migrate migrate . --sources="./utils/**/*.ts{,x}" --sources "node_modules/**/*.d.ts" --sources "./declarations.d.ts"
    pause "ts-migration add @ts-ignore or @ts-expect-error"
}

migration_functions=(
    gulpfile_remove_flowRemoveTypes_references
    gulpfile_change_js_to_js_references
    change_app_themeVars_reference
    change_theme_js_references
    remove_storybook_register_import
    change_wallet_import_exports
    update_packager
    replace_in_all_folders
    convert_flow_code
    reignore # Litter codebase with ts-error or ts-ignore annotations
)

function lockfile_fix() {
    yarn remove ts-migrate babel-eslint @typescript-eslint/eslint-plugin
    yarn lockfile:fix
    pause "lockfile fixed"
}

cleanup_functions=(
    lockfile_fix
)

function loop() {
    steps=("$@")
    for step in ${!steps[@]}
    do
        # Keep track of the last executed command
        STEP=$(($step+1))
        CURRENT_COMMAND=${steps[step]}
        trap 'LAST_COMMAND=${CURRENT_COMMAND^^}' DEBUG
        # Execute the next step
        ${steps[step]}
    done
}

if [ "$RUN_STAGE" != "UNSET" ];
    then
        enable_globstar
        $RUN_STAGE
    else
        echo -e "__Pre-requisite Steps__"
        loop "${prerequisite_functions[@]}"
        echo -e "\n__Configuration Steps__"
        loop "${configuration_functions[@]}"
        echo -e "\n__Conversion Steps__"
        loop "${conversion_functions[@]}"
        echo -e "\n__Migration Steps__"
        loop "${migration_functions[@]}"
        echo -e "\n__Cleanup Steps__"
        loop "${cleanup_functions[@]}"
fi
