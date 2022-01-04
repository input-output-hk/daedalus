# MIGRATION TO TYPESCRIPT SHELL SCRIPT

: '
This file is designed to assist in the migration of flow to typescript
This script pauses after completion of each step so that incremental changes can be commited
In order to maintain git histories, you MUST commit after the convert to TS/TSX step

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
WEBPACK_VERSION=UNSET
# FOLDERS TO MIGRATE
MIGRATION_FOLDERS=(
    # scripts
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

function detectWebpackVersion() {
    WEBPACK_VERSION=$( webpack -v | grep "webpack:" | sed --regexp-extended 's/webpack: (4|5).*/\1/')
}

detectWebpackVersion

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

# PRE-REQUISITE FUNCTIONS
function enable_globstar() {
    shopt -s globstar # ALLOWS RECURSION GLOBBING (NOT ALWAYS ENABLED BY DEFAULT IN TERMINAL)
    pause "enabled globstar"
}

function remove_node_modules() {
    #rm -rf node_modules & spin_while_executing # START FROM FRESH TO AVOID PACKAGE CONFLICTS
    pause "remove node modules"
}

function temporarily_remove_husky_pre_commit() {
    sed -i "/pre-commit/d" ./package.json
    pause "temporarily removed husky pre-commit"
}

prerequisite_functions=(
    enable_globstar
    # remove_node_modules
    temporarily_remove_husky_pre_commit
)


# CONFIGURATION FUNCTIONS
function remove_flow_files() {
    rm -f ./.flowconfig & rm -rf ./flow #& spin_while_executing
    pause "remove flow files + config"
}

function remove_packages() {
    yarn lockfile:fix
    #grep -wq './package.json' -e '@babel/preset-flow' && yarn remove @babel/preset-flow
    #yarn lockfile:fix
    #grep -wq './package.json' -e 'eslint-plugin-flowtype' && yarn remove eslint-plugin-flowtype
    #yarn lockfile:fix
    #grep -wq './package.json' -e 'flow-bin' && yarn remove flow-bin
    #yarn lockfile:fix
    #grep -wq './package.json' -e 'gulp-flow-remove-types' && yarn remove gulp-flow-remove-types
    #yarn lockfile:fix
    #grep -wq './package.json' -e 'babel-eslint' && yarn remove babel-eslint
    yarn remove @babel/preset-flow eslint-plugin-flowtype flow-bin gulp-flow-remove-types babel-eslint
    yarn lockfile:fix


    pause "un-install flow packages"
}

function install_packages() {
    yarn install
    # INSTALL TS PACKAGES
    yarn add -D ts-node ts-loader@8.2.0 @babel/plugin-transform-typescript @babel/preset-typescript @types/react @types/aes-js @types/qrcode.react @types/react-svg-inline @types/node ts-migrate babel-eslint @typescript-eslint/eslint-plugin typescript @typescript-eslint/parser
    # Might need @types/react-dom @types/electron
    pause "install required packages"
}

function update_babelrc() {
    sed -i 's/flow/typescript/' ./.babelrc
    pause "update babel configuration"
}

function update_package_json() {
    sed -i 's/"flow:test": "flow; test $? -eq 0 -o $? -eq 2"/"compile": "tsc --noEmit"/' ./package.json
    sed -i 's/flow:test/compile/' ./package.json
    sed -i "s/\(\"lint\".*\)\(\*.js\)/\1 --ext .ts,.tsx/g" ./package.json
    perl -i -pe "s/(importer|index|reporter|common|translation-runner|copyTheme|e2e)\.js/\1.ts/g" ./package.json
    sed -i "s/\*.js/*.ts/g" ./package.json
    sed -i "s/\/main\/index.ts/\/main\/index.js/" ./package.json
    sed -i "s/\&\& node/\&\& ts-node/g" ./package.json
    sed -i "s/\"node /\"ts-node /g" ./package.json
    sed -i "s/\^//g" ./package.json
    pause "update package.json"
}

function update_prettierignore() {
    sed -i "14i !*.ts\\n!*.tsx" ./.prettierignore
    pause "update prettier ignore"
}

function create_tsconfig() {
    tsc --init \
        --target es2019 \
        --jsx react \
        --sourceMap \
        --lib dom \
        --strict false \
        --noImplicitAny false \
        --noImplicitThis false \
        --moduleResolution node \
        --allowSyntheticDefaultImports \
        --esModuleInterop \
        --forceConsistentCasingInFileNames \
        --resolveJsonModule \
        --experimentalDecorators \
        --emitDecoratorMetadata \
        --useDefineForClassFields \
        --noEmitOnError \
        --noFallthroughCasesInSwitch

    sed -i "101i,\"exclude\": [\"node_modules\"]" ./tsconfig.json #Speeds up compilation
    pause '.tsconfig generation'
}

function change_webpack_configuration() {
    # substitutions
    MODULE_FIND='(module: {)'
    MODULE_ADD_RESOLVE="resolve: {\n\t\textensions: \[\'.tsx\', \'.ts\', \'.js\', \'.json\'],\n\t},"

    if [ "$WEBPACK_VERSION" == "5" ];
        then
            LOADER_FIND="cacheDirectory: true,"
            LOADER_REPLACE="cacheDirectory: true,\n\t\t\t\t\t\t\tpresets: \[\n\t\t\t\t\t\t\t\t\'\@babel\/preset-env\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-react\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-typescript\',\n\t\t\t\t\t\t\t\],"
        else
            LOADER_FIND="'babel-loader'"
            LOADER_REPLACE="\n\t\t\t\t\t{\n\t\t\t\t\t\tloader: \'babel-loader\',\n\t\t\t\t\t\toptions: {\n\t\t\t\t\t\t\tpresets: \[\n\t\t\t\t\t\t\t\t\'\@babel\/preset-env\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-react\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-typescript\',\n\t\t\t\t\t\t\t\],\n\t\t\t\t\t\t},\n\t\t\t\t\t},\n\t\t\t\t"
    fi

    # source/main
    perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./source/main/webpack.config.js
    perl -i -pe "s/$LOADER_FIND/${LOADER_REPLACE}/" ./source/main/webpack.config.js
    sed -i "s/\(index\|preload\)\.js/\1.ts/g" ./source/main/webpack.config.js
    sed -i "s/jsx/tsx/g" ./source/main/webpack.config.js
    sed -i "s/\.js/.ts/g" ./source/main/config.js
    # source/renderer
    perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./source/renderer/webpack.config.js
    perl -i -pe "s/$LOADER_FIND/${LOADER_REPLACE}/" ./source/renderer/webpack.config.js
    sed -i "s/\(renderer\/index\).js/\1.ts/g" ./source/renderer/webpack.config.js
    sed -i "s/jsx/tsx/g" ./source/renderer/webpack.config.js

    if [ "$WEBPACK_VERSION" == "5" ];
        then
            perl -i -pe "s/$LOADER_FIND/${LOADER_REPLACE}/" ./storybook/main.js
        else
            LOADER_REPLACE_STORYBOOK="{\n\t\t\t\t\t\ttest: \/\.tsx?\\$\/,\n\t\t\t\t\t\tloader: \'babel-loader\',\n\t\t\t\t\t\toptions: {\n\t\t\t\t\t\t\tpresets: \[\n\t\t\t\t\t\t\t\t\'\@babel\/preset-env\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-react\',\n\t\t\t\t\t\t\t\t\'\@babel\/preset-typescript\',\n\t\t\t\t\t\t\t\],\n\t\t\t\t\t\t},\n\t\t\t\t\t}"
            perl -i -pe "s/$MODULE_FIND/${MODULE_ADD_RESOLVE}\n\t\1/g" ./storybook/webpack.config.js
            perl -i -pe "s/(jsxRule,\n)/\1\t\t\t\t${LOADER_REPLACE_STORYBOOK},/g" ./storybook/webpack.config.js
    fi
    # storybook
    pause "change_webpack_configuration"
}

function update_eslintrc() {
    sed -i "s/flowtype/@typescript-eslint/g" ./.eslintrc
    sed -i "s/\@babel\/eslint-parser/@typescript-eslint\/parser/" ./.eslintrc
    # These are the additional rules we need to prevent lint stage failing, update rules to use warn flag, so we don't forget about them

    sed -i "74i\    \"react/jsx-first-prop-new-line\": [1, \"multiline-multiprop\"],\n    \"@typescript-eslint/ban-ts-comment\": 1,\n    \"@typescript-eslint/no-empty-function\": 1,\n    \"@typescript-eslint/ban-types\": 1,\n    \"import/no-unresolved\": 1,\n    \"@typescript-eslint/no-var-requires\": 1,\n    \"camelcase\": 1,\n\t\t\"no-empty\": 1,\n    \"@typescript-eslint/no-explicit-any\": 1,\n    \"no-shadow\": 1,\n    \"react/no-did-update-set-state\": 1," ./.eslintrc
    
    sed -i "/import\/resolver/d" ./.eslintrc
    # Line location webpack4 105
    # Line location webpack5 106
    if [ "$WEBPACK_VERSION" == "5" ];
        then
            sed -i "106i\     \"import/resolver\": {\n      \"node\": {\n        \"extensions\": [\".js\", \".jsx\",\".ts\", \".tsx\"]\n      }\n    }" ./.eslintrc
        else
            sed -i "105i\     \"import/resolver\": {\n      \"node\": {\n        \"extensions\": [\".js\", \".jsx\",\".ts\", \".tsx\"]\n      }\n    }" ./.eslintrc
    fi
    pause "update eslintrc"
}

function add_declaration_dts() {
    touch declaration.d.ts
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
" > declaration.d.ts
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
    update_eslintrc
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

function gulpfile_remove_flowRemoveTypes_references() {
    sed -i "/flowRemoveTypes/d" ./gulpfile.js
    # May need to check here if we need to compile to ts
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

function update_DelegationStepsConfirmationDialog() {
    sed -i "32i import { ReactIntlMessage } from '../../../types/i18nTypes';" ./source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog.tsx
    sed -i "s/\(const messages\)\( = {\)/\1: Record<string, ReactIntlMessage>\2/g" ./source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog.tsx
    pause "update type for intl message in DelegationStepsConfirmationDialog"
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
                    #lift annotations
                    perl -0777 -i -pe "s/((@.*\n?)+)((export\sdefault\s)((class\s(\w+))(.*\n*)*))/\$1\$5\n\$4\$7/" $line
                    # FIND CLASSES WITHOUT DEFAULT EXPORT (e.g StakePoolsTableBody.js)
                    perl -0777 -i -pe "s/((@.*\n?)+)((export\s)((class\s(\w+))(.*\n*)*))/\$1\$5\n\$4\{ \$7 \}/" $line
                    # require to import
                    perl -0777 -i -pe "s/[^\s](const|var)\s(\{?\n?((?!argv).+)\}?)\s=\srequire\(('.*')\).*;/\nimport * as \$2 from \$4;/g" $line
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
                # Does npx run on CI?
                npx @khanacademy/flow-to-ts --inline-utility-types --write -o tsx "./${folder}/**/*.tsx"
                npx @khanacademy/flow-to-ts --inline-utility-types --write -o ts "./${folder}/**/*.ts"
            done
    ) & spin_while_executing
    yarn prettier:format # Fixing prettier ensures ts-migrate fixes will be on the correct line
    pause "convert flow code"
}

function reignore() {
    # Create Node script
    touch migrate.ts
    echo "import path from 'path';
import { tsIgnorePlugin, eslintFixPlugin } from 'ts-migrate-plugins';
import { migrate, MigrateConfig } from 'ts-migrate-server';

// get input files folder
const inputDir = path.resolve(__dirname);

// create new migration config and add ts-ignore plugin with options
const config = new MigrateConfig()
  .addPlugin(eslintFixPlugin, { fix: true, useEslintrc: true })
  .addPlugin(tsIgnorePlugin, {
    useTsIgnore: true,
  });
// run migration
(async () => {
  const exitCode = await migrate({
    rootDir: inputDir,
    tsConfigDir: path.resolve(__dirname),
    config,
    sources: [
      './scripts/**/*.ts{,x}',
      './source/**/*.ts{,x}',
      './storybook/**/*.ts{,x}',
      './tests/**/*.ts{,x}',
      './translations/**/*.ts{,x}',
      './utils/**/*.ts{,x}',
      './declarations.d.ts',
    ],
  });
  process.exit(exitCode);
})();
" > migrate.ts
    # Need to use node script because CLI doesn't expose options
    ts-node ./migrate.ts & spin_while_executing
    pause "ts-migration add @ts-ignore"
}

function update_translation_runner() {
    perl -0777 -i -pe "s/const(.*) = require\(('.*')\).*;/import\$1 from \$2;/g" ./translations/translation-runner.ts
    pause "update translation runner"
}

function update_api_importer_functions() {
    (
        find ./utils/api-importer -type f -name "*.ts" -or -name "*.tsx" |
        while read line;
        do
            perl -0777 -i -pe "s/const(.*) = require\(('.*')\);/import\$1 from \$2;/g" $line
            perl -0777 -i -pe "s/((async )function\s?)([^\.])([\w|,|\s|-|_|\\$]*)(.+?\{)((.|\n)*)\n\}\n\n((function\s?)([^\.])([\w|,|\s|-|_|\\$]*)(.+?\{)((.|\n)*)\n\})\n\nmain\(\);/(\$2() => {\n\$8\$6\n})();/" $line
        done
    ) & spin_while_executing
    pause "update api importer functions"
}

function update_ts_ignore_annotations() {
    (
        for migration_folder in ${!MIGRATION_FOLDERS[@]}
            do
                folder=${MIGRATION_FOLDERS[migration_folder]}
                find ./$folder -type f -name "*.tsx" |
                while read line;
                do
                    perl -0777 -i -pe "s/([^A-z]<?\/?.*[^=]>)([\n|\r]\s*)(\/\/( \@ts-ignore.*))/\$1\$2\{\/*\$4 *\/\}/g" $line
                done
            done
    ) & spin_while_executing
    pause "update @ts-ignore annotations in components"
}

function ts-ignore-TransferFunds() {
    sed -i "36i\ \t\t\t// @ts-ignore" ./storybook/stories/wallets/legacyWallets/TransferFunds.stories.tsx
    pause "update @ts-ignore annotations in TransferFunds.stories.tsx"
}

migration_functions=(
    gulpfile_remove_flowRemoveTypes_references
    gulpfile_change_js_to_js_references
    change_app_themeVars_reference
    change_theme_js_references
    remove_storybook_register_import
    change_wallet_import_exports
    update_DelegationStepsConfirmationDialog
    replace_in_all_folders
    update_translation_runner
    update_api_importer_functions
    convert_flow_code
    reignore # Litter codebase with ts-error or ts-ignore annotations
    update_ts_ignore_annotations
    ts-ignore-TransferFunds
)

function tsc_check() {
    tsc --noEmit
    pause "tsc check"
}

function destroy_migrate_ts() {
    rm ./migrate.ts
    pause "destroyed migrate ts file"
}

function lockfile_fix() {
    sed -i "s/\^//g" ./package.json # Pin all versions
    yarn lockfile:fix
    yarn remove ts-migrate
    yarn lockfile:fix
    #yarn
    yarn prettier:format & spin_while_executing # Needs a final run
    yarn lockfile:fix
    pause "lockfile fixed"
}

# Check if check:all function still passes
function check_all() {
    yarn check:all
    echo "✅ check:all success! the project is not broken"
}

function reenable_husky_pre_commit() {
    #webpack4 276
    #webpack5 293
    sed -i "293i\      \"pre-commit\": \"pretty-quick --staged\"," ./package.json
    pause "re-enabled husky pre-commit"
}

function update_nix_files() {
    sed -i "s/flow/tsc/g" default.nix
    sed -i "s/Flow/Tsc/g" default.nix
    mv ./tests/flow.nix ./tests/tsc.nix
    sed -i "s/flow/tsc/" ./tests/tsc.nix
    sed -i "61d" yarn2nix.nix
    sed -i "211,216d" yarn2nix.nix
    pause "updated nix files"
}

function update_bors_toml() {
    sed -i "s/Flow/Tsc/g" bors.toml
    pause "Updated bors toml"
}

cleanup_functions=(
    update_nix_files
    update_bors_toml
    reenable_husky_pre_commit
    destroy_migrate_ts # We no longer need the ts-node migration script
    tsc_check
    lockfile_fix
    check_all
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
        # Keep track of the last executed command
        STEP=$(($step+1))
        CURRENT_COMMAND=$RUN_STAGE
        trap 'LAST_COMMAND=${CURRENT_COMMAND^^}' DEBUG
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
