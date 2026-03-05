# Theme Management Skill

> **Purpose**: Add, update, validate and verify theme files using built-in package.json commands and utilities.
>
> **Target Users**: Frontend developers managing Daedalus theme CSS variables and styling system.
>
> **Related Components**: 
> - `source/renderer/app/themes/` — Theme definitions and utilities
> - `source/renderer/app/themes/daedalus/` — Theme output files (cardano.ts, light-blue.ts, etc.)
> - `source/renderer/app/themes/utils/` — Theme creation and validation scripts
> - `gulpfile.js` — Build tasks for themes

---

## Overview

Daedalus uses a CSS custom properties (CSS variables) theme system built with TypeScript. All themes are generated from a centralized `createTheme` object and validated against existing theme files to ensure consistency across the application.

### Key Theme Files

| Path | Purpose |
|------|---------|
| `source/renderer/app/themes/daedalus/index.ts` | Theme output definitions (CARDANO_THEME_OUTPUT, LIGHT_BLUE_THEME_OUTPUT, etc.) |
| `source/renderer/app/themes/daedalus/cardano.ts` | Dark theme (primary Cardano theme) |
| `source/renderer/app/themes/daedalus/light-blue.ts` | Light theme variant |
| `source/renderer/app/themes/daedalus/dark-cardano.ts` | Dark variant of Cardano theme |
| `source/renderer/app/themes/utils/constants.ts` | `createTheme` object and theme creation parameters |
| `source/renderer/app/themes/utils/checkCreateTheme.ts` | Validation logic for theme consistency |

---

## Core Commands

All commands use Gulp build system and TypeScript compilation before execution.

### 1. Validate Theme Creation (`themes:check:createTheme`)

**Purpose**: Validate that the `createTheme` object contains all CSS custom properties defined in existing theme files.

**Command**:
```bash
yarn themes:check:createTheme
```

**What It Does**:
- Builds themes via `gulp build:themes`
- Compiles and runs `source/renderer/app/themes/scripts/check.ts`
- Compares `createTheme` object against all existing theme outputs
- Logs missing definitions in each theme with color-coded output
- **Exit Code**: 0 if valid, 1 if missing definitions found

**Output Example**:
```
createTheme.js is missing the following definitions that exist in the cardano.ts theme:

{
  "--theme-wallet-address-copy-tooltip-background-color": "#2cbb69",
  "--theme-wallet-address-copy-tooltip-text-color": "#fafafa"
}
```

**When to Use**:
- After adding new CSS variables to an existing theme
- Before committing theme changes to verify consistency
- When implementing new UI components that need theme variables

**Note**: This command does NOT modify files; it only reports differences.

---

### 2. Update Theme Files (`themes:update`)

**Purpose**: Update theme files based on changes in the `createTheme` object, then auto-format with Prettier.

**Command**:
```bash
yarn themes:update
```

**What It Does**:
- Builds themes via `gulp build:themes`
- Runs `source/renderer/app/themes/utils/updateThemesCLI.ts` (interactive CLI)
- Presents available updates to existing theme files
- Applies selected updates to theme files
- Auto-formats all updated files with Prettier (`--loglevel warn`)
- Modifies files in: `source/renderer/app/themes/daedalus/*.ts`

**Interactive CLI Usage**:
1. Command prompts you to select which theme(s) to update
2. Displays summary of changes to be applied
3. Confirms before writing changes to disk

**Output Example**:
```
Updating cardano.ts...
✓ Added 3 new CSS variables
✓ Updated 2 existing variables
✓ Formatted with Prettier
```

**When to Use**:
- After adding new CSS variables to `createTheme` object
- When propagating color/style changes across all themes
- Part of the standard workflow when defining new theme variables

**Result**: Updated TypeScript theme files with auto-formatting applied.

---

### 3. Copy Theme Properties (`themes:copy`)

**Purpose**: Interactive CLI tool to quickly duplicate CSS properties between all theme files.

**Command**:
```bash
yarn themes:copy
```

**What It Does**:
- Runs `source/renderer/app/themes/utils/copyTheme.ts` (interactive CLI)
- Launches CLI interface to select source and target CSS variables
- Copies property value from one theme to all other theme files
- Auto-formats updated files with Prettier

**Interactive CLI Steps**:
1. **Select Property**: Prompts for CSS variable name (e.g., `--theme-button-primary-color`)
2. **Select Source Theme**: Choose which theme to copy value FROM
3. **Select Target Themes**: Choose which theme(s) to copy value TO
4. **Confirm**: Review changes before applying

**Usage Example**:
```
? Which CSS variable do you want to copy?
> --theme-button-primary-background

? Copy from which theme?
> cardano.ts

? Copy to which themes? (select multiple with Space)
> [x] light-blue.ts
> [ ] dark-cardano.ts
> [x] light-cardano.ts

✓ Applied to 2 themes
✓ Running Prettier...
```

**When to Use**:
- When adding a new theme and need to copy color values from existing theme
- After creating new CSS variables and need to propagate to multiple themes
- When updating a color value that should be consistent across themes
- Quick reference for comparing CSS variable values across themes

**Result**: All selected theme files updated with copied property; auto-formatted.

---

### 4. Build Themes (`gulp build:themes`)

**Purpose**: Internal build step that compiles theme TypeScript files for use by check/update/copy commands.

**Command**:
```bash
gulp build:themes
```

**What It Does**:
- Cleans `dist/` directory
- Runs Webpack to compile theme files
- Outputs to `dist/`

**When It Runs**: Automatically executed before `themes:check:createTheme`, `themes:update`, and `themes:copy`.

**Note**: You typically don't need to run this directly; it's a dependency of other theme commands.

---

## Workflow Examples

### Example 1: Adding a New Theme Variable

**Scenario**: You need to add a new CSS variable `--theme-mithril-card-background` for the Mithril feature.

**Steps**:
1. **Add to `createTheme` object**:
   ```typescript
   // source/renderer/app/themes/utils/constants.ts
   export const CREATE_CARDANO_THEME_PARAMS = {
     // ... existing vars
     'mithril-card-background': {
       light: '#ffffff',
       dark: '#0f1822',
     },
   };
   ```

2. **Validate**:
   ```bash
   yarn themes:check:createTheme
   ```
   - If valid output: ✓ Proceed to step 3
   - If missing definitions: Add missing variables to `createTheme`

3. **Update theme files**:
   ```bash
   yarn themes:update
   ```
   - Select themes to update (usually all)
   - Confirm changes
   - Prettier auto-formats output

4. **Verify**:
   ```bash
   yarn prettier:check
   yarn compile
   ```

### Example 2: Copying a Property to a New Theme

**Scenario**: You created a new theme variant and need to copy all color values from the Cardano theme.

**Steps**:
1. Create new theme file: `source/renderer/app/themes/daedalus/my-theme.ts`
2. Export output constant in `source/renderer/app/themes/daedalus/index.ts`:
   ```typescript
   export const MY_THEME_OUTPUT = { /* ... */ };
   export const EXISTING_THEME_OUTPUTS = [
     // ... existing
     ['my-theme.ts', MY_THEME_OUTPUT],
   ];
   ```

3. **Copy properties iteratively**:
   ```bash
   yarn themes:copy
   ```
   - Select property from cardano.ts
   - Apply to my-theme.ts
   - Repeat for each variable needed

4. **Or update in batch** (if `createTheme` supports new theme):
   ```bash
   yarn themes:update
   ```
   - Select my-theme.ts
   - Apply all updates at once

### Example 3: Verifying Theme Consistency Before Commit

**Scenario**: You've made changes to theme files and want to ensure everything is consistent.

**Steps**:
```bash
# 1. Check createTheme validity
yarn themes:check:createTheme

# 2. Run full code quality checks
yarn compile        # TypeScript check
yarn prettier:check # Formatting check
yarn lint           # ESLint check
```

---

## Theme File Structure

### Theme Output Object Format

Each theme file exports a TypeScript object mapping CSS variable names to color values:

```typescript
// source/renderer/app/themes/daedalus/cardano.ts
export const CARDANO_THEME_OUTPUT = {
  '--theme-about-window-background-color': 'rgba(15, 24, 34, 0.96)',
  '--theme-button-primary-background': '#2cbb69',
  '--theme-button-primary-text-color': '#ffffff',
  // ... 200+ more CSS variables
};
```

### Theme Parameters Format

The `createTheme` object uses parameter format:

```typescript
// source/renderer/app/themes/utils/constants.ts
export const CREATE_CARDANO_THEME_PARAMS = {
  'property-name': {
    light: '#color-for-light-theme',
    dark: '#color-for-dark-theme',
  },
  // ...
};
```

---

## Validation and Quality Checks

### Pre-Commit Checklist

Before committing theme changes:

```bash
# 1. Validate theme creation object
yarn themes:check:createTheme

# 2. Verify TypeScript compilation
yarn compile

# 3. Check code formatting
yarn prettier:check

# 4. Run full checks
yarn lint
```

### Automated Checks (CI/CD)

Theme changes are validated in CI via:
- `yarn check:all` — Runs all quality checks including theme validation
- Theme files must have consistent formatting
- No missing CSS variables between `createTheme` and existing themes

---

## Common Patterns

### Adding a Feature's Theme Variables

1. Create variables in `createTheme` with both light and dark values
2. Run `yarn themes:check:createTheme` to validate
3. Run `yarn themes:update` to sync to all theme files
4. Verify with `yarn prettier:check && yarn compile`
5. Use variables in SCSS: `color: var(--theme-feature-color);`

### Creating a New Theme Variant

1. Create new theme file in `source/renderer/app/themes/daedalus/`
2. Add export to `source/renderer/app/themes/daedalus/index.ts`
3. Add to `EXISTING_THEME_OUTPUTS` array
4. Run `yarn themes:copy` or `yarn themes:update` to populate
5. Register in theme selection UI

### Updating Color Values Across All Themes

1. Modify `createTheme` parameters in `constants.ts`
2. Run `yarn themes:update` and select all themes
3. Review changes in generated `.ts` files
4. Commit diff alongside any component changes

---

## Tips & Best Practices

### Do's ✓
- Run `themes:check:createTheme` after any theme-related changes
- Use `themes:copy` for one-off variable propagation
- Use `themes:update` for bulk changes affecting multiple files
- Always run `prettier:check` after theme updates (auto-run by update command)
- Keep CSS variable names consistent (kebab-case, semantic prefix)

### Don'ts ✗
- Don't manually edit theme `.ts` files if template exists (use tools instead)
- Don't commit unformatted theme files (Prettier is required)
- Don't add CSS variables only to one theme (use createTheme for all)
- Don't skip validation before committing (run `themes:check:createTheme`)

### Color Value Guidelines

- Use hex format for consistency: `#2cbb69`
- Use rgba for transparency: `rgba(255, 255, 255, 0.8)`
- Follow naming pattern: `--theme-[feature]-[element]-[property]`
  - Example: `--theme-wallet-address-copy-tooltip-background-color`
- Light theme variants should use lighter/brighter colors
- Dark theme variants should use darker/lower-contrast colors

---

## Troubleshooting

### "Missing definitions" Error

**Problem**: `yarn themes:check:createTheme` reports missing definitions

**Solution**:
1. Add missing variables to `CREATE_*_THEME_PARAMS` in `constants.ts`
2. Ensure both `light` and `dark` values are provided
3. Run `yarn themes:check:createTheme` again to verify

### Prettier Formatting Issues

**Problem**: Theme files aren't formatted after update

**Solution**:
- Prettier is auto-run by `themes:update` command
- If formatting is missing, run: `yarn prettier --loglevel warn --write source/renderer/app/themes/daedalus/*.ts`

### TypeScript Compilation Errors

**Problem**: `yarn compile` fails after theme changes

**Solution**:
1. Verify all new variables are exported in `daedalus/index.ts`
2. Check `createTheme` object matches theme file structure
3. Run `yarn themes:update` to auto-sync files

### CLI Tool Not Responding

**Problem**: `themes:copy` or `themes:update` hangs

**Solution**:
- Ensure `gulp build:themes` completed successfully
- Check `dist/` directory exists after build
- Try clearing cache: `yarn clear:cache && yarn themes:update`

---

## Related Documentation

- **Theme Architecture**: [System Architecture - Theme System](./../../../.agent/system/architecture.md)
- **Component Development**: [Storybook Workflow](./../../../.agent/workflows/storybook.md)
- **Best Practices**: [BESTPRACTICES.md](./../../../BESTPRACTICES.md)
- **Theme Tutorial Series**: [Theme Tutorial Videos](https://www.youtube.com/playlist?list=PLhYa9Lr0WjdXN69m4B2pVyuP9UlAT1Xw_)

---

## Video Tutorials

The Daedalus project includes comprehensive video tutorials on theme management:

- **Part 1**: Creating a new theme using `createTheme` utility
- **Part 2**: Using the `themes:update` script
- **Part 3**: Writing theme CSS variables to output objects
- **Part 4**: Using `themes:check:createTheme` for validation

See [Theme README](./../../renderer/app/themes/README.md) for video links.

---

## Quick Reference Card

| Task | Command | Interactive? | Modifies Files? |
|------|---------|--------------|-----------------|
| Validate themes | `yarn themes:check:createTheme` | No | No |
| Update all themes | `yarn themes:update` | Yes | Yes |
| Copy one variable | `yarn themes:copy` | Yes | Yes |
| Build themes | `gulp build:themes` | No | Generated only |
| Check formatting | `yarn prettier:check` | No | No |
| Auto-format themes | `yarn prettier --write source/renderer/app/themes/daedalus/*.ts` | No | Yes |

---

## For Copilot Agents

When working with theme files:

1. **New theme variable**: Add to `createTheme` → validate → update files → verify
2. **Theme property copy**: Use `themes:copy` CLI for single operations
3. **Batch theme updates**: Use `themes:update` CLI for multiple variables
4. **Before commits**: Run `themes:check:createTheme && yarn compile && yarn prettier:check`
5. **Modify theme colors**: Edit `constants.ts` then sync with `themes:update`

Always ensure theme files remain in sync with validation tools and auto-formatted.
