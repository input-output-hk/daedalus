#!/bin/bash

find source/renderer/react-polymorph -name "*.js" | while read file; do
    mv "$file" "${file%.js}.tsx"  # Rename .js to .tsx
    flow-to-ts --inplace "$file"  # Convert Flow types to TypeScript
done

