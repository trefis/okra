#!/bin/sh

# Sed to:
#  - use "-" for all bulletpoints, replace + or *
#  - replace double space with tabs
#  - if leading single space " -", assume root level OKR entry and remove the space
#  - if double space after "-", remove one space
sed "s/\* /- /g" | \
sed "s/+ /- /g" | \
sed "s/\t/  /g" | \
sed "s/^ -/- /g" | \
sed "s/-  /- /g"
