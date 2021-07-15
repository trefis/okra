OKR report aggregation tool
=======

Prototype aggregation tool for markdown-based OKR reports. This is work in progress.

## Running it

The code reads data from stdin, aggregates per KR and outputs the result.

Example:

```
# cat okr-report1.md okr-report2.md | okra | less
```

If there are warnings that are not expected (i.e not about the "OKR updates" section), they should be manually resolved. There's also a script that handles some common formatting issues by fixing indents, bulletpoints etc (fmt.sh). To use it:


```
# cat okr-report1.md okr-report2.md | ./fmt.sh | okra | less
```

## Report format

The expected report format:

```
# Project

## Objective

- My KR (KR ID)
  - @engineer1 (1 day), @engineer2 (2 days)
  - work item 1
    - subitem
  - work item 2
```

The tool will attempt to group data by project, objective and KR if these match.

## Fixing warnings

Warnings will be shown for sections that are unparsable for some reason. This can be because it doesn't follow the defined format (e.g. it is valid, but something else) or it can have accidental formatting errors. You should always check all warnings to make sure that the returned result is correct.

Some issues that cause warnings:

  - Sections/Headlines that contain other data, such as OKR updates. This is usually fine, and if the section is given the heading "OKR updates" it will automatically be ignored from the warnings.
  - Inconsistent bulletpoint formatting. If switching between different bulletpoint markers (such as +, - and asterisk) in the same list, the parser `omd` can become confused. Fix this by always using `-`.
  - Inconsistent indent. If the indent for bulletpoints is only a space (instead of two or a tab) the parse can be confused. Similarly if it switches between tabs and spaces.
  - The OKR line is not followed by a line of engineer-time, with each name prefixed by '@' -- sometimes the '@' is forgotten.
