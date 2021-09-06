OKR report aggregation tool
=======

Prototype aggregation tool for markdown-based OKR reports. This is work in progress.

## Running it

The code reads data from stdin, aggregates per KR and outputs the result.

Example:

```
# cat okr-report1.md okr-report2.md | okra cat | less
```

If there are warnings that are not expected (i.e not about the "OKR updates" section), they should be manually resolved. There's also a script that handles some common formatting issues by fixing indents, bulletpoints etc (fmt.sh). To use it:

```
# cat okr-report1.md okr-report2.md | ./fmt.sh | okra cat | less
```

Several options are available for modifying the output format, see `okra cat --help` for details. Sections can optionally be ignored and removed from the final output ("OKR updates" is ignored by default).

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

Exceptions with warnings will be raised if the tool is unable to locate the engineer time in a section. This can be because it doesn't follow the defined format (e.g. it is valid, but something else) or it can have accidental formatting errors. You should always resolve warnings to make sure that the returned result is correct.

Some issues that cause warnings:

  - Sections/Headlines that contain other data, such as OKR updates. This is usually fine, and if the section is given the heading "OKR updates" it will automatically be ignored from the warnings.
  - Inconsistent bulletpoint formatting. If switching between different bulletpoint markers (such as +, - and asterisk) in the same list, the parser `omd` can become confused. Fix this by always using `-`.
  - Inconsistent indent. If the indent for bulletpoints is only a space (instead of two or a tab) the parse can be confused. Similarly if it switches between tabs and spaces.
  - The OKR line is not followed by a line of engineer-time, with each name prefixed by '@' -- sometimes the '@' is forgotten.

## Okra Configuration File

You can store a `conf.yaml` file in `~/.okra` to provide the binary with extra information to help writing weekly reports, aggregating across directories etc. You don't need a configuration file, there is a default one (but it isn't particularly useful).

```yaml
# Projects are used in weekly report generation (optional)
projects:
  - "Make okra great (Plat123)"
# Locations will be used in aggregation across multiple directories (optional)
locations:
  - "/path/to/admin/dir1"
  - "/path/to/admin/dir2"
```
