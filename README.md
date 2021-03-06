## About

`manglediff.pl` is a Perl script written to help dealing with (large) [diff](https://en.wikipedia.org/wiki/Diff) files (`.diff` / `.patch`) - "dealing" means "remove non-interesting parts from file(s)".

**Nota bene**: here and later, "diff" means "diff in [unified format](https://en.wikipedia.org/wiki/Diff#Unified_format)".

## Prerequisites / installation

`manglediff.pl` has no extra dependencies. E.g. for Debian GNU/Linux the only required package is "perl-base" which is already essential for any Debian-based distribution.

However, `Text::Glob` ([metacpan](https://metacpan.org/pod/Text::Glob)) Perl module was roughly imported to work with glob patterns, but I'm looking to replace it with own implementation because original module is missing regex anchors while converting glob to regex.

## Usage

`manglediff.pl [options] file ...`

### Options:

#### Select file names with [glob](https://en.wikipedia.org/wiki/Glob_(programming)) patterns:

| Option      | Long option         | Description                                            |
| ----------- | ------------------- | ------------------------------------------------------ |
| `-iPATTERN` | `--include=PATTERN` | include diffs which file names match pattern `PATTERN` |
|             |                     |                                                        |
| `-xPATTERN` | `--exclude=PATTERN` | exclude diffs which file names match pattern `PATTERN` |

#### Select file names with [regular expression](https://en.wikipedia.org/wiki/Regular_expression#Perl_and_PCRE) patterns:

| Option      | Long option               | Description                                            |
| ----------- | ------------------------- | ------------------------------------------------------ |
| `-IPATTERN` | `--include-regex=PATTERN` | include diffs which file names match pattern `PATTERN` |
|             |                           |                                                        |
| `-XPATTERN` | `--exclude-regex=PATTERN` | exclude diffs which file names match pattern `PATTERN` |

#### File name stripping:

| Option     | Long option      | Description                                                   |
| ---------- | ---------------- | ------------------------------------------------------------- |
| `-sLENGTH` | `--strip=LENGTH` | strip (at most) LENGTH leading path components from file name |
|            |                  |                                                               |
| `-n`       | `--no-strip`     | don't strip file names                                        |

By default, file name stripping length is `1`.

#### Select by meta information:

| Option | Long option        | Description                                                |
| ------ | ------------------ | ---------------------------------------------------------- |
| `-C`   | `--strip-comments` | remove comments (non-diff part of file)                    |
|        |                    |                                                            |
| `-N`   | `--strip-new`      | remove diffs which create files                            |
|        |                    |                                                            |
| `-D`   | `--strip-deleted`  | remove diffs which delete files                            |
|        |                    |                                                            |
| `-R`   | `--strip-renames`  | remove diffs which rename files                            |
|        |                    |                                                            |
| `-U`   | `--strip-regular`  | remove _regular_ diffs which changes only contents of file |

#### Output handling:

| Option | Long option  | Description                                |
| ------ | ------------ | ------------------------------------------ |
| `-Z`   | `--in-place` | replace (input) file with _mangled_ output |

By default, result is written to new file.

E.g. for input file `input.diff` output file will be named `input.diff.new.%Y%m%d-%H%M%S`
where last name part is simply current date/time.

#### Debugging:

| Option | Long option | Description            |
| ------ | ----------- | ---------------------- |
| `-d`   | `--debug`   | print debugging output |

## License

BSD 3-Clause
- [spdx.org](https://spdx.org/licenses/BSD-3-Clause.html)
- [opensource.org](https://opensource.org/licenses/BSD-3-Clause)
