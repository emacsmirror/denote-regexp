<!--
SPDX-FileCopyrightText: Copyright (C) 2025 Samuel W. Flint <swflint@samuelwflint.com>

SPDX-License-Identifier: GFDL-1.3-or-later
-->

# Denote Regexp [![Not Yet on MELPA](https://melpa.org/packages/denote-regexp-badge.svg)](https://melpa.org/#/denote-regexp) [![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/denote-regexp)](https://api.reuse.software/info/git.sr.ht/~swflint/denote-regexp)


This library provides two user- or developer-facing functions, `denote-regexp` and `denote-regexp-rx`.
The function `denote-regexp` uses `denote-regexp-rx` under the hood, and this may be helpful to developers primarily.
In either case, they generate some representation of a regular expression to match Denote files, based on `denote-file-name-components-order` and user-provided input.
Each of them take the following arguments:

 - `:identifier` a (partial) identifier string.
   If the length of the string is less than 15, it will be expanded with the `any` character class so that it fits 15 characters.
   Additionally, if `identifier` is not the first element of `denote-file-name-components-order`, the "@@" prefix will be added.
 - `:signature` a Denote file signature.
 This will be prefixed with "==".
 If a string, it will be sluggified with `denote-sluggify`; if a regular expression (i.e., a list), it will be passed through otherwise.
  - `:title` a note's title.
  This will be prefixed with "--".
  If a string, it will be sluggified with `denote-sluggify`; if a regular expression (i.e., a list), it will be passed through otherwise.
 - `:keywords` keywords for a note.
   This will be prefixed with "__".
   The format of this argument is as follows:
    - If it is a string, it well be passed to `denote-sluggify`.
    - If it is a list, starting with `or` or `:or`, it will be treated as a regular expression disjunction (`rx`s `or`), and the remainder of the list will be processed recursively.
    - If it is otherwise a list, (optionally starting with `and` or `:and`), it will be treated as a regular expression sequence.
      If all remaining elements of the list are strings, it will be sorted following `denote-sort-keywords`, otherwise, all - elements will be processed recursively.
 - `:file-type` will match known file types.
   This should be a symbol or list of symbols representing file types which are part of `denote-file-types`.
- `:directory` will match the subdirectory of a note.
   This should be a string or a regexp.

Finally, a `denote` construct for `rx` is available as well, which follows the same arguments as above.

## Examples

 - To match a file with the keywords "project" and "inprogress", use:
```elisp
(denote-regexp :keywords '("project" "inprogress"))
```

 - To match a file whose identifier starts with "2024", use:
```elisp
(denote-regexp :identifier "2024")
```

 - To match a file with the (unsluggified) signature `"soloway85:_from_probl_progr_plans"`, use:
```elisp
(denote-regexp :signature "soloway85:_from_probl_progr_plans")
```

 - To match a file with either the keyword "agenda" or both "project" and "inprogress", use:
```elisp
(denote-regexp :keywords '(or "agenda" (and "project" "inprogress")))
```

 - To match denote-journal-extras files from May of 2023, use:
```elisp
(denote-regexp :signature "202305" :keywords denote-journal-extras-keyword)
```

 - To match in-progress projects as part of a denote-links block:
 ```org
#+BEGIN: denote-links :regexp (denote :keywords '("project" "inprogress"))
#+END:
```

## Errors and Patches

If you find an error, or have a patch to improve this package, please send an email to `~swflint/emacs-utilities@lists.sr.ht`.
