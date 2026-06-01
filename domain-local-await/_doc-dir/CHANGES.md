# Release notes

All notable changes to this project will be documented in this file.

## 1.0.1

- Add `(implicit_transitive_deps false)` (@polytypic)
- Fix to unlock mutex even when condition wait raises (@polytypic)

## 1.0.0

- Internal improvements (@polytypic)
- Change license to ISC from 0BSD (@tarides)

## 0.2.1

- Support OCaml 4.12.0+ (@polytypic)
- Use lock-free thread-safe hash table for per thread configuration (@polytypic)

## 0.2.0

- Avoid unnecessary type alias for `(module Thread)` (@polytypic)
- Fix to update per thread configuration atomically (@polytypic)

## 0.1.0

- Initial version of scheduler independent blocking mechanism (@polytypic)
