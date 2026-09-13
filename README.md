# worm-jovial

JOVIAL WORM SECRET — hand-rolled dense implementation.

Write Once / Consume Key+Plain / Sealed Cipher.

Flow: `SECRET -> WORM ENCRYPT (PLAIN+KEY CONSUMED) -> SEALED CT -> SINGLE DECRYPT`

Classic JOVIAL style (J73/J3 compatible subset). Educational only.

## State machine

```
EMPTY -> WRITEP -> SUPKEY -> ENCRYPT (PLAIN+KEY zeroed) -> SEALED -> DECRYPT -> CONSUMED
```

Result codes: `OK ALREADY BADSTATE TOOSMALL SHORTKEY BADTAG NILL LOCKED`

## Demos

20 demos covering: happy path, double-write reject, no-key reject, double-decrypt reject,
labelled sealed, multi-round stress, large payload, zero-after-consume, state machine walk,
parallel independent WORMs, CAN-* predicates, 8-round stress, label roundtrip, tiny payload,
post-destroy safety, and 3 self-tests.

## Build

Requires a JOVIAL J73 or J3 compiler (JOVIAL Systems, Inc. or equivalent aerospace toolchain).
The source stays within the classic subset and is also readable as a formal specification.

## License

FSL-1.1 — converts to Apache 2.0 two years after initial distribution.
