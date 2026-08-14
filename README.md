# Témoin - OCaml Litmus Tests

Litmus tests for OCaml's memory model, written against `Atomic` rather than
assembly, so what gets tested is what the compiler and runtime actually deliver
on the machine you run it on. I've been making this to help with some of my Atomic work on the OCaml compiler

Not related to `litmus7` and `herd7`, which test hardware and are the serious
tools in this space. 


```
dune build && ./_build/default/bin/main.exe -n 1000000
```

`-only NAME` runs a subset.

## The tests

| | what it checks |
|---|---|
| MP | a value written before an atomic flag is visible to whoever reads the flag |
| MP publication | the same without atomics, which OCaml's fences should still hold |
| SB | store buffering, forbidden for atomics |
| **SB control** | store buffering with plain accesses, which the hardware may legally do |
| LB | load buffering |
| CoRR | once you have seen the new value you cannot see the old one again |
| WRC | causality across three domains |
| IRIW | four domains, whether everyone agrees on the order of two independent writes |
| CAS | exactly one winner |
| FAA | two increments, two distinct old values |
| XCHG | exactly one domain sees the initial value |

## The control matters

A run where nothing fails proves nothing on its own. It might mean the model
holds, or it might mean the harness cannot see a violation.

**SB control** exists to settle that. It is built from racy plain accesses in a
shape no fence orders, so the hardware is allowed to produce the forbidden
outcome, and it firing is the good result. If it does not fire, the run says so
and you should not trust the clean results above it.

## Getting the domains to collide

Two domains started normally almost never interleave interestingly. The runner
spawns them once and drives every iteration through a pair of spin barriers,
with a per-domain jitter before the critical section so the alignment shifts
from one iteration to the next. Outcomes that require genuine overlap should
appear in the counts; if they do not, the run is not testing what you think.

## My findings so far

| | amd64 | Power10 | SpacemiT X60 | z15 |
|---|---|---|---|---|
| cores | 12 | 192 | 8 | 2 |
| SB control | fires | 29,387 | 11 | does not fire |
| WRC, IRIW | ok | ok | ok | skipped |
| everything else | ok | ok | ok | ok |

Nothing has produced a forbidden outcome through `Atomic` on any machine.

Plain accesses on POWER are not multi-copy atomic, so two readers can disagree
about the order of two independent writes. A million iterations of IRIW across
four domains on a 192-core Power10 produced no disagreement.

The z15 is strongly ordered enough that the control never fires, so that run
cannot detect anything and says so rather than reporting success.

MP publication holds without atomics because OCaml emits `lwsync` before every
plain store to mutable heap state on POWER. That orders store against store but
not store against load, which is why MP cannot fail and SB can.

## License

Apache 2.0
