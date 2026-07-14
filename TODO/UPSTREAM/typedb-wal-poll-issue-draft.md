<!--
DRAFT upstream issue for github.com/typedb/typedb — ready to paste.
Before filing, do the two verification steps flagged at the bottom
("Before I file") — confirm on 3.12.0 and check the constant/struct
names against current source. Everything else is measured. See
TODO/UPSTREAM/make-typedb-event-driven-not-polling.org and
TODO/typedb-performance-research.org for the full evidence trail.
-->

# Title

WAL committer polls at ~1 kHz per open database while idle; make it event-driven (condvar group commit)

# Body

## Summary

Each open database appears to run a WAL-sync thread that wakes about
1000 times per second on a fixed timer and does no work when the commit
queue is empty. With N databases open (a fresh server already has
`_system`, so N ≥ 2 in practice) this is ~1000·N idle timer wakeups per
second, indefinitely, with the server otherwise doing nothing.

On a plugged-in server this is invisible. On a battery-powered or
C-state-sensitive host it is a measurable idle tax: the wakeups keep the
CPU package out of deep idle states. The standard fix is a condition
variable (group commit): park when the queue is empty, wake on commit.
That gives **zero idle cost and lower commit latency** with no
durability trade-off.

This is a request to replace the fixed-interval WAL poll with an
event-driven committer. Filing it because I couldn't find an existing
issue or forum thread about idle CPU / idle wakeups — happy to be
pointed at one if it exists.

## Environment

- TypeDB CE, Linux x86-64. Observed on **3.10.3** and **3.11.5**; I have
  not re-confirmed against 3.12.0 yet (see "Before I file" below).
- Reproduces with an **empty data directory**, and with
  `--diagnostics.monitoring.enabled false`,
  `--diagnostics.reporting.metrics false`,
  `--diagnostics.reporting.errors false`, and
  `--server.http.enabled false` — so it is not diagnostics, HTTP, or
  data-dependent.
- No client connected, no queries running.

## What I observe

With the server idle, two threads account for essentially all of its
CPU; every other thread is parked in `futex`/`epoll` and the RocksDB
threads are flat at zero. Both hot threads sit in `hrtimer_nanosleep`.
There is **one such thread per open database** — with `_system` +
one user database open, exactly two.

Measured behaviour of those threads (idle server):

- **~920 voluntary context switches per second each** — i.e. a ~1 ms
  sleep loop. (I use `voluntary_ctxt_switches` from
  `/proc/<pid>/task/<tid>/status` sampled over a fixed window, because
  it is invariant to CPU clock; see the note on `%CPU` below.)
- **Independent of database size.** The two threads are statistically
  identical to three significant figures even though the two databases
  differ ~90× in on-disk size (~91 MB vs ~1 MB). So the cost tracks the
  *number of open databases*, not their contents.
- **It is genuinely idle spin, not work.** A control loop that does
  nothing but `sleep(1ms)` twice over, measured in the same window,
  reproduces the entire cost of TypeDB's two threads. There is no
  TypeDB work being done — it is the timer itself.

Wakeup rate vs. sleep interval (measured on the bare control loop, which
sets the scale of the potential saving):

| sleep interval | wakeups/s | %CPU of one core |
|----------------|-----------|------------------|
| 1 ms           | ~920      | ~2.3%            |
| 2 ms           | ~480      | ~1.3%            |
| 10 ms          | ~99       | ~0.37%           |
| 50 ms          | ~20       | ~0.12%           |

## A note on `%CPU`, to preempt "it only shows 1% here"

The idle `%CPU` this shows is close to meaningless without the core
clock, and I mention it only so the report isn't dismissed on a warm
machine. `%CPU` is billed in seconds while a timer wakeup costs a
roughly fixed number of *cycles*, so the same ~920 wakeups/s reads
anywhere from ~1% (cores warm, ~2.7 GHz) to ~15%+ (idle cores parked at
400 MHz on battery). The wakeup *rate* is the honest, clock-invariant
metric and it is flat at ~920/s/database regardless. So:

- On a busy server you may see ~1% and reasonably shrug.
- The cost that actually matters is the **wakeup count / lost C-state
  residency**, i.e. battery and package power — not the `%CPU` figure.

## Why this is worth fixing

- It is pure idle overhead: ~1000·N wakeups/s that do no work, forever,
  on every idle server.
- It scales with open databases, so a single server hosting many
  databases multiplies it.
- It defeats deep idle C-states on laptops / power-sensitive hosts.
- Raising the interval only trades the problem down (fewer but more
  expensive wakeups as the core idles deeper) and widens the durability
  window — it doesn't remove the idle cost. An event-driven committer
  removes it entirely.

## Proposal: condvar-based group commit

Replace the fixed-interval WAL sync loop with the standard
leader/follower group-commit pattern:

- The committer thread **parks on a condition variable with no timeout**
  when the commit queue is empty → zero wakeups, zero CPU, indefinitely.
- A transaction that commits appends its WAL records and **signals** the
  condvar.
- The committer wakes, **drains everything queued, issues one fsync**,
  releases all waiters batched behind it, and parks again.

Properties vs. the 1 kHz poll:

- **Idle cost: zero**, not ~1000·N wakeups/s.
- **Latency: lower** — it fsyncs the instant a commit arrives instead of
  waiting up to a full poll interval for the next tick.
- **Batching under load: equal or better** — the batch forms naturally
  from whatever arrives during the in-flight fsync (classic group
  commit), so throughput is unaffected.
- **Durability window shrinks** rather than widens.

If a bounded worst-case batch latency under sustained load is desired,
park with a timeout *only while work is already pending*; when the queue
is empty, park with no timeout. That keeps batching with still-zero idle
cost. The one piece of care needed is the usual condvar discipline
(check the queue predicate under the lock; don't lose a signal that
races the park).

The periodic statistics updater (~50 ms) is the same idea with a twist —
statistics usually want *debouncing*, not firing per commit. Ideal is
edge-triggered-then-debounced: a commit arms a one-shot timer if none is
armed; it fires once, recomputes, disarms. Idle: no timer.

I'm happy to help with a PR if the direction is welcome.

## Reproduction

1. Start a server on an empty data directory with diagnostics and HTTP
   disabled (flags above). Connect once to create/open a database, then
   disconnect (or don't — an idle `_system`-only server already shows
   one such thread).
2. Find the server PID; for each thread, sample
   `/proc/<pid>/task/<tid>/status` `voluntary_ctxt_switches` twice, ~10 s
   apart. The WAL threads show a delta of ~9200 over 10 s (~920/s); all
   other threads are ~flat.
3. Confirm it isn't work: a two-line program that just loops
   `sleep(1ms)` twice, sampled the same way, matches the two threads'
   cost.

## Before I file (self-check, not part of the issue)

- [ ] **Confirm on 3.12.0** (current latest, released 2026-07-06). My
      traces are from 3.10.3 / 3.11.5.
- [ ] **Verify the constant / struct names against current source.**
      From the earlier trace these looked like
      `WAL_SYNC_INTERVAL_MICROSECONDS` (~1000) and a statistics-update
      interval (~50 ms), plausibly in a `resource/constants.rs`, but
      that came from a prebuilt binary, not a source read — GitHub code
      search needs auth, which I haven't done. Name them precisely (or
      drop the specific names and describe the behaviour) before filing.
- [ ] Search open+closed issues once more for a dup at file time.
