"""Run one command and measure it: wall time, CPU time, cores used,
peak memory of its whole process group, and the machine's lowest
available memory meanwhile.  Kills the run if MemAvailable < 1 GiB.

Usage: python3 measure_run.py LABEL TIMELINE.tsv -- CMD ARGS...
Prints one summary line, and appends it to TIMELINE.tsv's '.summary'.
To stand in for a small machine, put 'taskset -c 8-11' (four of this
laptop's efficiency cores) before CMD, and set CARGO_TARGET_DIR to a
scratch folder to build cold without touching target/.

The group is CMD's session (it starts in a new one), so daemons it
starts in another session are missed.  CPU time is the reaped
children's rusage, exact; 'peak cores' is sampled every 0.5 s and
misses processes shorter than that (nextest's), so trust 'avg cores'.
RSS is summed per process, so shared pages count more than once.
"""
import os, sys, time, signal, subprocess, resource

label, timeline = sys.argv[1], sys.argv[2]
cmd = sys.argv[sys.argv.index("--") + 1:]
PAGE = os.sysconf("SC_PAGE_SIZE")
TCK = os.sysconf("SC_CLK_TCK")
FLOOR_KB = 1024 * 1024

def avail_kb():
    with open("/proc/meminfo") as f:
        for l in f:
            if l.startswith("MemAvailable:"):
                return int(l.split()[1])

def group(sid):
    rss = 0; ticks = {}
    for p in os.listdir("/proc"):
        if not p.isdigit(): continue
        try:
            with open(f"/proc/{p}/stat") as f: s = f.read()
            rest = s[s.rindex(")") + 2:].split()
            if int(rest[3]) != sid: continue   # field 6: session
            ticks[p] = int(rest[11]) + int(rest[12])
            with open(f"/proc/{p}/statm") as f: rss += int(f.read().split()[1]) * PAGE
        except (FileNotFoundError, ProcessLookupError, ValueError, IndexError):
            pass
    return rss, ticks

t0 = time.time()
proc = subprocess.Popen(cmd, start_new_session=True)
sid = proc.pid
peak_rss = 0; min_avail = avail_kb(); peak_cores = 0.0; killed = False
last_ticks = {}; last_t = t0; window = []
with open(timeline, "w") as tl:
    tl.write("t\trss_mb\tcores\tavail_mb\n")
    while proc.poll() is None:
        time.sleep(0.5)
        now = time.time()
        rss, ticks = group(sid)
        d = sum(max(0, v - last_ticks.get(p, v if p in last_ticks else 0)) for p, v in ticks.items())
        # a process first seen now contributes all its ticks so far; fine for short-lived rustc
        cores = d / TCK / (now - last_t)
        last_ticks, last_t = ticks, now
        window = (window + [cores])[-4:]
        peak_cores = max(peak_cores, sum(window) / len(window))
        a = avail_kb()
        peak_rss = max(peak_rss, rss); min_avail = min(min_avail, a)
        tl.write(f"{now - t0:.1f}\t{rss / 2**20:.0f}\t{cores:.2f}\t{a / 1024:.0f}\n"); tl.flush()
        if a < FLOOR_KB and not killed:
            killed = True
            os.killpg(sid, signal.SIGTERM); time.sleep(3)
            try: os.killpg(sid, signal.SIGKILL)
            except ProcessLookupError: pass
rc = proc.wait()
wall = time.time() - t0
ru = resource.getrusage(resource.RUSAGE_CHILDREN)
cpu = ru.ru_utime + ru.ru_stime
line = (f"{label}\twall {wall:.0f}s\tcpu {cpu:.0f}s\tavg cores {cpu / wall:.1f}\t"
        f"peak cores(2s) {peak_cores:.1f}\tpeak group RSS {peak_rss / 2**30:.2f} GiB\t"
        f"min avail {min_avail / 2**20:.1f} GiB\texit {rc}{'  KILLED at memory floor' if killed else ''}")
print(line)
with open(timeline + ".summary", "a") as f: f.write(line + "\n")
