#!/usr/bin/env python3
"""PURPOSE: test that 'pacer.sh run' refuses to start while another
pacer lives, or while a session a killed pacer started still runs, and
that a pid file left by a pacer that is gone stops nothing.

Run from anywhere: 'python3 tools/team/test_pacer.py'. It spends
nothing and touches nothing real: it copies the pacer into a temporary
folder, whose 'tools/team/' and 'bash/' hold stand-ins for the scripts
the pacer calls, and puts a fake 'claude', which only sleeps, first on
PATH. The pacer finds its root from its own path, so its folder, log
and pid file are all in the temporary folder.
"""
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest

TEAM_FOLDER = os . path . dirname ( os . path . abspath (__file__) )

# How long any wait here lasts before it fails. A wait returns as soon
# as its condition holds, which takes well under a second on an idle
# machine. But 'bash/all-tests.sh' runs this at nice 15, and when other
# suites keep every core busy at a higher priority, each of the dozen
# short processes a pacer starts before its session (python3, date,
# ps, ...) can wait seconds for a core. With three busy loops sharing
# one core, the first test took 18 seconds, though nothing was wrong.
# So the limit is generous; it is reached only if the pacer is stuck.
PATIENCE_SECONDS = 120

USAGE_STAND_IN = """#!/usr/bin/env bash
now=$(date +%s)
echo "{\\"session_5h\\": {\\"used_frac\\": 0.0, \\"reset_epoch\\": $((now + 3600))},"
echo " \\"weekly_7d\\": {\\"used_frac\\": 0.0, \\"reset_epoch\\": $((now + 300000))}}"
"""

CHECK_STAND_IN = """import sys
if '--list' in sys.argv:
  open (sys.argv [sys.argv.index ('--list') + 1], 'w') . close ()
"""

# Says which session it is and its pid, then sleeps as long as a session might.
CLAUDE_STAND_IN = """#!/usr/bin/env bash
echo "$PACER_SESSION $$" >> "$FAKE_CLAUDE_STARTS"
exec sleep 300
"""

def read (path):
  with open (path, encoding = "utf-8") as f:
    return f . read ()

def write (path, text, mode = 0o644):
  os . makedirs ( os . path . dirname (path), exist_ok = True )
  with open (path, "w", encoding = "utf-8") as f:
    f . write (text)
  os . chmod (path, mode)

class PacerTest (unittest . TestCase):

  def setUp (self):
    self . root = tempfile . mkdtemp ()
    self . pacer = os . path . join (self . root, "tools", "team", "pacer.sh")
    os . makedirs ( os . path . dirname (self . pacer) )
    shutil . copy ( os . path . join (TEAM_FOLDER, "pacer.sh"), self . pacer )
    write ( os . path . join (self . root, "tools", "team", "check_jeffs_files.py"),
            CHECK_STAND_IN )
    write ( os . path . join (self . root, "tools", "team", "spending_account.py"), "" )
    write ( os . path . join (self . root, "bash", "claude-usage.sh"),
            USAGE_STAND_IN, 0o755 )
    write ( os . path . join (self . root, "bin", "claude"), CLAUDE_STAND_IN, 0o755 )
    self . dir = os . path . join (self . root, "tools", "team-processing", "pacer")
    os . makedirs (self . dir)
    write ( os . path . join (self . dir, "ALLOW"), "0.5\n" ) # so the pace says yes
    self . starts = os . path . join (self . root, "claude-starts")
    self . env = dict ( os . environ,
                        PATH = os . path . join (self . root, "bin") + ":" + os . environ ["PATH"],
                        PACER_DIR = self . dir,
                        MAIL_FILE = os . path . join (self . root, "mail"),
                        FAKE_CLAUDE_STARTS = self . starts,
                        POLL_SECONDS = "600" )
    self . env . pop ("PACER_SESSION", None)
    self . groups = []

  def tearDown (self):
    self . end_sessions ()
    for process in self . groups:
      try: os . killpg (process . pid, signal . SIGKILL)
      except ProcessLookupError: pass
      process . wait (timeout = PATIENCE_SECONDS)
    shutil . rmtree (self . root, ignore_errors = True)

  def start_pacer (self, output = subprocess . DEVNULL):
    """Starts 'pacer.sh run' in its own process group, as 'setsid' would."""
    process = subprocess . Popen ( [ "bash", self . pacer, "run" ], cwd = self . root,
                                   env = self . env, stdin = subprocess . DEVNULL,
                                   stdout = output, stderr = subprocess . STDOUT,
                                   start_new_session = True, text = True )
    self . groups . append (process)
    return process

  def log (self):
    path = os . path . join (self . dir, "pacer.log")
    return read (path) if os . path . exists (path) else ""

  def sessions_started (self):
    """The stamp and the fake claude's pid of each session started."""
    if not os . path . exists (self . starts): return []
    return [ line . split () for line in read (self . starts) . splitlines () ]

  def end_sessions (self):
    """Ends every fake claude. Killing a pacer's process group would not:
    'timeout' puts itself and the session in a group of their own."""
    for _, pid in self . sessions_started ():
      try: os . kill ( int (pid), signal . SIGKILL )
      except ProcessLookupError: pass

  def wait_for (self, condition, what, seconds = PATIENCE_SECONDS):
    deadline = time . time () + seconds
    while time . time () < deadline:
      if condition (): return
      time . sleep (0.1)
    self . fail ( "waited %d seconds for %s; the log:\n%s" % (seconds, what, self . log ()) )

  def assert_refused (self, naming):
    second = self . start_pacer (subprocess . PIPE)
    started = "pacer started (pid %d)" % second . pid
    self . wait_for (lambda: second . poll () is not None or started in self . log (),
                     "the second pacer to refuse")
    if started in self . log (): # it would run on; say so now, not when a wait gives up
      self . fail ( "the second pacer started instead of refusing; the log:\n" + self . log () )
    output, _ = second . communicate (timeout = PATIENCE_SECONDS)
    self . assertEqual (second . returncode, 1, output)
    self . assertIn ("not starting: " + naming, output)
    self . assertIn ("a pacer (pid %d) refused to start: %s" % (second . pid, naming), self . log ())

  def test_a_second_pacer_refuses_while_the_first_lives (self):
    first = self . start_pacer ()
    self . wait_for (lambda: len (self . sessions_started ()) == 1, "the first pacer's session")
    self . assert_refused ("the pacer (pid %d, started " % first . pid)
    self . assertIsNone (first . poll ())
    self . assertEqual (len (self . sessions_started ()), 1)
    self . assertEqual (self . log () . count ("pacer started"), 1)

  def test_a_session_whose_pacer_was_killed_keeps_a_new_pacer_from_starting (self):
    """What happened on 2026-11-15: the pacer was killed just after it
    began a session, and a new one was started by hand."""
    first = self . start_pacer ()
    self . wait_for (lambda: len (self . sessions_started ()) == 1, "the first pacer's session")
    stamp = self . sessions_started () [0] [0]
    os . kill (first . pid, signal . SIGTERM)
    first . wait (timeout = PATIENCE_SECONDS)
    subshell = int ( read ( os . path . join (self . dir, "running-session") ) . split () [0] )
    self . assert_refused ("session %s (pid %d), which no living pacer watches," % (stamp, subshell))
    self . assertEqual (len (self . sessions_started ()), 1)
    # Once that session ends, a pacer starts, though the dead pacer's pid file remains.
    self . end_sessions ()
    self . wait_for (lambda: not os . path . exists ("/proc/%d" % subshell),
                     "the orphaned session to end")
    self . assertTrue ( os . path . exists ( os . path . join (self . dir, "pid") ) )
    third = self . start_pacer ()
    self . wait_for (lambda: len (self . sessions_started ()) == 2, "the third pacer's session")
    self . assertIsNone (third . poll ())

  def test_a_pid_file_naming_a_process_that_is_not_a_pacer_stops_nothing (self):
    gone = subprocess . run ( [ "sh", "-c", "echo $$" ], capture_output = True, text = True )
    gone_pid = int ( gone . stdout )
    for pid in [ gone_pid, os . getpid () ]: # one dead, one alive but no pacer
      write ( os . path . join (self . dir, "pid"), "%d\n" % pid )
      pacer = self . start_pacer ()
      self . wait_for (lambda: "pacer started (pid %d)" % pacer . pid in self . log (),
                       "a pacer to start over a pid file naming %d" % pid)
      self . assertEqual ( read ( os . path . join (self . dir, "pid") ) . strip (),
                           str (pacer . pid) )
      # Its session's subshell dies with it, so its running-session file stops nothing.
      os . killpg (pacer . pid, signal . SIGKILL)
      pacer . wait (timeout = PATIENCE_SECONDS)
      self . end_sessions ()

if __name__ == "__main__":
  unittest . main ()
