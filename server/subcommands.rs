//! The subcommands of the 'skg' binary, which 'server/main.rs'
//! dispatches: the usage of each, and how a message names a command
//! the user can paste into a shell.
//!
//! There need be no 'skg' on the user's PATH: the binary is
//! 'target/release/skg' or 'target/debug/skg' ('bash/skg' runs
//! whichever exists). So a message that tells the user to run a
//! subcommand names the running executable by its absolute path. Not a
//! path relative to the directory the binary runs in, because the
//! message may be read elsewhere: a server's startup warnings are read
//! in a client or a log, far from the server's working directory. Each
//! word of the command is shell-quoted where it needs to be.

use std::env;
use std::iter::once;
use std::path::{Path, PathBuf};

pub struct Subcommand {
  pub name        : &'static str,
  pub arguments   : &'static str, // as the usage line shows them
  pub description : &'static str, }

pub const SUBCOMMANDS : &[Subcommand] = &[
  Subcommand {
    name        : "check-config",
    arguments   : "[CONFIG]",
    description : "\
Loads CONFIG (by default data/jeff/skgconfig.toml) as the server does
at startup, prints its warnings, and exits 1 if it is invalid." },
  Subcommand {
    name        : "check-attributions",
    arguments   : "CONFIG",
    description : "\
Prints how many attributed forks the owned skgrepos of CONFIG hold,
and what is doubtful in their attributions, each with the command
that fixes it: the warnings a server prints at startup. Needs no
server. See docs/forks.org, \"Attribution\"." },
  Subcommand {
    name        : "fix-attributions",
    arguments   : "CONFIG OLD SKGREPO",
    description : "\
Rewrites, in the notes of the owned skgrepos of CONFIG, each
attribution whose remote hash or skgrepo name is OLD, to name SKGREPO
as CONFIG loads it now: its name and its remote's hash. The original
and the commit recorded stay. Prints each file rewritten. Needs no
server. See docs/forks.org, \"Attribution\"." },
  Subcommand {
    name        : "show-forked-original",
    arguments   : "CONFIG FORK_ID",
    description : "\
Prints the file of the note that FORK_ID was forked from, as it was at
the commit FORK_ID's attribution records, read from the gitrepo of the
skgrepo the attribution names. Needs no server." },
  Subcommand {
    name        : "export-org",
    arguments   : "[CONFIG] [SKGREPO-RESTRICTION] [OUTPUT-DIR] [--include-overPrivateText-telescopes]",
    description : "\
Exports the notes marked for export to org files in OUTPUT-DIR (by
default org-exports, relative to the current directory), showing
nothing more private than SKGREPO-RESTRICTION (by default the config's
most private skgrepo). Needs no server. See docs/COMMANDS.org,
\"exporting to org\"." },
  Subcommand {
    name        : "export-obsidian",
    arguments   : "[CONFIG] [SKGREPO] [OUTPUT-DIR]",
    description : "\
Writes the owned skgrepo SKGREPO (by default the config's most public)
as an Obsidian vault in OUTPUT-DIR (by default obsidian-vault, relative
to the current directory). Needs no server. See docs/COMMANDS.org,
\"exporting a skgrepo as an Obsidian vault\"." },
  Subcommand {
    name        : "import-obsidian",
    arguments   : "VAULT-DIR OUT-DIR",
    description : "\
Reads a vault that export-obsidian wrote back into .skg files, one
folder of OUT-DIR per skgrepo folder of the vault; each must be absent
or empty. Needs no server, and touches no configured skgrepo." },
  Subcommand {
    name        : "check-obsidian-vault",
    arguments   : "VAULT-DIR",
    description : "\
Checks a vault against the rules Obsidian documents, prints each
problem, and exits 1 if it finds any." },
  Subcommand {
    name        : "import-org-roam",
    arguments   : "ORG-DIR SKG-OUTPUT-DIR SKGREPO-NAME",
    description : "\
Converts the org-roam files of ORG-DIR to .skg files in
SKG-OUTPUT-DIR, for the skgrepo SKGREPO-NAME. See docs/COMMANDS.org,
\"importing from org-roam\"." },
];

pub fn subcommand_named (
  name : &str,
) -> Option<&'static Subcommand> {
  SUBCOMMANDS . iter () . find ( |subcommand| subcommand . name == name ) }

/// Whether the arguments ask for help rather than work.
pub fn asks_for_help (
  arguments : &[String],
) -> bool {
  arguments . iter () . any ( |argument| argument == "--help" || argument == "-h" ) }

/// A subcommand's usage line and description.
pub fn usage_text (
  executable_shown : &str,
  subcommand       : &Subcommand,
) -> String {
  format! ( "Usage: {} {} {}\n\n{}\n",
            executable_shown, subcommand . name, subcommand . arguments,
            subcommand . description ) }

/// The binary's usage: the server, then each subcommand.
pub fn usage_text_of_every_form (
  executable_shown : &str,
) -> String {
  let subcommand_lines : String = SUBCOMMANDS . iter ()
    . map ( |subcommand| format! ( "  {} {} {}\n",
                                   executable_shown, subcommand . name,
                                   subcommand . arguments ))
    . collect ();
  format! ( "Usage: {} [CONFIG]\n  \
             runs the server that CONFIG (by default data/jeff/skgconfig.toml) describes.\n\
             Or one of these subcommands, each of which describes itself when given --help:\n{}",
            executable_shown, subcommand_lines ) }

/// A command line running the current executable with these
/// arguments, ready to paste into a shell from any directory.
pub fn skg_command_for_user (
  arguments : &[&str],
) -> String {
  command_line_for_user ( &running_executable_for_user (), arguments ) }

/// The current executable's absolute path, shell-quoted.
/// If it cannot be found, a bare 'skg'.
pub fn running_executable_for_user (
) -> String {
  match ( env::current_exe (), env::current_dir () ) {
    (Ok (executable), Ok (cwd)) => executable_for_user (&executable, &cwd),
    (Ok (executable), Err (_)) => shell_quoted ( &executable . to_string_lossy () ),
    (Err (_), _) => "skg" . to_string (), }}

/// An executable's path, made absolute against 'cwd' if it is not,
/// and shell-quoted.
pub fn executable_for_user (
  executable : &Path,
  cwd        : &Path,
) -> String {
  let absolute_executable : PathBuf = cwd . join (executable);
  shell_quoted ( &absolute_executable . to_string_lossy () ) }

/// The shown executable, then each argument, shell-quoted.
pub fn command_line_for_user (
  executable_shown : &str,
  arguments        : &[&str],
) -> String {
  once ( executable_shown . to_string () )
    . chain ( arguments . iter () . map ( |argument| shell_quoted (argument) ))
    . collect::<Vec<String>> () . join (" ") }

/// A word as a POSIX shell reads it back: as it is if every character
/// is one the shell takes literally, else in single quotes.
pub fn shell_quoted (
  word : &str,
) -> String {
  let taken_literally : bool =
    ! word . is_empty ()
    && word . chars () . all ( |c| c . is_ascii_alphanumeric ()
                                   || "_-./=:,+@%" . contains (c) );
  if taken_literally { word . to_string () }
  else { format! ( "'{}'", word . replace ('\'', "'\\''") ) }}

#[cfg(test)]
#[path = "../tests/unit/subcommands.rs"]
mod tests;
