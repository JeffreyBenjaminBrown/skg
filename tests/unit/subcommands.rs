use super::*;

#[test]
fn a_relative_executable_is_shown_absolute_against_the_working_directory () {
  assert_eq! ( executable_for_user ( Path::new ("target/release/skg"),
                                     Path::new ("/home/u/skg") ),
               "/home/u/skg/target/release/skg" );
  assert_eq! ( executable_for_user ( Path::new ("target/debug/skg"),
                                     Path::new ("/home/u/my notes") ),
               "'/home/u/my notes/target/debug/skg'",
               "a path with a space is quoted" ); }

#[test]
fn an_absolute_executable_is_shown_as_it_is () {
  assert_eq! ( executable_for_user ( Path::new ("/opt/skg/target/release/skg"),
                                     Path::new ("/somewhere/else") ),
               "/opt/skg/target/release/skg" ); }

#[test]
fn each_argument_is_quoted_only_where_the_shell_needs_it () {
  assert_eq! ( command_line_for_user (
                 "/opt/skg/target/release/skg",
                 &[ "fix-attributions", "/home/u/data/jeff/skgconfig.toml",
                    "claude-public", "NAME" ] ),
               "/opt/skg/target/release/skg fix-attributions /home/u/data/jeff/skgconfig.toml claude-public NAME" );
  assert_eq! ( shell_quoted ("Ed's notes"), "'Ed'\\''s notes'" );
  assert_eq! ( shell_quoted (""), "''" );
  assert_eq! ( shell_quoted ("$HOME"), "'$HOME'" ); }

#[test]
fn the_running_executable_is_named_absolutely () {
  let shown : String = running_executable_for_user ();
  assert! ( shown . starts_with ('/') || shown . starts_with ("'/"), "{}", shown );
  assert! ( skg_command_for_user (&[ "check-attributions", "C" ])
            . ends_with (" check-attributions C") ); }

#[test]
fn fix_attributions_describes_its_usage () {
  let subcommand : &Subcommand = subcommand_named ("fix-attributions") . unwrap ();
  let usage : String = usage_text ("/opt/skg/target/release/skg", subcommand);
  assert! ( usage . starts_with (
              "Usage: /opt/skg/target/release/skg fix-attributions CONFIG OLD SKGREPO\n\n\
               Rewrites, in the notes of the owned skgrepos of CONFIG, each\n" ),
            "{}", usage ); }

#[test]
fn the_usage_of_every_form_lists_each_subcommand () {
  let usage : String = usage_text_of_every_form ("skg");
  assert! ( usage . starts_with ("Usage: skg [CONFIG]\n"), "{}", usage );
  for subcommand in SUBCOMMANDS {
    assert! ( usage . contains ( &format! ( "  skg {} {}\n", subcommand . name,
                                             subcommand . arguments )),
              "{} is missing from\n{}", subcommand . name, usage ); }}

#[test]
fn help_is_asked_for_anywhere_among_the_arguments () {
  let arguments = |words : &[&str]| -> Vec<String> {
    words . iter () . map ( |word| word . to_string () ) . collect () };
  assert! ( asks_for_help ( &arguments (&[ "--help" ]) ));
  assert! ( asks_for_help ( &arguments (&[ "CONFIG", "-h" ]) ));
  assert! ( ! asks_for_help ( &arguments (&[ "CONFIG", "OLD", "NEW" ]) )); }
