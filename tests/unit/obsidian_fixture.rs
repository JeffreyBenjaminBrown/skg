//! A skgrepo, as sections, holding each case the Obsidian export must
//! carry: shared children, cycles (one no root reaches), a fork with an
//! attribution, hides, aliases, flags, extra IDs, an anchor, a member
//! and links outside the skgrepo, an ID that is no block ID, a titleless
//! section, odd titles (links, slashes, colons, list markers, length,
//! duplicates, case-only differences, leading blanks, backticks) and
//! bodies with Org and Markdown markup, code, and text the escapes
//! cannot carry. Shared by the export and import tests.

use crate::telescope::types::ListItem;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::attribution::Attribution;
use crate::types::nodes::complete::Flag;
use crate::types::nodes::fs::GraphnodeOnDisk;

/// A UUID-shaped ID whose first eight hex digits are 'n'.
pub fn id (
  n : u32,
) -> ID {
  ID::from ( format! ("{:08x}-0000-4000-8000-{:012x}", n, n) ) }

pub const ROOT        : u32 = 1;
pub const FIRST       : u32 = 2;
pub const GRANDCHILD  : u32 = 3;
pub const SECOND      : u32 = 4;
pub const SHARED      : u32 = 5;
pub const OTHER_DOC   : u32 = 6;
pub const FORKER      : u32 = 7;
pub const FORKER_KID  : u32 = 8;
pub const CYCLE_ONE   : u32 = 9;
pub const CYCLE_TWO   : u32 = 10;
pub const BY_EXTRA    : u32 = 11;
pub const OUTSIDE     : u32 = 900; // in no section
pub const EXTRA_ID    : &str = "extra-id-of-the-third";
pub const WEIRD_ID    : &str = "Weird\u{201c}ID";

pub fn section (
  n     : u32,
  title : &str,
) -> GraphnodeOnDisk {
  GraphnodeOnDisk {
    title       : Some (title . to_string ()),
    aliases     : Vec::new (),
    pid         : id (n),
    extra_ids   : Vec::new (),
    body        : None,
    contains    : Vec::new (),
    forks       : Vec::new (),
    hides       : Vec::new (),
    flags       : Vec::new (),
    attribution : None, }}

fn members (
  ns : &[u32],
) -> Vec<ListItem> {
  ns . iter () . map ( |n| ListItem::Member (id (*n)) ) . collect () }

pub fn fixture_sections () -> Vec<GraphnodeOnDisk> {
  let link = |n : u32, label : &str| -> String {
    format! ("[[id:{}][{}]]", id (n) . as_str (), label) };
  let mut out : Vec<GraphnodeOnDisk> = Vec::new ();
  { let mut s : GraphnodeOnDisk = section (ROOT, "Threads");
    s . aliases = vec! [ "threads" . to_string () ];
    s . body = Some ( [
      "# an Org comment line, not a heading".to_string (),
      "Prices: $5, 50%% off, x^2 and a trailing ^word".to_string (),
      format! ("A #tag, [[https://example.com][a web link]], and {}.", link (FIRST, "the first child")),
      "- [ ] a checkbox item in the body".to_string (),
      format! ("\\{} and !{}", link (SECOND, "after a backslash"), link (SECOND, "after a bang")),
      "#+begin_src sh".to_string (),
      format! ("echo {}", link (FIRST, "in a block")),
      "#+end_src".to_string (),
      "```text".to_string (),
      "- x ^notanid".to_string (),
      format! ("{}", link (FIRST, "in a fence")),
      "```".to_string (),
      format! ("=verbatim {}=", link (FIRST, "an example")),
      format! ("An extra ID: [[id:{}][extra]]; outside: {}; a bar: {}.",
               EXTRA_ID, link (OUTSIDE, "gone"), link (FIRST, "a|b")),
      "  indented, with a tab\tinside".to_string (),
      "".to_string (),
      "after a blank line, %% and [[not a link]] and ![[nor an embed]]".to_string (),
    ] . join ("\n") );
    s . contains = vec! [
      ListItem::Member (id (FIRST)), ListItem::Member (id (SECOND)),
      ListItem::Member (id (SHARED)),
      ListItem::Anchor { anchor : id (OUTSIDE + 1) },
      ListItem::Member (id (OUTSIDE)),
      ListItem::Member (ID::from (EXTRA_ID)),
      ListItem::Member (ID::from (WEIRD_ID)) ];
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (FIRST, "First child");
    s . body = Some ("Its body.\n\nTwo paragraphs." . to_string ());
    s . contains = members (&[GRANDCHILD]);
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (GRANDCHILD, "A grandchild");
    s . flags = vec! [ Flag::Had_ID_Before_Import, Flag::Was_Overloaded ];
    s . contains = members (&[FIRST]); // a cycle back to its parent
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (SECOND, & format! ("Second child, with a {}", link (FIRST, "link")));
    s . aliases = vec! [ "x" . to_string (), "y: z" . to_string () ];
    s . flags = vec! [ Flag::NoSearchMatching ];
    s . hides = vec! [ id (OUTSIDE + 2) ];
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (SHARED, "Shared note");
    s . body = Some ("Shared body." . to_string ());
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (OTHER_DOC, "Other document");
    s . contains = members (&[SHARED, FORKER]);
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (FORKER, "Forker");
    s . forks = vec! [ ListItem::Member (id (OUTSIDE + 3)), ListItem::Member (id (SHARED)),
                       ListItem::Anchor { anchor : id (SHARED) } ];
    s . attribution = Some ( Attribution {
      original    : id (OUTSIDE + 3),
      skgrepo     : SkgrepoName::from ("elsewhere"),
      remote_hash : Some ("abc123" . to_string ()),
      commit      : Some ("def456" . to_string ()),
      uncommitted : true, } );
    s . contains = members (&[FORKER_KID]);
    out . push (s); }
  out . push (section (FORKER_KID, "Forker's child"));
  { let mut s : GraphnodeOnDisk = section (CYCLE_ONE, "Cycle one");
    s . contains = members (&[CYCLE_TWO]);
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (CYCLE_TWO, "Cycle two");
    s . contains = members (&[CYCLE_ONE]);
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (BY_EXTRA, "Third, listed by an extra ID");
    s . extra_ids = vec! [ ID::from (EXTRA_ID) ];
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (0, "A weird ID");
    s . pid = ID::from (WEIRD_ID);
    s . body = Some ( format! ("Links to itself: [[id:{}][me]].", WEIRD_ID) );
    out . push (s); }
  let odd_titles : Vec<String> = vec! [
    "a/b: c? <d> *e* | f #g ^h [i] \"q\" 100%".to_string (),
    "word " . repeat (60) . trim_end () . to_string (),
    "see also".to_string (), "see also".to_string (),
    "Case".to_string (), "case".to_string (),
    format! ("{} notes", link (FIRST, "First")),
    "1. Intro".to_string (), "- dash".to_string (), "> quote".to_string (),
    "  leading blanks".to_string (),
    "back`tick` title".to_string (),
    "```fence in a title".to_string (),
    "<div> in a title".to_string (),
    ".hidden-looking".to_string (),
    "ends with a backslash \\".to_string (),
    "%%skg section%%".to_string (),
    "tab\tinside".to_string (),
  ];
  for (i, title) in odd_titles . iter () . enumerate () {
    out . push (section (100 + i as u32, title)); }
  { let mut s : GraphnodeOnDisk = section (200, "unused");
    s . title = None; // a section whose home is elsewhere
    s . aliases = vec! [ "added here" . to_string () ];
    s . contains = members (&[201]);
    out . push (s); }
  out . push (section (201, "Child of a titleless section"));
  { let mut s : GraphnodeOnDisk = section (202, "Body with a carriage return");
    s . body = Some ("line one\r\nline two" . to_string ());
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (203, "Body with leading blank lines");
    s . body = Some ("\n\nafter blanks\n" . to_string ());
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (204, "Markdown-ish body");
    s . body = Some ( [
      "```", "unclosed fence", "<!-- an unclosed comment", "<div>",
      "- item ^looks-like-an-id", "    - deeper", "| a | b |", "|---|---|",
      "$$", "x", "$$", "==highlight==", "trailing spaces  " ] . join ("\n") );
    out . push (s); }
  { let mut s : GraphnodeOnDisk = section (205, "Body that is only a fence");
    s . body = Some ("````md\n```\ninner\n```\n````" . to_string ());
    out . push (s); }
  out }
