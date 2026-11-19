//! Attribution: what a fork records of the note it was forked from.
//!
//! The fork operation writes it into the clone's home section, once,
//! when the clone is made ('build_fork_clone' in
//! [[../../from_text/fork.rs]]). It is never derived from the 'forks'
//! relation at render time, so it outlives the original's deletion, a
//! nodeMerge, and the original's skgrepo not being loaded. The herald
//! reads it ('docs/heralds.org', "Attribution"); 'skg-strip-attribution'
//! removes it. See 'docs/forks.org', "Attribution".

use serde::{Serialize, Deserialize};

use crate::types::misc::{ID, SkgrepoName};

/// Field order fixes YAML output order.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Attribution {
  /// The note forked: the pid it had when forked.
  pub original : ID,

  /// The original's home skgrepo, by the name the forker's config gave
  /// it then. A name is only a local alias, so it is the fallback: the
  /// herald shows it, marked unverified, only where no loaded skgrepo
  /// has the recorded remote.
  pub skgrepo : SkgrepoName,

  /// The git blob hash of the normalised URL of that skgrepo's git
  /// remote ('remote_hash_of_url' in [[../../attribution.rs]]): the
  /// skgrepo's identity, independent of anyone's config. Hashed, so a
  /// private remote's URL never appears in a public note. None when
  /// the skgrepo had no remote.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub remote_hash : Option<String>,

  /// The HEAD commit of the gitrepo holding that skgrepo when the
  /// fork was made, so the original's text then can be read back
  /// ('skg show-forked-original'). None when it was no gitrepo.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub commit : Option<String>,

  /// True when the original's home file differed from that commit when
  /// the fork was made: the commit then lacks the text forked, which
  /// only the fork's own first version holds.
  #[serde(default, skip_serializing_if = "std::ops::Not::not")]
  pub uncommitted : bool,
}
