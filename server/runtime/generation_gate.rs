use crate::types::store_state::GraphGeneration;

use std::collections::BTreeMap;
use std::sync::{Arc, Condvar, Mutex};

#[derive(Debug)]
struct GateState {
  current_generation : GraphGeneration,
  selecting          : bool,
  exclusive          : bool,
  queryable          : bool,
  unqueryable_reason : Option<String>,
  readers            : BTreeMap<GraphGeneration, usize>,
}

#[derive(Debug)]
struct GenerationGateInner {
  state   : Mutex<GateState>,
  changed : Condvar,
}

#[derive(Clone, Debug)]
pub struct GenerationGate {
  inner : Arc<GenerationGateInner>,
}

#[derive(Debug)]
pub struct QueryLease {
  inner      : Arc<GenerationGateInner>,
  generation : GraphGeneration,
  released   : bool,
}

#[derive(Debug)]
pub struct SelectionLease {
  inner               : Arc<GenerationGateInner>,
  expected_generation : GraphGeneration,
  finished            : bool,
}

impl GenerationGate {
  pub fn new (generation : GraphGeneration) -> Self {
    Self {
      inner: Arc::new (GenerationGateInner {
        state: Mutex::new (GateState {
          current_generation: generation,
          selecting: false,
          exclusive: false,
          queryable: true,
          unqueryable_reason: None,
          readers: BTreeMap::new (),
        }),
        changed: Condvar::new (),
      }),
    }
  }

  /// Acquire only when EXPECTED is still the selected generation. A caller
  /// which loaded an old immutable runtime while selection was completing gets
  /// `Ok(None)` and retries with the newly published snapshot.
  pub fn acquire_query (
    &self,
    expected : GraphGeneration,
  ) -> Result<Option<QueryLease>, String> {
    let mut state = self . inner . state . lock ()
      . map_err (|_| "generation gate poisoned" . to_string ())?;
    while state . selecting && !state . exclusive {
      state = self . inner . changed . wait (state)
        . map_err (|_| "generation gate poisoned" . to_string ())?; }
    if state . exclusive {
      return Err ("full rebuild is exclusively replacing the selected stores"
        . into ()); }
    if !state . queryable {
      return Err (state . unqueryable_reason . clone ()
        . unwrap_or_else (|| "selected stores are not queryable" . into ())); }
    if state . current_generation != expected { return Ok (None); }
    *state . readers . entry (expected) . or_insert (0) += 1;
    Ok (Some (QueryLease {
      inner: Arc::clone (&self . inner),
      generation: expected,
      released: false,
    }))
  }

  pub fn begin_selection (
    &self,
    expected : GraphGeneration,
    exclusive : bool,
  ) -> Result<SelectionLease, String> {
    let mut state = self . inner . state . lock ()
      . map_err (|_| "generation gate poisoned" . to_string ())?;
    if state . selecting {
      return Err ("another selected-store transition is active" . into ()); }
    if state . current_generation != expected {
      return Err (format! (
        "selected generation advanced from {} to {}",
        expected . get (), state . current_generation . get ())); }
    state . selecting = true;
    state . exclusive = exclusive;
    while state . readers . values () . any (|count| *count > 0) {
      state = self . inner . changed . wait (state)
        . map_err (|_| "generation gate poisoned" . to_string ())?; }
    Ok (SelectionLease {
      inner: Arc::clone (&self . inner),
      expected_generation: expected,
      finished: false,
    })
  }

  pub fn inspect (&self) -> (GraphGeneration, usize, bool, bool) {
    let state = self . inner . state . lock () . unwrap ();
    (state . current_generation,
     state . readers . values () . sum (),
     state . selecting,
     state . exclusive)
  }
}

impl QueryLease {
  pub fn generation (&self) -> GraphGeneration { self . generation }

  pub fn release (mut self) {
    self . release_inner (); }

  fn release_inner (&mut self) {
    if self . released { return; }
    let mut state = self . inner . state . lock () . unwrap ();
    let count = state . readers . get_mut (&self . generation)
      . expect ("query lease generation remains registered");
    *count -= 1;
    if *count == 0 { state . readers . remove (&self . generation); }
    self . released = true;
    self . inner . changed . notify_all ();
  }
}

impl Drop for QueryLease {
  fn drop (&mut self) { self . release_inner (); }
}

impl SelectionLease {
  pub fn publish (
    mut self,
    generation : GraphGeneration,
  ) -> Result<(), String> {
    if generation <= self . expected_generation {
      return Err ("a selected-store publication must advance generation"
        . into ()); }
    let mut state = self . inner . state . lock ()
      . map_err (|_| "generation gate poisoned" . to_string ())?;
    state . current_generation = generation;
    state . selecting = false;
    state . exclusive = false;
    state . queryable = true;
    state . unqueryable_reason = None;
    self . finished = true;
    self . inner . changed . notify_all ();
    Ok (( ))
  }

  pub fn retain_generation (mut self) {
    let mut state = self . inner . state . lock () . unwrap ();
    state . selecting = false;
    state . exclusive = false;
    self . finished = true;
    self . inner . changed . notify_all ();
  }

  pub fn mark_unqueryable (
    mut self,
    reason : String,
  ) {
    let mut state = self . inner . state . lock () . unwrap ();
    state . selecting = false;
    state . exclusive = false;
    state . queryable = false;
    state . unqueryable_reason = Some (reason);
    self . finished = true;
    self . inner . changed . notify_all ();
  }
}

impl Drop for SelectionLease {
  fn drop (&mut self) {
    if self . finished { return; }
    if let Ok (mut state) = self . inner . state . lock () {
      state . selecting = false;
      state . exclusive = false;
      self . inner . changed . notify_all (); }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::sync::mpsc;
  use std::thread;

  #[test]
  fn selection_waits_for_old_query_and_new_query_retries_generation () {
    let gate = GenerationGate::new (GraphGeneration::INITIAL);
    let old_query = gate . acquire_query (GraphGeneration::INITIAL)
      . unwrap () . unwrap ();
    let cloned = gate . clone ();
    let (send, receive) = mpsc::channel ();
    let worker = thread::spawn (move || {
      let selection = cloned . begin_selection (
        GraphGeneration::INITIAL, false) . unwrap ();
      send . send (()) . unwrap ();
      selection . publish (GraphGeneration::INITIAL . successor ()) . unwrap ();
    });
    thread::yield_now ();
    assert! (receive . try_recv () . is_err ());
    drop (old_query);
    receive . recv () . unwrap ();
    worker . join () . unwrap ();
    assert! (gate . acquire_query (GraphGeneration::INITIAL)
      . unwrap () . is_none ());
    assert_eq! (
      gate . acquire_query (GraphGeneration::INITIAL . successor ())
        . unwrap () . unwrap () . generation (),
      GraphGeneration::INITIAL . successor ());
  }

  #[test]
  fn dropped_selection_reopens_the_old_generation () {
    let gate = GenerationGate::new (GraphGeneration::INITIAL);
    drop (gate . begin_selection (GraphGeneration::INITIAL, true) . unwrap ());
    assert! (gate . acquire_query (GraphGeneration::INITIAL)
      . unwrap () . is_some ());
  }
}
