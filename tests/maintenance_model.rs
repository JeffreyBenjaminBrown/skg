//! Executable specification of publication, admission and durable ordering.
//! Filesystem and editor adapters must additionally test the real effects.

use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Base {
  session : u64,
  graph   : u64,
  manifest : u64, }

#[derive(Clone, Debug, Eq, PartialEq)]
struct Buffer {
  revision : u64,
  restricted_by : BTreeSet<u64>, }

#[derive(Clone, Debug, Eq, PartialEq)]
struct Transition {
  operation : u64,
  base : Base,
  census : Option<BTreeMap<u64, u64>>,
  archived : bool,
  effects_authorized : bool,
  files_applied : bool,
  index_ready : bool, }

#[derive(Clone, Debug, Eq, PartialEq)]
struct Report {
  census : BTreeMap<u64, u64>,
  pending_buffers : BTreeSet<u64>, }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Outcome { Selected (Base), Cancelled, Failed }

#[derive(Clone, Debug, Eq, PartialEq)]
struct Model {
  selected : Base,
  durable_selection : Base,
  journal_revision : u64,
  constructors : BTreeSet<u64>,
  buffers : BTreeMap<u64, Buffer>,
  transition : Option<Transition>,
  reports : BTreeMap<u64, Report>,
  outcomes : BTreeMap<u64, Outcome>,
  waits : BTreeMap<u64, u64>,
  retired_operations : BTreeSet<u64>, }

impl Model {
  fn new () -> Self {
    let selected : Base = Base { session: 1, graph: 1, manifest: 1 };
    Self {
      selected, durable_selection: selected, journal_revision: 0,
      constructors: BTreeSet::new (), buffers: BTreeMap::new (),
      transition: None, reports: BTreeMap::new (),
      outcomes: BTreeMap::new (), waits: BTreeMap::new (),
      retired_operations: BTreeSet::new (), } }

  fn construct (
    &mut self,
    buffer : u64,
  ) -> bool {
    if self . transition . is_some () { return false; }
    self . constructors . insert (buffer);
    true }

  fn finish_constructor (
    &mut self,
    buffer : u64,
  ) {
    if self . constructors . remove (&buffer) {
      self . buffers . insert (buffer, Buffer {
        revision: 1, restricted_by: BTreeSet::new () }); } }

  fn begin (
    &mut self,
    operation : u64,
  ) -> bool {
    if self . transition . is_some ()
    || self . outcomes . contains_key (&operation)
    || self . retired_operations . contains (&operation)
    { return false; }
    self . transition = Some (Transition {
      operation, base: self . selected, census: None,
      archived: false, effects_authorized: false,
      files_applied: false, index_ready: false });
    self . journal_revision += 1;
    true }

  fn freeze_census (&mut self) -> bool {
    if !self . constructors . is_empty () { return false; }
    let transition : &mut Transition = self . transition . as_mut () . unwrap ();
    if transition . census . is_some () { return false; }
    transition . census = Some (self . buffers . iter_mut ()
      . map (|(id, buffer)| {
        buffer . restricted_by . insert (transition . operation);
        (*id, buffer . revision) }) . collect ());
    self . journal_revision += 1;
    true }

  fn archive (&mut self) -> bool {
    let transition : &mut Transition = self . transition . as_mut () . unwrap ();
    if transition . census . is_none () { return false; }
    transition . archived = true;
    self . journal_revision += 1;
    true }

  fn authorize_effects (&mut self) -> bool {
    let transition : &mut Transition = self . transition . as_mut () . unwrap ();
    if !transition . archived { return false; }
    transition . effects_authorized = true;
    self . journal_revision += 1;
    true }

  fn complete_worker (
    &mut self,
    operation : u64,
    base : Base,
    index : bool,
  ) -> bool {
    let Some (transition) : Option<&mut Transition> =
      self . transition . as_mut () else { return false; };
    if transition . operation != operation || transition . base != base
    || !transition . effects_authorized
    { return false; }
    if index { transition . index_ready = true; }
    else { transition . files_applied = true; }
    true }

  fn commit (
    &mut self,
    changed : bool,
    crash_before_publication : bool,
  ) -> bool {
    let transition : &Transition = self . transition . as_ref () . unwrap ();
    if !transition . files_applied || !transition . index_ready { return false; }
    let result : Base = Base {
      session: self . selected . session,
      graph: self . selected . graph + u64::from (changed),
      manifest: self . selected . manifest + u64::from (changed), };
    self . durable_selection = result;
    self . outcomes . insert (transition . operation, Outcome::Selected (result));
    self . journal_revision += 1;
    if !crash_before_publication { self . publish_committed (); }
    true }

  fn publish_committed (&mut self) {
    let transition : Transition = self . transition . take () . unwrap ();
    let census : BTreeMap<u64, u64> = transition . census . unwrap ();
    self . selected = self . durable_selection;
    self . reports . insert (transition . operation, Report {
      pending_buffers: census . keys () . copied () . collect (), census }); }

  fn settle (
    &mut self,
    operation : u64,
    buffer : u64,
    revision : u64,
  ) -> bool {
    let Some (report) : Option<&mut Report> =
      self . reports . get_mut (&operation) else { return false; };
    if report . census . get (&buffer) != Some (&revision) { return false; }
    if !report . pending_buffers . contains (&buffer) { return true; }
    let current : &mut Buffer = self . buffers . get_mut (&buffer) . unwrap ();
    if current . revision != revision { return false; }
    current . restricted_by . remove (&operation);
    report . pending_buffers . remove (&buffer);
    self . journal_revision += 1;
    true }

  fn save (
    &mut self,
    operation : u64,
    buffer : u64,
  ) -> bool {
    if self . transition . is_some ()
    || self . retired_operations . contains (&operation)
    || self . outcomes . contains_key (&operation)
    { return false; }
    let current : &mut Buffer = self . buffers . get_mut (&buffer) . unwrap ();
    if !current . restricted_by . is_empty () { return false; }
    current . revision += 1;
    self . selected . graph += 1;
    self . selected . manifest += 1;
    self . durable_selection = self . selected;
    self . outcomes . insert (operation, Outcome::Selected (self . selected));
    self . journal_revision += 1;
    true }

  fn finish_report (
    &mut self,
    operation : u64,
  ) -> bool {
    if self . reports . get (&operation)
      . is_some_and (|report| !report . pending_buffers . is_empty ())
    { return false; }
    self . reports . remove (&operation);
    self . retired_operations . insert (operation);
    self . outcomes . remove (&operation);
    self . journal_revision += 1;
    true }

  fn wait_outcome (
    &self,
    wait : u64,
  ) -> Option<Outcome> {
    self . waits . get (&wait)
      . and_then (|operation| self . outcomes . get (operation)) . copied () }

  fn restart (&mut self) {
    if let Some (transition) = &self . transition {
      if self . outcomes . contains_key (&transition . operation) {
        self . publish_committed (); } }
    self . selected . session += 1;
    self . durable_selection . session = self . selected . session;
    self . constructors . clear ();
    self . buffers . clear (); }
}

fn prepared () -> Model {
  let mut model : Model = Model::new ();
  for buffer in [1, 2] {
    assert! (model . construct (buffer));
    model . finish_constructor (buffer); }
  assert! (model . begin (10));
  assert! (model . freeze_census ());
  assert! (model . archive ());
  assert! (model . authorize_effects ());
  model }

fn ready (
  model : &mut Model,
  changed : bool,
) {
  let base : Base = model . selected;
  assert! (model . complete_worker (10, base, false));
  assert! (model . complete_worker (10, base, true));
  assert! (model . commit (changed, false)); }

#[test]
fn fixed_census_waits_for_admitted_constructor_and_excludes_late_query () {
  let mut model : Model = Model::new ();
  assert! (model . construct (1));
  assert! (model . begin (10));
  assert! (!model . freeze_census ());
  assert! (!model . archive ());
  assert! (!model . authorize_effects ());
  assert! (!model . construct (2));
  model . finish_constructor (1);
  model . finish_constructor (2);
  assert! (model . freeze_census ());
  assert_eq! (model . transition . as_ref () . unwrap () . census,
    Some (BTreeMap::from ([(1, 1)])));
  assert! (!model . freeze_census ()); }

#[test]
fn obsolete_workers_and_incomplete_pairs_cannot_publish_in_either_order () {
  for index_first in [false, true] {
    let mut model : Model = prepared ();
    let base : Base = model . selected;
    let before : Model = model . clone ();
    assert! (!model . complete_worker (9, base, index_first));
    assert! (!model . complete_worker (10, Base { graph: 0, ..base }, index_first));
    assert_eq! (model, before);
    assert! (model . complete_worker (10, base, index_first));
    assert! (!model . commit (true, false));
    assert_eq! (model . selected, base);
    assert! (!model . save (11, 1));
    assert! (!model . begin (12));
    assert! (model . complete_worker (10, base, !index_first));
    assert! (model . commit (true, false));
    assert_eq! (model . selected . graph, 2); } }

#[test]
fn independent_release_new_save_and_late_ack_leave_old_report_harmless () {
  for first in [1, 2] {
    let other : u64 = 3 - first;
    let mut model : Model = prepared ();
    ready (&mut model, true);
    assert! (model . settle (10, first, 1));
    assert! (model . save (11, first));
    assert! (!model . save (12, other));
    assert! (model . construct (3));
    model . finish_constructor (3);
    assert! (model . save (13, 3));
    let selected : Base = model . selected;
    assert! (model . settle (10, first, 1));
    assert_eq! (model . buffers[&first] . revision, 2);
    assert! (model . settle (10, other, 1));
    assert! (model . finish_report (10));
    assert_eq! (model . selected, selected);
    assert! (!model . begin (10));
    assert! (!model . save (11, first)); } }

#[test]
fn old_incident_ack_cannot_remove_successor_restriction () {
  let mut model : Model = prepared ();
  ready (&mut model, true);
  assert! (model . settle (10, 1, 1));
  assert! (model . begin (20));
  assert! (model . freeze_census ());
  assert! (model . settle (10, 1, 1));
  assert_eq! (model . buffers[&1] . restricted_by, BTreeSet::from ([20])); }

#[test]
fn crash_retains_unresolved_effects_and_recovers_durable_publication () {
  let mut model : Model = prepared ();
  let base : Base = model . selected;
  assert! (model . complete_worker (10, base, false));
  model . restart ();
  assert! (model . transition . as_ref () . unwrap () . effects_authorized);
  assert! (!model . begin (20));
  assert! (model . complete_worker (10, base, true));
  assert! (model . commit (true, true));
  assert_eq! (model . selected . graph, 1);
  model . restart ();
  assert_eq! (model . selected . graph, 2);
  assert_ne! (model . selected . session, base . session);
  assert! (model . transition . is_none ());
  assert! (model . reports . contains_key (&10));
  assert! (model . buffers . is_empty ());
  assert! (!model . begin (10)); }

#[test]
fn no_change_wait_is_about_its_operation_and_has_no_clock () {
  let mut model : Model = prepared ();
  model . waits . insert (100, 10);
  model . waits . insert (200, 20);
  ready (&mut model, false);
  assert_eq! (model . wait_outcome (100), Some (Outcome::Selected (model . selected)));
  assert! (model . settle (10, 1, 1));
  assert! (model . save (11, 1));
  assert_eq! (model . wait_outcome (200), None);
  model . restart ();
  assert_eq! (model . wait_outcome (200), None);
  model . outcomes . insert (20, Outcome::Cancelled);
  assert_eq! (model . wait_outcome (200), Some (Outcome::Cancelled));
  model . waits . insert (300, 30);
  model . outcomes . insert (30, Outcome::Failed);
  assert_eq! (model . wait_outcome (300), Some (Outcome::Failed)); }
