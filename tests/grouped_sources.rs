// Binary grouping (TODO/faster-tests.org): multi-source, source-set,
// and source/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "diff_mode_refusals.rs"]
mod diff_mode_refusals;

#[path = "leak_battery.rs"]
mod leak_battery;

#[path = "move_source.rs"]
mod move_source;


#[path = "source_sets.rs"]
mod source_sets;
