use crate::types::misc::{ID, SkgConfig, SourceName};

use futures::StreamExt;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use super::util::{
  ConceptRowStream,
  extract_payload_from_typedb_string_rep,
};

pub async fn create_all_sources (
  db_name : &str,
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  for (source_name, source) in &config . sources {
    tx . query ( format! (
      r#"insert $src isa source,
                    has source_name "{}",
                    has user_owns_it {};"#,
      source_name,
      source . user_owns_it ) ) . await ?;
  }
  tx . commit () . await ?;
  Ok (()) }

/// Updates the TypeDB 'has_source' relationship of a node.
/// Deletes the old relation and inserts the new one,
/// in a single transaction. (Using multiple transactions
/// would mean having no source at some point,
/// which the schema forbids.)
pub async fn update_node_source (
  db_name    : &str,
  driver     : &TypeDBDriver,
  pid        : &ID,
  new_source : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  tx . query ( format! (
    r#"match
         $n isa node, has id "{}";
         $old_rel isa has_source (node: $n, source: $old_src);
         $new_src isa source, has source_name "{}";
       delete
         $old_rel;
       insert
         $new_rel isa has_source (node: $n, source: $new_src);"#,
    pid . as_str (),
    new_source ) ) . await ?;
  tx . commit () . await ?;
  Ok (()) }

pub async fn source_ownership_for_node_ids (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &[ID],
) -> Result<HashMap<ID, bool>, Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read ) . await ?;
  let mut result : HashMap<ID, bool> = HashMap::new ();
  for id in ids {
    if let Some (owned) =
      source_ownership_for_one_node_id_in_tx (&tx, id) . await ?
    {
      result . insert (id . clone (), owned);
    }
  }
  tx . close () . await ?;
  Ok (result) }

async fn source_ownership_for_one_node_id_in_tx (
  tx : &Transaction,
  id : &ID,
) -> Result<Option<bool>, Box<dyn Error>> {
  let answer : QueryAnswer =
    tx . query ( format! (
      r#"match
           $n isa node, has id "{}";
           $src isa source, has user_owns_it $owned;
           $rel isa has_source (node: $n, source: $src);
         select $owned;"#,
      id . as_str () ) ) . await ?;
  let mut rows : ConceptRowStream = answer . into_rows ();
  match rows . next () . await {
    Some (Ok (row)) => {
      if let Some (concept) = row . get ("owned") ? {
        Ok (Some (concept . to_string () . contains ("true")))
      } else {
        Ok (None)
      } }
    Some (Err (e)) => Err (e . into ()),
    None => Ok (None),
  } }

pub async fn source_name_for_node_ids (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &[ID],
) -> Result<HashMap<ID, SourceName>, Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read ) . await ?;
  let mut result : HashMap<ID, SourceName> = HashMap::new ();
  for id in ids {
    if let Some (source_name) =
      source_name_for_one_node_id_in_tx (&tx, id) . await ?
    {
      result . insert (id . clone (), source_name);
    }
  }
  tx . close () . await ?;
  Ok (result) }

pub(crate) async fn source_name_for_one_node_id_in_tx (
  tx : &Transaction,
  id : &ID,
) -> Result<Option<SourceName>, Box<dyn Error>> {
  let answer : QueryAnswer =
    tx . query ( format! (
      r#"match
           $n isa node, has id "{}";
           $src isa source, has source_name $source_name;
           $rel isa has_source (node: $n, source: $src);
         select $source_name;"#,
      id . as_str () ) ) . await ?;
  let mut rows : ConceptRowStream = answer . into_rows ();
  match rows . next () . await {
    Some (Ok (row)) => {
      if let Some (concept) = row . get ("source_name") ? {
        Ok (Some (SourceName::from (
          extract_payload_from_typedb_string_rep (
            &concept . to_string () ) )))
      } else {
        Ok (None)
      } }
    Some (Err (e)) => Err (e . into ()),
    None => Ok (None),
  } }

pub async fn node_source_relation_count (
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result<HashMap<ID, usize>, Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read ) . await ?;
  let answer : QueryAnswer =
    tx . query (
      r#"match
           $n isa node, has id $id;
           $rel isa has_source (node: $n, source: $src);
         select $id;"# ) . await ?;
  let mut rows : ConceptRowStream = answer . into_rows ();
  let mut counts : HashMap<ID, usize> = HashMap::new ();
  while let Some (row_result) = rows . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get ("id") ? {
      let id : ID =
        ID::from (extract_payload_from_typedb_string_rep (
          &concept . to_string () ));
      *counts . entry (id) . or_insert (0) += 1;
    }
  }
  tx . close () . await ?;
  Ok (counts) }
