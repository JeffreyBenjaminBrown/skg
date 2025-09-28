use skg::types::{MetadataItem, RelToOrgParent, ID, find_in_metadata_collection, find_id_in_metadata_collection};

#[test]
fn test_find_in_metadata_collection() {
  let metadata = vec![
    MetadataItem::Folded,
    MetadataItem::ID("test123".to_string()),
    MetadataItem::Cycle,
    MetadataItem::Type(RelToOrgParent::Container),
  ];

  // Test finding ID
  let id: Option<ID> = find_id_in_metadata_collection(&metadata);
  assert_eq!(id, Some(ID("test123".to_string())));

  // Test generic function with different accessors
  let has_cycle: Option<()> = find_in_metadata_collection(
    &metadata,
    |item| { if item.is_cycle() { Some(()) } else { None }} );
  assert_eq!(has_cycle, Some(( )) );

  let type_val: Option<RelToOrgParent> = find_in_metadata_collection(
    &metadata,
    |item| { item.get_type().cloned() } );
  assert_eq!(type_val, Some(RelToOrgParent::Container));

  // Test with empty collection
  let empty: Vec<MetadataItem> = vec![];
  let no_id: Option<ID> = find_id_in_metadata_collection(&empty);
  assert_eq!(no_id, None);
}
