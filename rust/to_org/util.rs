pub fn newline_to_space ( s: &str ) -> String {
  s.replace ( '\n', " " ) }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
