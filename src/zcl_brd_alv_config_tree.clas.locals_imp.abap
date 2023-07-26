*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
DEFINE _set_timestamp_create.
  &1-ernam = sy-uname.
  &1-erdat = sy-datum.
  &1-erzet = sy-uzeit.
  &1-aenam = sy-uname.
  &1-aedat = sy-datum.
  &1-aezet = sy-uzeit.
end-OF-DEFINITION.
DEFINE _set_timestamp_change.
  &1-aenam = sy-uname.
  &1-aedat = sy-datum.
  &1-aezet = sy-uzeit.
end-OF-DEFINITION.
