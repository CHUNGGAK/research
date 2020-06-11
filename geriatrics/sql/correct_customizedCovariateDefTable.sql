CREATE TABLE drug_era_po_def_2 AS
SELECT attribute_definition_id,
    'drug_era only per oral during day -365 through 0 days relative to index: ' || attribute_name attribute_name
FROM drug_era_po_def;

DROP TABLE drug_era_po_def;

ALTER TABLE drug_era_po_def_2 RENAME TO drug_era_po_def;

SELECT * FROM drug_era_po_def;