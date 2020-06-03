-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 2 THEN
    -- Persistent does not support more precision than Int64, and outsum needs
    -- to support Word128. We have defined outsum as a Word128 and the easiest
    -- way to store than in the database as a readable value is using show/read
    -- in the database as a bytea.
    EXECUTE 'CREATE DOMAIN outsum AS text;';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
