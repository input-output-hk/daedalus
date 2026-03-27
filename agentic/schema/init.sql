CREATE EXTENSION IF NOT EXISTS pg_search;
CREATE EXTENSION IF NOT EXISTS vector;

CREATE SCHEMA IF NOT EXISTS agentic;

CREATE TABLE IF NOT EXISTS agentic.kb_schema_migrations (
    version BIGINT PRIMARY KEY,
    description TEXT NOT NULL,
    applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    applied_by TEXT NOT NULL DEFAULT CURRENT_USER
);

INSERT INTO agentic.kb_schema_migrations (version, description)
VALUES (1, 'initial kb schema bootstrap')
ON CONFLICT (version) DO NOTHING;
