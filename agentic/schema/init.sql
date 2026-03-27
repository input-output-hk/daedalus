CREATE EXTENSION IF NOT EXISTS pg_search;
CREATE EXTENSION IF NOT EXISTS vector;

CREATE SCHEMA IF NOT EXISTS agentic;

CREATE TABLE IF NOT EXISTS agentic.kb_schema_migrations (
    version BIGINT PRIMARY KEY,
    description TEXT NOT NULL,
    applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    applied_by TEXT NOT NULL DEFAULT CURRENT_USER
);

CREATE TABLE IF NOT EXISTS agentic.kb_documents (
    id TEXT PRIMARY KEY,
    source_domain TEXT NOT NULL,
    doc_kind TEXT NOT NULL,
    source_path TEXT NOT NULL,
    title TEXT NOT NULL,
    section_title TEXT,
    subsection_title TEXT,
    heading_path JSONB NOT NULL DEFAULT '[]'::jsonb,
    chunk_index INTEGER NOT NULL,
    content TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    content_hash TEXT NOT NULL,
    repo_commit_hash TEXT,
    source_updated_at TIMESTAMPTZ,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_documents_source_path_chunk_index_key UNIQUE (source_path, chunk_index)
);

CREATE TABLE IF NOT EXISTS agentic.kb_code_chunks (
    id TEXT PRIMARY KEY,
    repo_path TEXT NOT NULL,
    language TEXT NOT NULL,
    symbol_name TEXT,
    symbol_kind TEXT,
    parent_symbol_name TEXT,
    parent_symbol_kind TEXT,
    chunk_index INTEGER NOT NULL,
    start_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    content TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    content_hash TEXT NOT NULL,
    repo_commit_hash TEXT,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_code_chunks_repo_path_chunk_index_key UNIQUE (repo_path, chunk_index)
);

CREATE TABLE IF NOT EXISTS agentic.kb_github_issues (
    id TEXT PRIMARY KEY,
    repo TEXT NOT NULL,
    issue_number INTEGER NOT NULL,
    github_node_id TEXT,
    title TEXT NOT NULL,
    state TEXT NOT NULL,
    author_login TEXT,
    labels JSONB NOT NULL DEFAULT '[]'::jsonb,
    body_text TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    html_url TEXT NOT NULL,
    source_created_at TIMESTAMPTZ NOT NULL,
    source_updated_at TIMESTAMPTZ NOT NULL,
    source_closed_at TIMESTAMPTZ,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_github_issues_repo_issue_number_key UNIQUE (repo, issue_number)
);

CREATE TABLE IF NOT EXISTS agentic.kb_github_issue_comments (
    id TEXT PRIMARY KEY,
    issue_id TEXT NOT NULL,
    repo TEXT NOT NULL,
    issue_number INTEGER NOT NULL,
    github_comment_id BIGINT NOT NULL,
    github_node_id TEXT,
    author_login TEXT,
    body_text TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    html_url TEXT NOT NULL,
    source_created_at TIMESTAMPTZ NOT NULL,
    source_updated_at TIMESTAMPTZ NOT NULL,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_github_issue_comments_issue_id_fkey FOREIGN KEY (issue_id)
        REFERENCES agentic.kb_github_issues (id),
    CONSTRAINT kb_github_issue_comments_repo_github_comment_id_key UNIQUE (repo, github_comment_id)
);

CREATE TABLE IF NOT EXISTS agentic.kb_github_prs (
    id TEXT PRIMARY KEY,
    repo TEXT NOT NULL,
    pr_number INTEGER NOT NULL,
    github_node_id TEXT,
    title TEXT NOT NULL,
    state TEXT NOT NULL,
    author_login TEXT,
    base_branch TEXT,
    head_branch TEXT,
    labels JSONB NOT NULL DEFAULT '[]'::jsonb,
    body_text TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    html_url TEXT NOT NULL,
    source_created_at TIMESTAMPTZ NOT NULL,
    source_updated_at TIMESTAMPTZ NOT NULL,
    source_closed_at TIMESTAMPTZ,
    source_merged_at TIMESTAMPTZ,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_github_prs_repo_pr_number_key UNIQUE (repo, pr_number)
);

CREATE TABLE IF NOT EXISTS agentic.kb_github_pr_comments (
    id TEXT PRIMARY KEY,
    pr_id TEXT NOT NULL,
    repo TEXT NOT NULL,
    pr_number INTEGER NOT NULL,
    comment_type TEXT NOT NULL,
    github_comment_id BIGINT NOT NULL,
    github_node_id TEXT,
    author_login TEXT,
    repo_path TEXT,
    commit_oid TEXT,
    original_commit_oid TEXT,
    diff_hunk TEXT,
    line INTEGER,
    original_line INTEGER,
    side TEXT,
    start_line INTEGER,
    start_side TEXT,
    body_text TEXT NOT NULL,
    preview_text TEXT NOT NULL,
    html_url TEXT NOT NULL,
    source_created_at TIMESTAMPTZ NOT NULL,
    source_updated_at TIMESTAMPTZ NOT NULL,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_github_pr_comments_pr_id_fkey FOREIGN KEY (pr_id)
        REFERENCES agentic.kb_github_prs (id),
    CONSTRAINT kb_github_pr_comments_repo_comment_type_comment_id_key
        UNIQUE (repo, comment_type, github_comment_id),
    CONSTRAINT kb_github_pr_comments_comment_type_check
        CHECK (comment_type IN ('issue_comment', 'review_comment'))
);

CREATE TABLE IF NOT EXISTS agentic.kb_project_items (
    id TEXT PRIMARY KEY,
    project_owner TEXT NOT NULL,
    project_number INTEGER NOT NULL,
    project_item_node_id TEXT NOT NULL,
    content_type TEXT,
    content_id TEXT,
    content_node_id TEXT,
    title TEXT NOT NULL,
    body_text TEXT NOT NULL,
    repo TEXT,
    status TEXT,
    priority TEXT,
    size TEXT,
    work_type TEXT,
    area TEXT,
    phase TEXT,
    kb_impact TEXT,
    start_date DATE,
    target_date DATE,
    field_values JSONB NOT NULL DEFAULT '{}'::jsonb,
    html_url TEXT,
    source_updated_at TIMESTAMPTZ NOT NULL,
    embedding VECTOR(384),
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_project_items_project_item_node_id_key UNIQUE (project_item_node_id)
);

CREATE TABLE IF NOT EXISTS agentic.kb_sync_state (
    id TEXT PRIMARY KEY,
    source_name TEXT NOT NULL,
    scope_key TEXT NOT NULL,
    repo_commit_hash TEXT,
    cursor_text TEXT,
    watermark_text TEXT,
    watermark_timestamp TIMESTAMPTZ,
    schema_version BIGINT,
    last_attempted_at TIMESTAMPTZ,
    last_succeeded_at TIMESTAMPTZ,
    last_error TEXT,
    metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_sync_state_source_name_scope_key_key UNIQUE (source_name, scope_key)
);

CREATE TABLE IF NOT EXISTS agentic.kb_snapshot_manifest (
    id TEXT PRIMARY KEY,
    snapshot_name TEXT NOT NULL,
    schema_version BIGINT NOT NULL,
    snapshot_created_at TIMESTAMPTZ NOT NULL,
    repo_commit_hash TEXT,
    embedding_model TEXT,
    entity_counts JSONB NOT NULL DEFAULT '{}'::jsonb,
    github_watermarks JSONB NOT NULL DEFAULT '{}'::jsonb,
    manifest JSONB NOT NULL DEFAULT '{}'::jsonb,
    source_path TEXT,
    content_hash TEXT,
    imported_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT kb_snapshot_manifest_snapshot_name_snapshot_created_at_key
        UNIQUE (snapshot_name, snapshot_created_at)
);

INSERT INTO agentic.kb_schema_migrations (version, description)
VALUES (1, 'initial kb schema bootstrap')
ON CONFLICT (version) DO NOTHING;

INSERT INTO agentic.kb_schema_migrations (version, description)
VALUES (2, 'create core knowledge tables')
ON CONFLICT (version) DO NOTHING;
