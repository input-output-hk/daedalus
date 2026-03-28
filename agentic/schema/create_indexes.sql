CREATE INDEX IF NOT EXISTS kb_documents_bm25_idx
ON agentic.kb_documents
USING bm25 (id, source_domain, doc_kind, source_path, title, preview_text, content)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_documents_embedding_hnsw_idx
ON agentic.kb_documents
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_code_chunks_bm25_idx
ON agentic.kb_code_chunks
USING bm25 (id, repo_path, language, symbol_name, symbol_kind, preview_text, content)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_code_chunks_embedding_hnsw_idx
ON agentic.kb_code_chunks
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_github_issues_bm25_idx
ON agentic.kb_github_issues
USING bm25 (id, repo, issue_number, state, title, preview_text, body_text)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_github_issues_embedding_hnsw_idx
ON agentic.kb_github_issues
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_github_issue_comments_bm25_idx
ON agentic.kb_github_issue_comments
USING bm25 (id, repo, issue_number, preview_text, body_text)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_github_issue_comments_embedding_hnsw_idx
ON agentic.kb_github_issue_comments
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_github_prs_bm25_idx
ON agentic.kb_github_prs
USING bm25 (id, repo, pr_number, state, title, preview_text, body_text)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_github_prs_embedding_hnsw_idx
ON agentic.kb_github_prs
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_github_pr_comments_bm25_idx
ON agentic.kb_github_pr_comments
USING bm25 (id, repo, pr_number, comment_type, repo_path, preview_text, body_text)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_github_pr_comments_embedding_hnsw_idx
ON agentic.kb_github_pr_comments
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

CREATE INDEX IF NOT EXISTS kb_project_items_bm25_idx
ON agentic.kb_project_items
USING bm25 (
    id,
    project_owner,
    project_number,
    content_type,
    repo,
    status,
    priority,
    work_type,
    area,
    phase,
    kb_impact,
    title,
    body_text
)
WITH (key_field = 'id');

CREATE INDEX IF NOT EXISTS kb_project_items_embedding_hnsw_idx
ON agentic.kb_project_items
USING hnsw (embedding vector_cosine_ops)
WHERE embedding IS NOT NULL;

INSERT INTO agentic.kb_schema_migrations (version, description)
VALUES (3, 'create bm25 and hnsw indexes')
ON CONFLICT (version) DO NOTHING;
