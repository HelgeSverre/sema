use crate::db::Db;

/// Log an action to the audit trail. On failure, emits a tracing::error
/// so audit failures are always observable even if the caller continues.
///
/// - `actor`: username or "system"
/// - `action`: verb (e.g. "publish", "ban", "yank", "register")
/// - `target_type`: optional — "user", "package", or "version"
/// - `target_name`: optional — the username or package name affected
/// - `detail`: optional — free-text description, reason, version string, etc.
pub async fn log(
    db: &Db,
    actor: &str,
    action: &str,
    target_type: Option<&str>,
    target_name: Option<&str>,
    detail: Option<&str>,
) {
    let result = sqlx::query(
        "INSERT INTO audit_log (actor, action, target_type, target_name, detail) VALUES (?, ?, ?, ?, ?)",
    )
    .bind(actor)
    .bind(action)
    .bind(target_type)
    .bind(target_name)
    .bind(detail)
    .execute(db)
    .await;

    if let Err(e) = result {
        tracing::error!(
            error = %e,
            actor = actor,
            action = action,
            "AUDIT LOG FAILED — action was performed but not recorded"
        );
    }
}
