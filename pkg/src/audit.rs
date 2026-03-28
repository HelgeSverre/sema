use crate::db::Db;

/// Log an action to the audit trail.
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
) -> Result<(), sqlx::Error> {
    sqlx::query(
        "INSERT INTO audit_log (actor, action, target_type, target_name, detail) VALUES (?, ?, ?, ?, ?)",
    )
    .bind(actor)
    .bind(action)
    .bind(target_type)
    .bind(target_name)
    .bind(detail)
    .execute(db)
    .await?;
    Ok(())
}
