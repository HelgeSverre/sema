use crate::db::Db;
use crate::entity::audit_log;
use sea_orm::{ActiveModelTrait, Set};

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
    let model = audit_log::ActiveModel {
        actor: Set(actor.to_string()),
        action: Set(action.to_string()),
        target_type: Set(target_type.map(String::from)),
        target_name: Set(target_name.map(String::from)),
        detail: Set(detail.map(String::from)),
        ..Default::default()
    };

    if let Err(e) = model.insert(db).await {
        tracing::error!(
            error = %e,
            actor = actor,
            action = action,
            "AUDIT LOG FAILED — action was performed but not recorded"
        );
    }
}
