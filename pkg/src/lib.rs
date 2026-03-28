pub mod api;
pub mod audit;
pub mod auth;
pub mod blob;
pub mod config;
pub mod crypto;
pub mod db;
pub mod github;
pub mod github_sync;
pub mod web;

use axum::{
    routing::{delete, get, post, put},
    Router,
};
use std::sync::Arc;
use tower_http::services::ServeDir;

pub struct AppState {
    pub db: db::Db,
    pub config: config::Config,
}

pub fn build_router(state: Arc<AppState>) -> Router {
    Router::new()
        .route("/healthz", get(|| async { "ok" }))
        // Web pages
        .route("/", get(web::index))
        .route("/search", get(web::search))
        .route("/packages/{name}", get(web::package_detail))
        .route("/login", get(web::login))
        .route("/account", get(web::account))
        .route("/link", get(web::link_page))
        .route("/admin", get(web::admin_page))
        // GitHub OAuth
        .route("/auth/github", get(github::start))
        .route("/auth/github/callback", get(github::callback))
        // Static files
        .nest_service("/static", ServeDir::new("static"))
        // Auth API
        .route("/api/v1/auth/register", post(api::auth::register))
        .route("/api/v1/auth/login", post(api::auth::login))
        .route("/api/v1/auth/logout", post(api::auth::logout))
        // Tokens API
        .route(
            "/api/v1/tokens",
            post(api::tokens::create).get(api::tokens::list),
        )
        .route("/api/v1/tokens/{id}", delete(api::tokens::revoke))
        // Packages API
        .route("/api/v1/packages/{name}", get(api::packages::get_package))
        .route(
            "/api/v1/packages/{name}/downloads",
            get(api::packages::download_stats),
        )
        .route(
            "/api/v1/packages/{name}/{version}",
            put(api::packages::publish),
        )
        .route(
            "/api/v1/packages/{name}/{version}/download",
            get(api::packages::download),
        )
        .route(
            "/api/v1/packages/{name}/{version}/yank",
            post(api::packages::yank),
        )
        .route(
            "/api/v1/packages/{name}/owners",
            get(api::packages::list_owners)
                .put(api::packages::add_owner)
                .delete(api::packages::remove_owner),
        )
        .route("/api/v1/search", get(api::packages::search))
        // GitHub-linked packages API
        .route("/api/v1/packages/link", post(api::github::link))
        .route(
            "/api/v1/packages/{name}/sync",
            post(api::github::sync),
        )
        .route(
            "/api/v1/webhooks/github",
            post(api::github::webhook),
        )
        // Admin API
        .route("/api/v1/admin/stats", get(api::admin::stats))
        .route("/api/v1/admin/users", get(api::admin::list_users))
        .route("/api/v1/admin/users/{id}", get(api::admin::get_user))
        .route("/api/v1/admin/users/{id}/ban", post(api::admin::ban_user))
        .route(
            "/api/v1/admin/users/{id}/unban",
            post(api::admin::unban_user),
        )
        .route(
            "/api/v1/admin/users/{id}/revoke-tokens",
            post(api::admin::revoke_user_tokens),
        )
        .route(
            "/api/v1/admin/users/{id}/role",
            put(api::admin::set_user_role),
        )
        .route(
            "/api/v1/admin/packages",
            get(api::admin::list_packages),
        )
        .route(
            "/api/v1/admin/packages/{name}",
            get(api::admin::get_package).delete(api::admin::remove_package),
        )
        .route(
            "/api/v1/admin/packages/{name}/yank-all",
            post(api::admin::yank_all_versions),
        )
        .route(
            "/api/v1/admin/packages/{name}/transfer",
            post(api::admin::transfer_ownership),
        )
        .route("/api/v1/admin/audit", get(api::admin::list_audit))
        .route("/api/v1/admin/reports", get(api::admin::list_reports))
        .route(
            "/api/v1/admin/reports/{id}/action",
            post(api::admin::action_report),
        )
        .route(
            "/api/v1/admin/reports/{id}/dismiss",
            post(api::admin::dismiss_report),
        )
        // Report submission (any authenticated user)
        .route("/api/v1/reports", post(api::admin::submit_report))
        .with_state(state)
}
